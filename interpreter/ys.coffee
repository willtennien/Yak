parseError = (token, message) ->
    throw new SyntaxError "#{message} at #{token.file}:#{token.line}:#{token.character}"

last = (thing) -> thing[thing.length - 1]

tokenizer = do ->
    symbol =
        '(': 'group start'
        ')': 'group end'
        '{': 'funject start'
        '}': 'funject end'
        '[': 'list start'
        ',': 'list delimiter'
        ']': 'list end'
        ':': 'pattern match'
        '=': 'strict assignment'
        ':=': 'lazy assignment'
        '|=': 'reset strict assignment'
        '|:=': 'reset lazy assignment'
        '<-': 'inverse assignment'
        '<<': 'inheritance assignment'
        '::': 'prototypal application'

    isSpace = (c) -> c is ' ' or c is '\t'

    isDigit = (c) -> -1 isnt '0123456789'.indexOf c

    isIdentifier = (c) -> -1 isnt '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-+*/%^_$<>=?!'.indexOf c

    (raw, file = '<anonymous>', startLine = 1) ->
        token = (type, value = '') -> {
            type, value, line, file
            character: character }

        syntaxError = (message) ->
            throw new SyntaxError "#{message} at #{file}:#{line}:#{character}"

        s = ''
        i = 0
        loop
            j = raw.indexOf '#', i
            break if j is -1
            s += raw.substring i, j
            i = j + 1
            c = raw[i]
            if c is '|'
                ++i
                start = i
                pairs = 1
                while pairs
                    a = raw.indexOf '#|', i
                    b = raw.indexOf '|#', i
                    if a is -1 and b is -1
                        throw new SyntaxError "Unmatched multiline comment"
                    if a isnt -1 and (a < b or b is -1)
                        ++pairs
                        i = a + 2
                    if b isnt -1 and (b < a or a is -1)
                        --pairs
                        i = b + 2
                s += Array(raw.substring(start, i).split('\n').length).join('\n')
            else
                j = raw.indexOf '\n', i
                break if j is -1
                i = j

        s += raw.substring i

        i = 0
        character = 0
        lastNewline = 0
        line = startLine
        length = s.length
        indent = ['']
        tokens = []
        advance = ->
            if tokens.length
                return tokens.shift()
            c = s[i]
            while isSpace c
                ++i
                c = s[i]
            character = 1 + i - lastNewline
            if i >= length
                if indent.length > 1
                    indent.pop()
                    return token 'outdent'
                return token 'end'
            if (q = s.substr i, 3) is '|:=' or (q = s.substr i, 2) is ':=' or q is '|=' or q is '<-' or q is '<<' or q is '::'
                i += q.length
                return token symbol[q], q
            switch c
                when '\n', '\r'
                    loop
                        while c is '\n' or c is '\r'
                            ++i
                            ++line
                            lastNewline = i
                            character = 1 + i - lastNewline
                            c = s[i]
                        space = ''
                        loop
                            c = s[i]
                            break unless isSpace c
                            space += c
                            ++i
                        break unless c is '\n' or c is '\r'
                    l = last indent
                    now = space.length
                    prev = l.length
                    switch
                        when now is prev
                            if space isnt l
                                syntaxError 'Inconsistent indentation'
                            return token 'newline'
                        when now < prev
                            while now < prev
                                tokens.push token 'outdent'
                                indent.pop()
                                l = last indent
                                prev = l.length
                            if c and space isnt l
                                syntaxError 'Inconsistent indentation'
                            return tokens.shift()
                        when now > prev
                            if l isnt space.substr 0, prev
                                syntaxError 'Inconsistent indentation'
                            indent.push space
                            return token 'indent'
                when '{', '}', '[', ']', ':', '=', ',', '(', ')'
                    ++i
                    if c is '=' and s[i] is '='
                        --i
                    else
                        return token symbol[c], c

            if c is '-'
                q = s[i + 1]
                if q is '.' or isDigit q
                    digit = true
            if digit or c is '.' or isDigit c
                value = ''
                start = i
                if c is '-'
                    value += c
                    ++i
                    c = s[i]
                if isDigit c
                    loop
                        value += c
                        ++i
                        c = s[i]
                        break unless isDigit c
                b = false
                if c is '.'
                    value += c
                    ++i
                    c = s[i]
                    if not isDigit c
                        --i
                        c = s[i]
                        if value isnt '.'
                            return token 'number', +value.substr 0, value.length - 1
                        b = true
                    else
                        loop
                            value += c
                            ++i
                            c = s[i]
                            break unless isDigit c
                if not b
                    if c is 'e' or c is 'E'
                        value += c
                        ++i
                        c = s[i]
                        if c is '+' or c is '-'
                            value += c
                            ++i
                            c = s[i]
                        if not isDigit c
                            character = 1 + i - lastNewline
                            syntaxError 'Expected digit'
                        loop
                            value += c
                            ++i
                            c = s[i]
                            break unless isDigit c
                    if isIdentifier c
                        i = start
                        c = s[i]
                    else
                        return token 'number', +value

            if c is '@' or c is '.' or isIdentifier c
                type =
                    if c is '@'
                        'formal parameter'
                    else if c is '.'
                        'symbol'
                    else
                        'identifier'
                value = ''
                loop
                    value += c
                    ++i
                    c = s[i]
                    break unless isIdentifier c
                if value is 'true' or value is 'false'
                    return token 'boolean', value is 'true'
                if -1 isnt ['nil', 'unknown', 'class', 'module', 'try', 'catch', 'finally'].indexOf value
                    return token value, value
                if type is 'symbol'
                    if value.length is 1
                        character = 1 + i - lastNewline
                        syntaxError 'Expected identifier'
                    return token type, value.substr 1
                return token type, value

            if c is '"' or c is "'"
                delimiter = c
                value = ''
                ++i
                c = s[i]
                loop
                    break if c is delimiter
                    if c is '\\'
                        ++i
                        switch c = s[i]
                            when '"', "'", '\\' then value += c
                            when 'n' then value += '\n'
                            when 'r' then value += '\r'
                            when 't' then value += '\t'
                            else
                                character = 1 + i - lastNewline
                                syntaxError 'Invalid escape sequence'
                        ++i
                        c = s[i]
                        continue
                    if not c
                        syntaxError 'Unterminated string'
                    value += c
                    ++i
                    c = s[i]
                ++i
                return token 'string', value

            syntaxError "Unexpected #{c}"

        here = advance()
        next = advance()
        prev = null

        here: -> here
        next: -> next
        advance: ->
            prev = here
            here = next
            next = advance()
            prev
        match: (types...) ->
            if -1 isnt types.indexOf here.type
                return @advance()
            if prev and prev.type is 'outdent' and -1 isnt types.indexOf 'newline'
                return {
                    type: 'newline'
                    value: ''
                    file: prev.file
                    line: prev.line
                    character: prev.character
                }
            null
        accept: (values...) ->
            if -1 isnt values.indexOf here.value
                return @advance()
            null
        require: (types...) ->
            @match(types...) or parseError here, "Expected #{types.join " or "}"
        requireValue: (values...) ->
            @accept(values...) or parseError here, "Expected '#{values.join "', '"}'"

parse = do ->
    symbols = {}

    BINARY_OPERATORS =
        '^': -7
        '*': 6
        '/': 6
        '%': 6
        '+': 5
        '-': 5
        '>': 4
        '<': 4
        '>=': 4
        '<=': 4
        '==': 3
        '!=': 3
        'is': 3
        'isnt': 3
        'and': 2
        'or': 2

    PREFIX_OPERATORS =
        'not': 2

    class Symbol
        null: ->
            parseError "Unexpected operator"
        left: ->
            parseError "Unexpected value"

    parse = (s, file, startLine) ->
        tokens = tokenizer s, file, startLine
        while tokens.match 'newline' then
        result = sequence tokens
        tokens.require 'end'
        result

    sequence = (tokens) ->
        n = tokens.here()
        result = []
        loop
            s = expression tokens, 0, true
            break unless s
            result.push s
            break if not tokens.match 'newline'
        file: n.file
        line: n.line
        character: n.character
        type: 'sequence'
        expressions: result

    expression = (tokens, precedence = 0, optional, noIndent) ->
        if tokens.match 'indent'
            result = sequence tokens
            tokens.require 'outdent'
            return result
        if start = tokens.match 'class', 'module'
            if name = tokens.match 'identifier'
                loop
                    if t = tokens.match 'symbol'
                        name =
                            type: 'application'
                            file: t.file
                            line: t.line
                            character: t.character
                            funject: name
                            argument: t
                        continue
                    break
            if tokens.accept '<'
                parent = expression tokens, 0, false, true
            unless tokens.here().type is 'indent'
                parseError tokens.here(), "Expected indent"
            body = expression tokens
            result = {
                type: start.type
                file: start.file
                line: start.line
                character: start.character
                body
                parent
            }
            return result unless name
            return {
                type: 'assignment'
                operator: 'strict assignment'
                file: start.file
                line: start.line
                character: start.character
                left: name
                right: result
            }
        if start = tokens.match 'try'
            body = expression tokens
            tokens.require 'catch'
            handler = expression tokens
            result = {
                type: 'try'
                file: start.file
                line: start.line
                character: start.character
                body
                catch: handler
            }
            if tokens.match 'finally'
                result.finally = expression tokens
            return result
        e = value tokens
        if not e
            if optional
                return null
            else
                parseError tokens.here(), "Expected expression"
        loop
            if t = tokens.match 'symbol'
                e =
                    type: 'application'
                    file: t.file
                    line: t.line
                    character: t.character
                    funject: e
                    argument: t
                continue
            if tokens.here().type is 'list start'
                t = tokens.here()
                e =
                    type: 'application'
                    file: t.file
                    line: t.line
                    character: t.character
                    funject: e
                    argument: value tokens
                continue
            if precedence < 7
                if tokens.here().type is 'identifier'
                    cont = false
                    for op, p of BINARY_OPERATORS
                        if precedence < Math.abs(p) and t = tokens.accept op
                            e =
                                type: 'application'
                                file: t.file
                                line: t.line
                                character: t.character
                                funject: e
                                argument: t

                            ep = if p < 0 then -1 - p else p
                            if operand = expression tokens, ep, true
                                e =
                                if op is 'and' or op is 'or'
                                    type: op
                                    file: t.file
                                    line: t.line
                                    character: t.character
                                    left: e.funject
                                    right: operand
                                else
                                    type: 'application'
                                    file: t.file
                                    line: t.line
                                    character: t.character
                                    funject: e
                                    argument: operand

                            cont = true
                            break
                    continue if cont
                if 'identifier' is tokens.here().type and not BINARY_OPERATORS[tokens.here().value] or -1 isnt ['formal parameter', 'string', 'number', 'boolean', 'nil', 'unknown', 'funject start', 'group start', 'list start'].indexOf tokens.here().type
                    t = tokens.here()
                    e =
                        type: 'application'
                        file: t.file
                        line: t.line
                        character: t.character
                        funject: e
                        argument: expression tokens, 7
                    continue
                if t = tokens.match 'prototypal application'
                    symbol = tokens.require 'identifier'
                    symbol.type = 'symbol'
                    e =
                        type: 'application'
                        file: t.file
                        line: symbol.line
                        character: symbol.character
                        funject:
                            type: 'application'
                            file: t.file
                            line: t.line
                            character: t.character
                            funject: e
                            argument:
                                type: 'symbol'
                                file: t.file
                                line: t.line
                                character: t.character
                                value: 'instance'
                        argument: symbol
                    continue
                if not noIndent and 'indent' is tokens.here().type
                    t = tokens.here()
                    e =
                        type: 'application'
                        file: t.file
                        line: t.line
                        character: t.character
                        funject: e
                        argument: expression tokens
                    break
            break
        if assignment = tokens.match 'strict assignment', 'lazy assignment', 'reset strict assignment', 'reset lazy assignment', 'inverse assignment', 'inheritance assignment'
            if assignment.type isnt 'inheritance assignment' and assignment.type isnt 'inverse assignment' and e.type isnt 'identifier' and (e.type isnt 'application' or assignment.type isnt 'strict assignment' and assignment.type isnt 'lazy assignment')
                parseError assignment, 'Invalid left-hand side of assignment'
            e =
                type: 'assignment'
                operator: assignment.type
                file: assignment.file
                line: assignment.line
                character: assignment.character
                left: e
                right: expression tokens
        e

    value = (tokens) ->
        if tokens.match 'group start'
            e = expression tokens
            tokens.require 'group end'
            return e
        if t = tokens.match 'list start'
            values = []
            if not tokens.match 'list end'
                loop
                    values.push expression tokens
                    break if tokens.match 'list end'
                    tokens.require 'list delimiter'
            return {
                file: t.file
                line: t.line
                character: t.character
                type: 'list'
                values
            }
        if t = tokens.match 'funject start'
            patterns = []
            if not tokens.match 'funject end'
                if tokens.match 'indent'
                    loop
                        patterns.push pattern tokens
                        break if tokens.match 'outdent'
                        tokens.require 'newline'
                    tokens.require 'funject end'
                else
                    patterns.push pattern tokens
                    tokens.require 'funject end'
            return {
                file: t.file
                line: t.line
                character: t.character
                type: 'funject'
                patterns
            }
        if tokens.here().type is 'identifier' and Object::hasOwnProperty.call PREFIX_OPERATORS, tokens.here().value
            t = tokens.advance()
            return {
                type: 'application'
                file: t.file
                line: t.line
                character: t.character
                funject: expression tokens, PREFIX_OPERATORS[t.value]
                argument:
                    type: 'symbol'
                    file: t.file
                    line: t.line
                    character: t.character
                    value: 'not'
            }
        if t = tokens.match 'identifier', 'formal parameter', 'symbol', 'string', 'number', 'boolean', 'nil', 'unknown'
            return t

    pattern = (tokens) ->
        match = expression tokens
        tokens.require 'pattern match'
        result = expression tokens
        {
            pattern: match
            value: result
        }

    parse

parseForRacket = (s) ->
    transform = (n) ->
        switch n.type
            when 'number' then ['Token-number', n.value]
            when 'symbol' then ['Token-symbol', n.value]
            when 'string' then ['Token-string', n.value]
            when 'boolean' then ['Token-boolean', n.value]
            when 'nil' then ['Token-nil']
            when 'unknown' then ['Token-unknown']
            when 'identifier' then ['Token-identifier', n.value]
            when 'formal parameter' then ['Token-parameter', n.value]
            when 'list' then ['Token-list', transform x for x in n.values]
            when 'funject' then ['Token-funject', [transform(p.pattern), transform(p.value)] for p in n.patterns]
            when 'sequence' then ['Token-sequence', transform x for x in n.expressions]
            when 'application' then ['Token-invocation', transform(n.funject), transform(n.argument)]
            when 'or', 'and' then ['Token-' + n.type, transform(n.left), transform(n.right)]
            when 'class', 'module'
                if n.parent
                    ['Token-' + n.type, transform(n.parent), transform(n.body)]
                else
                    ['Token-' + n.type, transform(n.body)]
            when 'try'
                if n.finally
                    ['Token-try', transform(n.body), transform(n.catch), transform(n.finally)]
                else
                    ['Token-try', transform(n.body), transform(n.catch)]
            when 'assignment'
                switch n.operator
                    when 'strict assignment'
                        if n.left.type is 'application'
                            ['Token-funject-strict-assignment', transform(n.left), transform(n.right)]
                        else
                            ['Token-strict-assignment', transform(n.left), transform(n.right)]
                    when 'lazy assignment'
                        if n.left.type is 'application'
                            ['Token-funject-lazy-assignment', transform(n.left), transform(n.right)]
                        else
                            ['Token-lazy-assignment', transform(n.left), transform(n.right)]
                    when 'reset strict assignment' then ['Token-reset-strict-assignment', transform(n.left), transform(n.right)]
                    when 'reset lazy assignment' then ['Token-reset-lazy-assignment', transform(n.left), transform(n.right)]
                    when 'inheritance assignment' then ['Token-funject-inheritance', transform(n.left), transform(n.right)]
                    when 'inverse assignment' then ['Token-inverse-definition', transform(n.left), transform(n.right)]

    transform parse s

printTokens = (s) ->
    tokens = tokenizer s
    indent = ''
    loop
        t = tokens.advance()
        break if t.type is 'end'
        switch t.type
            when 'indent'
                console.log "#{indent}{"
                indent += '    '
            when 'outdent'
                indent = indent.substr 4
                console.log "#{indent}}"
            when 'newline'
                console.log "#{indent};"
            else
                console.log "#{indent}(:#{t.line}:#{t.character}) <#{t.type}> #{t.value}"

printTree = (s) ->
    print = (n, indent = '') ->
        if n instanceof Array
            if typeof n[0] is 'string'
                name = n[0].replace /^Token-/, ''
                if n.length is 2 and typeof n[1] isnt 'object'
                    console.log "#{indent}#{name}: #{n[1]}"
                else
                    console.log "#{indent}#{name}"
                    if n[1] and typeof n[1][0] isnt 'string'
                        for v in n[1]
                            print v, indent + '    '
                    else
                        for v in n[1..]
                            print v, indent + '    '
            else
                console.log "#{indent}pattern:"
                for v in n[0..]
                    print v, indent + '    '
        else
            console.log indent + n
    print parseForRacket s

stringify = (n) ->

if module?
    _exports = exports
    if not module.parent
        expression = null
        racket = false
        i = 2
        tokens = false
        verbose = false
        argc = process.argv.length
        while i < argc
            switch arg = process.argv[i++]
                when '-r' then racket = true
                when '-t' then tokens = true
                when '-v' then verbose = true
                when '-e'
                    expression = process.argv[i++]
                    break
                else
                    expression = '' + require('fs').readFileSync arg
                    break
        if i < argc or not expression?
            console.error 'Usage: coffee parser.coffee [ -r | -t | -v ] [ <filename> | -e <expression> ]'
            return
        try
            if tokens
                printTokens expression
            else if verbose or racket
                p = if racket then parseForRacket else parse
                console.log JSON.stringify p(expression), undefined, 2
            else
                printTree expression
        catch e
            if e instanceof SyntaxError
                console.error e.message
            else
                throw e
else
    (@Yak ?= {}).parser = _exports = {}

_exports.parse = parse
_exports.stringify = stringify
_exports.tokenizer = tokenizer
_exports.parseForRacket = parseForRacket
environment = global ? @

SPECIAL_FORM = {}

last = (thing) -> thing[thing.length - 1]

extend = (object, properties) ->
    object[key] = value for key, value of properties
    object

equal = (a, b) ->
    if a.type isnt b.type
        return false
    switch a.type
        when 'list'
            if a.value.length isnt b.value.length
                return false
            for x, i in a
                if not equal x, b[i]
                    return false
            return true
        when 'funject'
            return a is b
        when 'number', 'boolean', 'string', 'symbol'
            return a.value is b.value
        when 'nil', 'unknown'
            return true

class InterpreterError
    constructor: (@message) ->

class RuntimeError extends Error
    constructor: (@message, @trace) ->

class MatchError extends InterpreterError
    constructor: (funject, argument) ->
        super "No match for #{funject} applied to #{argument}"

class Scope
    constructor: (@parent, vars, unnamed = true) ->
        @vars = Object.create null
        if vars?
            for name, value of vars
                if unnamed
                    @vars[name] = value
                else
                    @set name, value

    get: (name) ->
        if Object::hasOwnProperty.call @vars, name
            return @vars[name]
        if @parent
            return @parent.get name
        throw new InterpreterError "Undefined variable #{name}"

    set: (name, value) ->
        if @isProxy
            @parent.set name, value
        else
            value.name ?= name
            @vars[name] = value

    reset: (name, value) ->
        if Object::hasOwnProperty.call @vars, name
            return @vars[name] = value
        if @parent
            return @parent.reset name, value
        throw new InterpreterError "Can't reset undefined variable #{name}"

class Funject
    type: 'funject'

    toString: ->
        try
            s = new Interpreter().evaluate
                type: 'application'
                funject:
                    type: 'value'
                    value: @
                argument:
                    type: 'symbol'
                    value: 'to-string'
            if s.isString
                s.value
            else
                @basicToString()
        catch e
            if e instanceof RuntimeError
                @basicToString()
            else
                throw e

    basicToString: ->
        if @isClass
            if @name
                "#<class #{@name}>"
            else
                '#<class>'
        else if @call and @call.length isnt 0
            '#<primitive>'
        else
            '#<funject>'

    toSource: (depth) ->
        if depth is 0
            return @basicToString()
        try
            s = new Interpreter().evaluate
                type: 'application'
                funject:
                    type: 'value'
                    value: @
                argument:
                    type: 'symbol'
                    value: 'inspect'
            if s.isString
                return s.value
            else
                return @getSource()
        catch e
            if e instanceof RuntimeError
                return @getSource depth
            else
                throw e

    getSource: (depth) ->
        if depth is 0 or @isClass or (@call and @call.length isnt 0)
            @basicToString()
        else if not @patterns or @patterns.length is 0
            '{}'
        else
            ###
            s = "{"
            for p in @patterns
                s += "\n    "
                s += parser.stringify p.pattern
                s += ": "
                s += (if p.expression
                    parser.stringify p.expression
                else
                    p.value.toSource depth - 1
                ).replace /\n/g, '\n    '
            s += "\n}"
            ###
            @basicToString() # TODO

    constructor: (properties) ->
        @patterns = []
        if properties
            @[key] = value for key, value of properties

    methodHasArgs: ->
        method = @
        hasArgs = null
        answer = (result) ->
            if hasArgs? and hasArgs isnt result
                throw new InterpreterError 'Method cannot match both [@self] and [@self, @arg]'
            hasArgs = result
        loop
            if method.call
                i = 0
                length = method.call.length
                while i < length
                    pattern = method.call[i++]
                    while pattern is 'own' or pattern is 'interpreter'
                        pattern = method.call[i++]
                    if pattern instanceof Array
                        if pattern.length is 1
                            answer false
                        if pattern.length is 2
                            answer true
                    ++i
            if method.patterns
                for o in method.patterns
                    p = o.pattern
                    if p.type is 'list' and p.value.length is 1
                        answer false
                    if p.type is 'list' and p.value.length is 2
                        answer true
            if hasArgs?
                return hasArgs
            if not method = method.parent
                throw new InterpreterError "#{@} is not a method"

    hasSymbol: (name) ->
        method = @
        while method
            if method.call
                for p in method.call
                    if p[0] is '.' and name is p.substr 1
                        return true
            if method.patterns
                for p in method.patterns
                    if p.pattern.type is 'symbol' and p.pattern.value is name
                        return true
            method = method.parent
        false

    keys: ->
        pattern = (p) =>
            switch p.type
                when 'list'
                    new ListFunject (pattern v for v in p.values)
                when 'symbol' then yakSymbol p.value
                when 'string' then new StringFunject p.value
                when 'number' then new NumberFunject p.value
                when 'nil', 'unknown' then lang[p.value]
                else throw new InterpreterError "#{@} has non-constant keys"
        result = []
        if @patterns
            for p in @patterns
                result.push pattern p.pattern
        if @call
            i = 0
            length = @call.length
            while i < length
                p = @call[i++]
                while p is 'interpreter' or p is 'own'
                    p = @call[i++]
                if p[0] is '.'
                    result.push yakSymbol p.substr 1
                else if p[0] is '"'
                    result.push new StringFunject p.substr 1
        new ListFunject result

    isObject: ->
        if @patterns
            for p in @patterns
                if p.pattern.type isnt 'symbol'
                    return false
        if @call
            i = 0
            length = @call.length
            while i < length
                p = @call[i++]
                while p is 'interpreter' or p is 'own'
                    p = @call[i++]
                if p[0] isnt '.'
                    return false
        true

    symbolicKeys: ->
        result = []
        if @patterns
            for p in @patterns when p.pattern.type is 'symbol'
                result.push yakSymbol p.pattern.value
        if @call
            i = 0
            length = @call.length
            while i < length
                p = @call[i++]
                while p is 'interpreter' or p is 'own'
                    p = @call[i++]
                if p[0] is '.'
                    result.push yakSymbol p.substr 1
        new ListFunject result

    allKeys: (symbolic = false) ->
        result = []
        f = @
        while f
            for k in (if symbolic then f.symbolicKeys() else f.keys()).value
                add = true
                for l in result
                    if equal l, k
                        add = false
                if add
                    result.push k
            f = f.parent
        new ListFunject result

    native: (pattern, argument) ->
        if pattern instanceof Array
            if pattern[0] is '|'
                for p in pattern[1..]
                    if args = @native p, argument
                        return args
                return false
            args = []
            if not argument.isList or pattern.length isnt argument.value.length
                return false
            for x, i in argument.value
                if a = @native pattern[i], x
                    args = args.concat a
                else
                    return false
            return args
        if pattern is '*'
            return [argument]
        if lang[pattern]
            if argument is lang[pattern]
                return []
            else
                return false
        if typeof pattern is 'string'
            if pattern[0] is '.'
                if argument.isSymbol and argument.value is pattern.substr 1
                    return []
            else if pattern[0] is '"'
                if argument.isString and argument.value is pattern.substr 1
                    return []
            else if pattern[0] is '&'
                if argument is globalScope.get pattern.substr 1
                    return []
            else
                if argument.type is pattern
                    return [argument]
            return false

    scan: (scope, bindings, applications, pattern, argument) ->
        if pattern.type is 'list'
            return false unless argument.isList and argument.value.length is pattern.values.length
            for x, i in pattern.values
                return false unless sub = @scan scope, bindings, applications, x, argument.value[i]
            return true
        if pattern.type is 'formal parameter'
            if Object::hasOwnProperty.call bindings, pattern.value
                return equal bindings[pattern.value], argument
            bindings[pattern.value] = argument
            return true
        if pattern.type is 'identifier'
            return equal argument, scope.get pattern.value
        if pattern.type is 'application'
            applications.push
                funject: pattern.funject
                argument: pattern.argument
                value: argument
            return true
        if pattern.type is 'number' or pattern.type is 'symbol' or pattern.type is 'string' or pattern.type is 'boolean'
            return pattern.type is argument.type and pattern.value is argument.value and true
        if pattern.type is 'nil' or pattern.type is 'unknown'
            return pattern.type is argument.type and true
        throw new InterpreterError "Invalid pattern"

    match: (interpreter, bound, pattern, argument) ->
        if pattern.type is 'list'
            bindings = {}
            for x, i in pattern.values
                return false unless sub = @match interpreter, bound, x, argument.value[i]
                extend bindings, sub
            return bindings
        if pattern.type is 'application'
            arg = interpreter.first().value[interpreter.frame.arg++]
            if not arg.isList
                throw new InterpreterError 'Invalid inverse'
            return false unless arg.value.length
            return @bind arg.value[0], pattern, {}, bound
        return {}

    bind: (value, pattern, bindings, bound) ->
        if pattern.type is 'list'
            @bind value, x, bindings, bound for x in pattern.values
        else if pattern.type is 'formal parameter'
            if not Object::hasOwnProperty.call bound, pattern.value
                bindings[pattern.value] = value
        else if pattern.type is 'application'
            @bind value, pattern.argument, bindings, bound
        return bindings

    substitute: (bindings, argument) ->
        if argument.type is 'list'
            return new ListFunject (@substitute bindings, x for x in argument.value)
        if argument.type is 'number'
            return new NumberFunject argument.value
        if argument.type is 'string'
            return new StringFunject argument.value
        if argument.type is 'symbol'
            return yakSymbol argument.value
        if argument.type is 'formal parameter'
            if Object::hasOwnProperty.call bindings, argument.value
                return bindings[argument.value]
            return lang.unknown
        if argument.type is 'application'
            throw new InterpreterError 'Nested applications are unimplemented'
        if argument.type is 'boolean' or argument.type is 'nil' or argument.type is 'unknown'
            return lang[argument.value]

    apply: (interpreter, own, argument, instance = true) ->
        if @call
            i = 0
            length = @call.length
            while i < length
                pattern = @call[i++]
                if provideInterpreter = pattern is 'interpreter'
                    pattern = @call[i++]
                if provideSelf = pattern is 'own'
                    pattern = @call[i++]
                value = @call[i++]
                if args = @native pattern, argument
                    if provideSelf
                        args.unshift own
                    if provideInterpreter
                        args.unshift interpreter
                    if SPECIAL_FORM is result = value.apply @, args
                        return
                    return interpreter.return result

        if @patterns
            arg = interpreter.frame.arg ? 0
            offset = interpreter.frame.index ? 0
            for p, i in @patterns[offset..]
                applications = []
                if interpreter.frame.bindings
                    bindings = interpreter.frame.bindings
                    applications = interpreter.frame.applications
                    delete interpreter.frame.applications
                    delete interpreter.frame.bindings
                else
                    bindings = {}
                    continue unless @scan p.scope, bindings, applications, p.pattern, argument
                if applications.length
                    if interpreter.frame.step is 'match'
                        unless bound = @match interpreter, bindings, p.pattern, argument
                            delete interpreter.frame.step
                            continue
                        extend bindings, bound
                    else if interpreter.frame.step is 'inverse'
                        n = interpreter.frame.expression
                        interpreter.pop()
                        interpreter.stack.push {
                            expression: {
                                line: n.line
                                character: n.character
                                type: 'reapply funject'
                                funject: @
                                argument
                                own
                            }
                            arguments: []
                            step: 'match'
                            applications
                            bindings
                            arg
                            index: offset + i
                        }
                        interpreter.push
                            line: p.pattern.line
                            character: p.pattern.character
                            type: 'list'
                            values:
                                for a, i in interpreter.first().value
                                    if not a.inverse
                                        throw new InterpreterError "#{a} has no inverse"
                                    type: 'application'
                                    funject:
                                        type: 'value'
                                        value: a.inverse
                                    argument:
                                        type: 'value'
                                        value: new ListFunject [
                                            applications[i].value
                                            @substitute bindings, applications[i].argument
                                        ]
                        return
                    else
                        n = interpreter.frame.expression
                        interpreter.pop()
                        interpreter.stack.push {
                            expression: {
                                line: n.line
                                character: n.character
                                type: 'reapply funject'
                                funject: @
                                argument
                                own
                            }
                            arguments: []
                            step: 'inverse'
                            applications
                            bindings
                            arg
                            index: offset + i
                        }
                        interpreter.push
                            line: p.pattern.line
                            character: p.pattern.character
                            type: 'list'
                            values: a.funject for a in applications
                        return
                if p.value
                    return interpreter.return p.value
                else
                    interpreter.pop()
                    scope = interpreter.scope
                    bindings.own = own
                    interpreter.scope = new Scope p.scope, bindings, true
                    interpreter.push { type: 'set scope', scope }
                    interpreter.push p.expression
                return

        if instance and argument.isSymbol and @instance and @instance.hasSymbol argument.value
            exp = interpreter.frame.expression
            interpreter.pop()
            interpreter.pop()
            interpreter.push
                type: 'native'
                value: ->
                    method = @first()
                    method.name ?= 'instance.' + argument.value
                    if method.expression
                        (@frame.expression ?= {}).file = method.expression.file
                        @frame.expression.line = method.expression.line
                        @frame.expression.character = method.expression.character
                    hasArgs = method.methodHasArgs()
                    @callStack.pop()
                    if hasArgs
                        @return new Funject
                            call: ['interpreter', '*', (interpreter, arg) ->
                                interpreter.pop()
                                interpreter.push
                                    type: 'application'
                                    file: exp?.file
                                    line: exp?.line
                                    character: exp?.character
                                    funject:
                                        type: 'value'
                                        value: method
                                    argument:
                                        type: 'value'
                                        value: new ListFunject [own, arg]
                                SPECIAL_FORM]
                    else
                        @pop()
                        @push
                            type: 'application'
                            file: exp?.file
                            line: exp?.line
                            character: exp?.character
                            funject:
                                type: 'value'
                                value: method
                            argument:
                                type: 'value'
                                value: new ListFunject [own]
            interpreter.push
                type: 'application'
                file: exp?.file
                line: exp?.line
                character: exp?.character
                funject:
                    type: 'value'
                    value: @instance
                argument:
                    type: 'value'
                    value: argument
                instance: false
            return

        return @parent.apply interpreter, own, argument, instance if @parent and @parent isnt @

        interpreter.callStack.pop()

        throw new MatchError own, argument

    @bridge: (v, context = environment) ->
        if not v?
            return lang.nil
        if v instanceof Array
            return new ListFunject (Funject.bridge x for x in v)
        switch typeof v
            when 'number' then new NumberFunject v
            when 'string' then new StringFunject v
            when 'boolean' then lang[v]
            when 'function' then new Funject
                call: [
                    'list', (list) -> Funject.bridge v.apply context, Funject.unbridge list,
                    'symbol', (property) -> Funject.bridge v[property.value], v]
            when 'object' then new Funject
                call: [
                    'symbol', (property) -> Funject.bridge v[property.value], v]

    @unbridge: (f) ->
        switch f.type
            when 'nil' then null
            when 'unknown' then throw new InterpreterError "Cannot unbridge #{f.type}"
            when 'number', 'string', 'boolean' then f.value
            when 'symbol' then throw new InterpreterError "Cannot unbridge symbols"
            when 'list' then Funject.unbridge v for v in f.value
            when 'funject'
                if f.isObject()
                    interpreter = new Interpreter()
                    o = {}
                    for k in f.symbolicKeys().value
                        o[k.value] = Funject.unbridge interpreter.evaluate
                            type: 'application'
                            funject:
                                type: 'value'
                                value: f
                            argument:
                                type: 'symbol'
                                value: k.value
                    o
                else
                    () ->
                        Funject.unbridge new Interpreter().evaluate
                            type: 'application'
                            funject:
                                type: 'value'
                                value: f
                            argument:
                                type: 'value'
                                value: Funject.bridge [].slice.call arguments

yakObject = (parent, properties, Type = Funject) ->
    call = []
    if properties
        for k, v of properties
            do (v) ->
                call.push '.' + k, -> v
    return new Type {call, parent}

yakFunction = (pattern, value) ->
    return new Funject call: [pattern, value]

yakClass = (name, extend, {exports, instance, _instance} = {}) ->
    methods = _instance ? yakObject extend?.$instance, instance
    result = yakObject extend, exports ? {}, ClassFunject
    delete result.parent unless extend?
    result.$instance = methods
    result.$super = extend
    result.$subclasses = new ListFunject []
    if extend
        extend.$subclasses.value.push result
    result.instance = (lang.Class ? result).$instance
    result.name = name
    lang[name] = result
    result

yakBoolean = (value) -> lang[!!value]

yakSymbol = (value) ->
    if Object::hasOwnProperty.call SymbolFunject.instances, value
        SymbolFunject.instances[value]
    else
        SymbolFunject.instances[value] = new SymbolFunject value

lang = {}

BaseFunject = yakObject null,
    initialize: yakFunction ['*'], (x) -> lang.nil
    clone: yakFunction ['*'], (x) -> new Funject parent: x
    name: yakFunction ['*'], (x) ->
        if x.name then new StringFunject x.name else lang.nil
    apply: new Funject
        call: ['interpreter', ['*', ['*']], (interpreter, f, x) ->
            interpreter.pop()
            interpreter.push
                type: 'application'
                funject:
                    type: 'value'
                    value: f
                argument:
                    type: 'value'
                    value: x
            SPECIAL_FORM]
    on: new Funject
        call: ['interpreter', ['*', '*'], (interpreter, x, y) ->
            interpreter.pop()
            interpreter.push
                type: 'application'
                funject:
                    type: 'value'
                    value: y
                argument:
                    type: 'value'
                    value: x
            SPECIAL_FORM]
    then: new Funject
        call: ['interpreter', ['*', ['*']], (interpreter, x, y) ->
            interpreter.pop()
            interpreter.push
                type: 'application'
                funject:
                    type: 'value'
                    value: y
                argument:
                    type: 'value'
                    value: x
            SPECIAL_FORM]
    is: yakFunction ['*', '*'], (x, y) -> yakBoolean x is y
    isnt: yakFunction ['*', '*'], (x, y) -> yakBoolean x isnt y
    '==': yakFunction ['*', '*'], (x, y) -> yakBoolean equal x, y
    '!=': yakFunction ['*', '*'], (x, y) -> yakBoolean not equal x, y
    'symbol?': yakFunction ['*'], (x) -> yakBoolean x.isSymbol
    'string?': yakFunction ['*'], (x) -> yakBoolean x.isString
    'number?': yakFunction ['*'], (x) -> yakBoolean x.isNumber
    'list?': yakFunction ['*'], (x) -> yakBoolean x.isList
    'boolean?': yakFunction ['*'], (x) -> yakBoolean x.isBoolean
    'nil?': yakFunction ['*'], (x) -> yakBoolean x.isNil
    'unknown?': yakFunction ['*'], (x) -> yakBoolean x.isUnknown
    'class?': yakFunction ['*'], (x) -> yakBoolean x.isClass
    'integer?': yakFunction ['*'], (x) ->
        yakBoolean x.isNumber and x.isInteger()
    'float?': yakFunction ['*'], (x) ->
        yakBoolean x.isNumber and x.isFloat()
    'to-string': yakFunction ['*'], (x) ->
        if x.type is 'funject' or x.type is 'class'
            new StringFunject Funject::basicToString.call x
        else
            new StringFunject '' + x
    inspect: yakFunction ['*'], (x) ->
        if x.type is 'funject' or x.type is 'class'
            new StringFunject x.getSource -1
        else
            new StringFunject x.toSource()
    keys: yakFunction ['*'], (f) -> f.keys()
    'all-keys': yakFunction ['*'], (f) -> f.allKeys()
    each: new Funject
        call: ['interpreter', ['funject', ['funject']], (interpreter, f, iterator) ->
            next = ->
                interpreter.push
                    type: 'application'
                    funject:
                        type: 'value'
                        value: iterator
                    argument:
                        type: 'list'
                        values: [{
                            type: 'value'
                            value: keys[i]
                        }, {
                            type: 'application'
                            funject:
                                type: 'value'
                                value: f
                            argument:
                                type: 'value'
                                value: keys[i]
                        }]
            i = 0
            keys = f.keys().value
            end = keys.length
            return lang.nil if end is 0
            interpreter.pop()
            interpreter.push
                type: 'native'
                value: ->
                    ++i
                    if i is end
                        return @return new ListFunject @frame.arguments
                    next()
            next()
            SPECIAL_FORM]

Funject::instance = BaseFunject
BaseFunject.instance = null

class ClassFunject extends Funject
    type: 'class'
    isClass: true

class ListFunject extends Funject
    type: 'list'
    isList: true

    copy: -> new ListFunject @value

    constructor: (@value) ->
    toString: -> "[#{@value.join ', '}]"
    toSource: (depth) -> "[#{(v.toSource depth - 1 for v in @value).join ', '}]"

    call: ['.class', -> lang.List]

yakClass 'Class', null,
    instance:
        superclass: yakFunction ['class'], (f) -> f.$super ? lang.nil
        subclasses: yakFunction ['class'], (f) -> f.$subclasses.copy()
        instance: yakFunction ['class'], (f) -> f.$instance
        methods: yakFunction ['class'], (f) -> f.$instance.symbolicKeys()
        'all-methods': yakFunction ['class'], (f) -> f.$instance.allKeys true

ClassFunject::parent = yakObject null,
    class: lang.Class
ClassFunject::instance = lang.Class.$instance

lang.Class.$instance.parent = BaseFunject

yakClass 'Funject', null,
    _instance: BaseFunject

lang.Class.$super = lang.Funject
lang.Funject.$subclasses.value.push lang.Class

Funject::parent = yakObject null,
    class: lang.Funject

yakClass 'Symbol', lang.Funject,
    instance: {}

yakClass 'String', lang.Funject,
    instance:
        initialize: yakFunction ['string', ['string']], (s) ->
            if s.value?
                throw new InterpreterError "#{s} is immutable"
            @value = s
        length: yakFunction ['string'], (s) ->
            new NumberFunject s.value.length
        '+': new Funject
            call: [['string', 'string'], (x, y) ->
                new StringFunject x.value + y.value]
            inverse: new Funject
                call: [
                    ['string', ['unknown', 'string']], (s, x) ->
                        if x.value isnt s.value.slice -x.value.length
                            new ListFunject []
                        else
                            new ListFunject [new StringFunject s.value.slice 0, -x.value.length]
                    ['string', ['string', 'unknown']], (s, x) ->
                        if x.value isnt s.value.slice 0, x.value.length
                            new ListFunject []
                        else
                            new ListFunject [new StringFunject s.value.slice x.value.length]]
        '*': yakFunction ['string', 'number'], (s, n) -> s.repeat n

integerIdentityInverse = new Funject
    call: [
        ['number', ['unknown']], (x) ->
            if x.isInteger()
                new ListFunject [x]
            else
                new ListFunject []]

yakClass 'Number', lang.Funject,
    exports:
        random: new Funject call: [
            [], -> new NumberFunject Math.random(),
            ['number'], (x) ->
                if not x.isInteger()
                    throw new InterpreterError "Cannot generate random[#{x}]"
                new NumberFunject Math.floor Math.random() * x.value
            ['number', 'number'], (x, y) ->
                if not x.isInteger() or not y.isInteger() or y.value <= x.value
                    throw new InterpreterError "Cannot generate random[#{x}, #{y}]"
                new NumberFunject x.value + Math.floor Math.random() * (y.value - x.value)]
    instance:
        'degrees-to-radians': yakFunction ['number'], (x) ->
            new NumberFunject x.value * Math.PI / 180
        'radians-to-degrees': yakFunction ['number'], (x) ->
            new NumberFunject x.value * 180 / Math.PI
        max: yakFunction ['number', ['number']], (x, y) ->
            new NumberFunject (if x.value > y.value then x.value else y.value)
        min: yakFunction ['number', ['number']], (x, y) ->
            new NumberFunject (if x.value < y.value then x.value else y.value)
        sqrt: yakFunction ['number'], (x) ->
            if x.value < 0
                throw new InterpreterError "Cannot compute #{x}.sqrt"
            new NumberFunject Math.sqrt x.value
        root: yakFunction ['number', ['number']], (x, y) ->
            new NumberFunject Math.pow x.value, 1 / y.value
        ln: yakFunction ['number'], (x) ->
            new NumberFunject Math.log x.value
        log: yakFunction ['number', ['number']], (x, b) ->
            new NumberFunject Math.log(x.value) / Math.log(b.value)
        sin: yakFunction ['number'], (x) ->
            new NumberFunject Math.sin x.value
        cos: yakFunction ['number'], (x) ->
            new NumberFunject Math.cos x.value
        tan: yakFunction ['number'], (x) ->
            new NumberFunject Math.tan x.value
        sec: yakFunction ['number'], (x) ->
            new NumberFunject 1 / Math.cos x.value
        csc: yakFunction ['number'], (x) ->
            new NumberFunject 1 / Math.sin x.value
        cot: yakFunction ['number'], (x) ->
            new NumberFunject 1 / Math.tan x.value
        asin: yakFunction ['number'], (x) ->
            new NumberFunject Math.asin x.value
        acos: yakFunction ['number'], (x) ->
            new NumberFunject Math.acos x.value
        atan: yakFunction ['number'], (x) ->
            new NumberFunject Math.atan x.value
        'atan/': yakFunction ['number', ['number']], (y, x) ->
            new NumberFunject Math.atan2 y.value, x.value
        asec: yakFunction ['number'], (x) ->
            new NumberFunject Math.acos 1 / x
        acsc: yakFunction ['number'], (x) ->
            new NumberFunject Math.asin 1 / x
        acot: yakFunction ['number'], (x) ->
            new NumberFunject Math.atan 1 / x
        sinh: yakFunction ['number'], (x) ->
            new NumberFunject (Math.exp(x.value) - Math.exp(-x.value)) / 2
        cosh: yakFunction ['number'], (x) ->
            new NumberFunject (Math.exp(x.value) + Math.exp(-x.value)) / 2
        tanh: yakFunction ['number'], (x) ->
            new NumberFunject (Math.exp(x.value * 2) - 1) / (Math.exp(x.value * 2) + 1)
        coth: yakFunction ['number'], (x) ->
            new NumberFunject (Math.exp(x.value * 2) + 1) / (Math.exp(x.value * 2) - 1)
        sech: yakFunction ['number'], (x) ->
            new NumberFunject 2 / (Math.exp(x.value) + Math.exp(-x.value))
        csch: yakFunction ['number'], (x) ->
            new NumberFunject 2 / (Math.exp(x.value) - Math.exp(-x.value))
        asinh: yakFunction ['number'], (x) ->
            new NumberFunject Math.log x.value + Math.sqrt x.value * x.value + 1
        acosh: yakFunction ['number'], (x) ->
            new NumberFunject Math.log x.value + Math.sqrt x.value * x.value - 1
        atanh: yakFunction ['number'], (x) ->
            new NumberFunject Math.log((1 + x.value) / (1 - x.value)) / 2
        acoth: yakFunction ['number'], (x) ->
            new NumberFunject Math.log((1 - x.value) / (1 + x.value)) / 2
        asech: yakFunction ['number'], (x) ->
            new NumberFunject Math.log(1 / x.value + Math.sqrt(1 - x.value * x.value) / x.value)
        acsch: yakFunction ['number'], (x) ->
            new NumberFunject Math.log(1 / x.value + Math.sqrt(1 + x.value * x.value) / Math.abs(x.value))
        abs: new Funject
            call: [['number'], (x) ->
                new NumberFunject if x.value < 0 then -x.value else x.value]
            inverse: new Funject
                call: [['number', ['unknown']], (x) ->
                    new ListFunject [new NumberFunject(x.value), new NumberFunject(-x.value)]]
        ceil: new Funject
            call: [['number'], (x) ->
                new NumberFunject Math.ceil x.value]
            inverse: integerIdentityInverse
        floor: new Funject
            call: [['number'], (x) ->
                new NumberFunject Math.floor x.value]
            inverse: integerIdentityInverse
        round: new Funject
            call: [['number'], (x) ->
                new NumberFunject Math.round x.value]
            inverse: integerIdentityInverse
        'to-fixed': new Funject
            call: [['number', ['number']], (x, y) ->
                if y.value < 0 or y.value > 20
                    throw new InterpreterError "Cannot convert #{x}.to-fixed[#{y}]"
                new StringFunject x.value.toFixed y.value]
            inverse: integerIdentityInverse
        'to-pennies': new Funject
            call: [['number'], (x) ->
                new NumberFunject Math.round x.value * 100]
            inverse: new Funject
                call: [
                    ['number', ['unknown']], (x) ->
                        if x.isInteger()
                            new ListFunject [new NumberFunject Math.round(x.value) / 100]
                        else
                            new ListFunject []]
        'to-dollars': new Funject
            call: [['number'], (x) ->
                new NumberFunject Math.round(x.value) / 100]
            inverse: new Funject
                call: [
                    ['number', ['unknown']], (x) ->
                        if x.isInteger()
                            new ListFunject [new NumberFunject Math.round(x.value) / 100]
                        else
                            new ListFunject []]
        times: new Funject
            call: ['interpreter', ['number', ['*']], (interpreter, x, f) ->
                if not x.isInteger() or x.value < 0
                    throw new InterpreterError "Cannot #{x}.times[...]"
                i = 0
                end = x.value
                return new ListFunject [] if end is 0
                interpreter.pop()
                interpreter.push
                    type: 'native'
                    value: ->
                        ++i
                        if i is end
                            return @return new ListFunject @frame.arguments
                        @push
                            type: 'application'
                            funject:
                                type: 'value'
                                value: f
                            argument:
                                type: 'value'
                                value: new ListFunject [new NumberFunject i]
                interpreter.push
                    type: 'application'
                    funject:
                        type: 'value'
                        value: f
                    argument:
                        type: 'value'
                        value: new ListFunject [new NumberFunject i]
                SPECIAL_FORM]
        upto: yakFunction ['number', ['number']], (x, y) ->
            if y.value < x.value
                new ListFunject []
            else
                new ListFunject (new NumberFunject i for i in [x.value..y.value])
        downto: yakFunction ['number', ['number']], (x, y) ->
            if y.value > x.value
                new ListFunject []
            else
                new ListFunject (new NumberFunject i for i in [x.value..y.value])
        '+': new Funject
            call: [
                ['number', 'number'], (x, y) ->
                    new NumberFunject x.value + y.value]
            inverse: new Funject
                call: [
                    ['number', ['|', ['number', 'unknown'], ['unknown', 'number']]], (r, x) ->
                        new ListFunject [new NumberFunject r.value - x.value]]
        '-': new Funject
            call: [
                ['number', 'number'], (x, y) ->
                    new NumberFunject x.value - y.value]
            inverse: new Funject
                call: [
                    ['number', ['number', 'unknown']], (r, x) ->
                        new ListFunject [new NumberFunject x.value - r.value],
                    ['number', ['unknown', 'number']], (r, x) ->
                        new ListFunject [new NumberFunject r.value + x.value]]
        '*': new Funject
            call: [
                ['number', 'number'], (x, y) ->
                    new NumberFunject x.value * y.value
                ['number', 'string'], (n, s) -> s.repeat n]
            inverse: new Funject
                call: [
                    ['number', ['|', ['number', 'unknown'], ['unknown', 'number']]], (r, x) ->
                        new ListFunject [new NumberFunject r.value / x.value]]
        '/': new Funject
            call: [
                ['number', 'number'], (x, y) ->
                    new NumberFunject x.value / y.value]
            inverse: new Funject
                call: [
                    ['number', ['number', 'unknown']], (r, x) ->
                        new ListFunject [new NumberFunject x.value / r.value],
                    ['number', ['unknown', 'number']], (r, x) ->
                        new ListFunject [new NumberFunject r.value * x.value]]
        '%': yakFunction ['number', 'number'], (x, y) ->
                result = x.value % y.value
                result += y.value if result < 0
                new NumberFunject result
        '^': new Funject
            call: [
                ['number', 'number'], (x, y) -> new NumberFunject Math.pow x.value, y.value]
            inverse: new Funject
                call: [
                    ['number', ['number', 'unknown']], (r, x) ->
                        new ListFunject [new NumberFunject Math.log(r.value) / Math.log(x.value)],
                    ['number', ['unknown', 'number']], (r, x) ->
                        new ListFunject [new NumberFunject Math.pow r.value, 1 / x.value]]
        '>': yakFunction ['number', 'number'], (x, y) -> yakBoolean x.value > y.value
        '<': yakFunction ['number', 'number'], (x, y) -> yakBoolean x.value < y.value
        '>=': yakFunction ['number', 'number'], (x, y) -> yakBoolean x.value >= y.value
        '<=': yakFunction ['number', 'number'], (x, y) -> yakBoolean x.value <= y.value

lang.Number.call.unshift(
    '.e', -> new NumberFunject Math.E
    '.pi', -> new NumberFunject Math.PI)

yakClass 'List', lang.Funject,
    instance:
        head: yakFunction ['list'], (x) ->
            if x.value.length
                x.value[0]
            else
                throw new InterpreterError 'Cannot take the head of the empty list'
        tail: yakFunction ['list'], (x) ->
            if x.value.length > 1
                new ListFunject x.value[1..]
            else
                lang.nil
        'empty?': yakFunction ['list'], (x) ->
            yakBoolean x.value.length is 0
        length: yakFunction ['list'], (x) ->
            new NumberFunject x.value.length
        length: yakFunction ['list'], (x) ->
            new NumberFunject x.value.length
        sort: new Funject
            call: [
                ['list', []], (x) ->
                    return new ListFunject x.value.slice(0).sort (a, b) ->
                        if a.type is 'funject' or a.type is 'class'
                            0
                        else
                            ('' + a).localeCompare('' + b)
                'interpreter', ['list', ['funject']], (interpreter, x, f) ->
                    i = 0
                    list = x.value.slice 0
                    end = list.length
                    return new ListFunject [] if end is 0
                    interpreter.pop()
                    interpreter.push
                        type: 'native'
                        value: ->
                            ++i
                            if i is end
                                xs = @frame.arguments
                                return @return new ListFunject list.slice(0).sort (a, b) ->
                                    xs[list.indexOf a] - xs[list.indexOf b]
                            @push
                                type: 'application'
                                funject:
                                    type: 'value'
                                    value: f
                                argument:
                                    type: 'value'
                                    value: new ListFunject [list[i]]
                    interpreter.push
                        type: 'application'
                        funject:
                            type: 'value'
                            value: f
                        argument:
                            type: 'value'
                            value: new ListFunject [list[i]]
                    SPECIAL_FORM]
        map: new Funject
            call: ['interpreter', ['list', ['funject']], (interpreter, x, f) ->
                i = 0
                list = x.value.slice 0
                end = list.length
                return new ListFunject [] if end is 0
                interpreter.pop()
                interpreter.push
                    type: 'native'
                    value: ->
                        ++i
                        if i is end
                            return @return new ListFunject @frame.arguments
                        @push
                            type: 'application'
                            funject:
                                type: 'value'
                                value: f
                            argument:
                                type: 'value'
                                value: new ListFunject [list[i]]
                interpreter.push
                    type: 'application'
                    funject:
                        type: 'value'
                        value: f
                    argument:
                        type: 'value'
                        value: new ListFunject [list[i]]
                SPECIAL_FORM]
        filter: new Funject
            call: ['interpreter', ['list', ['funject']], (interpreter, x, f) ->
                i = 0
                list = x.value.slice 0
                end = list.length
                return new ListFunject [] if end is 0
                result = []
                interpreter.pop()
                interpreter.push
                    type: 'native'
                    value: ->
                        include = @frame.arguments[@frame.arguments.length - 1]
                        if not include.isBoolean
                            throw new InterpreterError "Cannot filter on #{include}"
                        if include.value
                            result.push list[i]
                        ++i
                        if i is end
                            return @return new ListFunject result
                        @push
                            type: 'application'
                            funject:
                                type: 'value'
                                value: f
                            argument:
                                type: 'value'
                                value: new ListFunject [list[i]]
                interpreter.push
                    type: 'application'
                    funject:
                        type: 'value'
                        value: f
                    argument:
                        type: 'value'
                        value: new ListFunject [list[i]]
                SPECIAL_FORM]
        each: new Funject
            call: ['interpreter', ['list', ['funject']], (interpreter, x, f) ->
                i = 0
                list = x.value.slice 0
                end = list.length
                return lang.nil if end is 0
                interpreter.pop()
                interpreter.push
                    type: 'native'
                    value: ->
                        @frame.arguments.pop()
                        ++i
                        if i is end
                            return @return lang.nil
                        @push
                            type: 'application'
                            funject:
                                type: 'value'
                                value: f
                            argument:
                                type: 'value'
                                value: new ListFunject [list[i]]
                interpreter.push
                    type: 'application'
                    funject:
                        type: 'value'
                        value: f
                    argument:
                        type: 'value'
                        value: new ListFunject [list[i]]
                SPECIAL_FORM]

ListFunject::instance = lang.List.$instance

yakClass 'Boolean', lang.Funject,
    instance:
        not: new Funject
            call: [['boolean'], (x) ->
                yakBoolean not x.value]
            inverse: new Funject
                call: [['boolean', ['unknown']], (x) ->
                    new ListFunject [yakBoolean not x.value]]
        and: yakFunction ['boolean', 'boolean'], (x, y) ->
            yakBoolean x.value and y.value
        or: yakFunction ['boolean', 'boolean'], (x, y) ->
            yakBoolean x.value or y.value
        xor: yakFunction ['boolean', 'boolean'], (x, y) ->
            yakBoolean x.value isnt y.value

yakClass 'Nil', lang.Funject
yakClass 'Unknown', lang.Funject

class SymbolFunject extends Funject
    @instances: {}

    instance: lang.Symbol.$instance
    type: 'symbol'
    isSymbol: true

    constructor: (@value) ->
    toString: -> @value
    toSource: -> "." + @value

    call: ['.class', -> lang.Symbol]

class StringFunject extends Funject
    instance: lang.String.$instance
    type: 'string'
    isString: true

    constructor: (@value) ->

    call: [
        'own', ['number'], (s, n) ->
            i = if n.value < 0 then s.value.length + n.value else n.value
            if i < 0 or i >= s.value.length
                lang.nil
            else
                new StringFunject s.value.charAt i
        '.class', -> lang.String]

    repeat: (n) ->
        if not n.isInteger() or n.value < 0
            throw new InterpreterError "Cannot create #{s.toSource()} * #{n}"
        new StringFunject Array(n.value + 1).join @value

    # The [] are there to avoid a syntax highlighting bug
    toString: -> @value
    toSource: -> "'" + @value.replace(/[\\]/g, '\\\\').replace(/'/g, "\\'").replace(/\n/g, '\\n').replace(/\r/g, '\\r').replace(/\t/g, '\\t') + "'"

class NumberFunject extends Funject
    instance: lang.Number.$instance
    type: 'number'
    isNumber: true
    isInteger: -> @value % 1 is 0
    isFloat: -> @value % 1 isnt 0

    constructor: (@value) ->
    toString: -> '' + @value
    toSource: -> '' + @value
    valueOf: -> @value

    call: ['.class', -> lang.Number]

class BooleanFunject extends Funject
    instance: lang.Boolean.$instance
    type: 'boolean'
    isBoolean: true

    constructor: (value) -> @value = !!value
    toString: -> '' + @value
    toSource: -> '' + @value

    call: ['.class', -> lang.Boolean]

lang.nil = new Funject
    instance: lang.Nil.$instance
    type: 'nil'
    isNil: true

    toString: -> 'nil'
    toSource: -> 'nil'

lang.unknown = new Funject
    instance: lang.Unknown.$instance
    type: 'unknown'
    isUnknown: true

    toString: -> 'unknown'
    toSource: -> 'unknown'

lang.true = new BooleanFunject true

lang.false = new BooleanFunject false

globalScope = new class extends Scope
    name: '<global scope>'

    constructor: ->
        super null, lang

    get: (name) ->
        try
            super name
        catch e
            if Object::hasOwnProperty.call environment, name
                Funject.bridge environment[name]
            else
                throw e

for operator in ['^', '*', '/', '%', '+', '-', '==', '!=', '<', '>', '<=', '>=', 'is', 'isnt']
    globalScope.set operator, yakSymbol operator

globalScope.set 'error', new Funject
    call: ['interpreter', ['*'], (interpreter, message) ->
        interpreter.pop()
        interpreter.pop()
        interpreter.callStack.pop()
        throw new InterpreterError '' + message
        SPECIAL_FORM]

globalScope.set 'print', new Funject
    call: [['*'], (thing) ->
        console.log '' + thing
        lang.nil]

globalScope.set 'debug', new Funject
    call: [['*'], (thing) ->
        console.log thing.toSource -1
        lang.nil]

itself = (n) -> @return lang[n.type]
variable = (n) -> @return @scope.get n.value
logical = (n) ->
    return unless @args n.left
    left = @first()
    if not left.isBoolean
        throw new InterpreterError "Non-boolean applied to ##{n.type}"
    if (if n.type is 'and' then not left.value else left.value)
        return @return yakBoolean n.type is 'or'
    return unless @args n.left, n.right
    right = @second()
    if not right.isBoolean
        throw new InterpreterError "##{n.type} applied to non-boolean"
    @return yakBoolean right.value
proxy = (f) -> (n) ->
    f.call @, n
    if @frame.arguments.length
        @return @first()
    else
        @pop()

class Interpreter
    expressions:
        number: (n) -> @return new NumberFunject n.value
        symbol: (n)-> @return yakSymbol n.value
        string: (n) -> @return new StringFunject n.value
        boolean: (n) -> @return lang[n.value]
        nil: itself
        unknown: itself

        value: (n) -> @return n.value

        'pop scope': proxy (n) -> @scope = @scope.parent
        'set scope': proxy (n) -> @scope = n.scope
        'pop call stack': proxy -> @callStack.pop()
        'pop catch stack': proxy -> @catchStack.pop()
        'ignore arguments': -> @pop()

        'reapply funject': (n) ->
            n.funject.apply @, n.own, n.argument

        native: (n) ->
            n.value.call @, n

        sequence: (n) ->
            if @frame.arguments.length is 0
                @scope = new Scope @scope
                @scope.isProxy = n.isProxy
                @scope.name = '<sequence>'
                @scope.line = n.line
                @scope.character = n.character
            return unless @args n.expressions...
            @popScope()
            @return last @frame.arguments

        assignment: (n) ->
            switch n.operator
                when 'inverse assignment'
                    return unless @args n.left, n.right
                    @first().inverse = @second()
                    return @return @first()
                when 'inheritance assignment'
                    return unless @args n.left, n.right
                    @first().parent = @second()
                    return @return @first()
            if n.left.type is 'identifier'
                switch n.operator
                    when 'strict assignment'
                        return unless @args n.right
                        @scope.set n.left.value, @first()
                        return @return @first()
                    when 'reset strict assignment'
                        return unless @args n.right
                        @scope.reset n.left.value, @first()
                        return @return @first()
                    when 'lazy assignment'
                        @scope.set n.left.value,
                            lazy: n.right
                            scope: @scope
                        return @return lang.nil
                    when 'reset lazy assignment'
                        @scope.reset n.left.value,
                            lazy: n.right
                            scope: @scope
                        return @return lang.nil
            if n.left.type is 'application'
                return unless @args n.left.funject
                funject = @first()
                if n.operator isnt 'lazy assignment' and @frame.arguments.length is 1
                    @push n.right
                    return
                (funject.patterns ?= []).unshift (
                    if n.operator is 'lazy assignment'
                        pattern: n.left.argument
                        expression: n.right
                        scope: @scope
                    else
                        pattern: n.left.argument
                        value: @second()
                        scope: @scope)
                return @return (if n.operator is 'lazy assignment' then lang.nil else @second())
            throw new InterpreterError "Unimplemented: #{n.operator}"

        identifier: variable
        'formal parameter': variable
        list: (n) ->
            return unless @args n.values...
            @return new ListFunject @frame.arguments
        funject: (n) ->
            f = new Funject
            f.expression = n
            f.patterns = (for p in n.patterns
                pattern: p.pattern
                expression: p.value
                scope: @scope)
            @return f
        try: (n) ->
            @pop()
            if n.finally
                @push n.finally
                @push type: 'ignore arguments'
            @catchStack.push
                index: @stack.length
                catch: n.catch
                scope: @scope
            @push type: 'pop catch stack'
            @push n.body
        module: (n) ->
            p = n.parent?
            return if p and not @args n.parent
            argc = if p then 1 else 0
            if @frame.arguments.length is argc
                @frame.super = if p then @first() else lang.Funject
                @push type: 'pop scope'
                @scope = @frame.scope = new Scope @scope, {
                    exports: new Funject parent: @frame.super
                    super: @frame.super
                }, true
                if p
                    @args n.parent, n.body
                else
                    @args n.body
                return
            exports = @frame.scope.vars.exports
            exports.parent = @frame.super
            @return exports
        class: (n) ->
            if (e = @stack[@stack.length - 2].expression) and e.type is 'assignment' and e.left.type is 'identifier'
                @frame.name = e.left.value
            else
                @frame.name = '<class>'
            p = n.parent?
            return if p and not @args n.parent
            argc = if p then 1 else 0
            if @frame.arguments.length is argc
                @frame.super = if p then @first() else lang.Funject
                if not @frame.super.isClass
                    throw new InterpreterError "Cannot inherit from #{@frame.super}"
                @push type: 'pop scope'
                @scope = @frame.scope = new Scope @scope, {
                    exports: new ClassFunject
                        parent: @frame.super
                        name: @frame.name
                        expression: n
                    super: @frame.super
                    instance: new Funject
                        parent: @frame.super.$instance
                        name: "#{@frame.name}.instance"
                        expression: n
                }, true
                if p
                    @args 0, n.body
                else
                    @args n.body
                return
            exports = @frame.scope.vars.exports
            exports.parent = @frame.super
            exports.$super = @frame.super
            exports.$subclasses = new ListFunject []
            @frame.super.$subclasses.value.push exports
            instance = exports.$instance = @frame.scope.vars.instance
            instance.parent = @frame.super.$instance
            prototype = yakObject null,
                class: exports
            (exports.call ?= []).unshift(
                'interpreter', '.new', (interpreter) ->
                    exp = interpreter.frame.expression
                    interpreter.pop()
                    interpreter.pop()
                    interpreter.callStack.pop()
                    interpreter.push
                        type: 'native'
                        value: ->
                            initializeCall =
                                type: 'application'
                                file: exp?.file
                                line: exp?.line
                                character: exp?.character
                                instance: false
                                funject:
                                    type: 'value'
                                    value: instance
                                argument:
                                    type: 'symbol'
                                    value: 'initialize'
                            return unless @args initializeCall
                            initialize = @first()
                            initialize.name ?= 'instance.initialize'
                            hasArgs = initialize.methodHasArgs()
                            result = new Funject { parent: prototype, instance }
                            if hasArgs
                                @return new Funject
                                    call: ['interpreter', '*', (interpreter, arg) ->
                                        interpreter.pop()
                                        interpreter.push
                                            type: 'value'
                                            value: result
                                        interpreter.push
                                            type: 'application'
                                            file: exp?.file
                                            line: exp?.line
                                            character: exp?.character
                                            funject:
                                                type: 'value'
                                                value: initialize
                                            argument:
                                                type: 'value'
                                                value: new ListFunject [result, arg]
                                        SPECIAL_FORM]
                            else
                                @pop()
                                @push
                                    type: 'value'
                                    value: result
                                @push
                                    type: 'application'
                                    file: exp?.file
                                    line: exp?.line
                                    character: exp?.character
                                    funject:
                                        type: 'value'
                                        value: initialize
                                    argument:
                                        type: 'value'
                                        value: new ListFunject [result]
                    SPECIAL_FORM
            )
            @return exports
        application: (n) ->
            return unless @args n.funject, n.argument
            if @frame.arguments.length > 2
                return @return @frame.arguments[2]
            funject = @first()
            argument = @second()
            @stack.splice @stack.length - 1, 0,
                expression:
                    type: 'pop call stack'
                arguments: []
            @callStack.push expression: n, name: funject.name
            funject.apply @, n.own ? funject, argument, n.instance
        or: logical
        and: logical

    load: (n) ->
        @scope = globalScope
        @stack = [ arguments: [] ]
        @callStack = []
        @catchStack = []
        n.isProxy = true
        @push n
        @

    evaluate: (n) ->
        @load n
        while @stack.length > 1
            @step()
        last @stack[0].arguments

    step: (n) ->
        if @stack.length is 1
            return true
        @frame = last @stack
        try
            @expression()
        catch error
            if error instanceof InterpreterError
                stack = ''
                calls = @callStack[0..]
                calls.push expression: @frame.expression
                name = null
                for {expression: e, name: n} in calls
                    if name
                        if e.file
                            stack = "\n at #{name} (#{e.file}:#{e.line}:#{e.character})" + stack
                        else
                            stack = "\n at #{name}" + stack
                    else
                        if e.file
                            stack = "\n at #{e.file}:#{e.line}:#{e.character}" + stack
                        else
                            stack = "\n at <anonymous>" + stack
                    name = n
                stack = error.message + stack
                if @catchStack.length
                    entry = @catchStack.pop()
                    @stack = @stack[0..entry.index]
                    @scope = entry.scope
                    @push entry.catch
                else
                    throw new RuntimeError error.message, stack
            else
                if require?
                    console.log require('util').inspect @stack, depth: 5
                throw error
        false

    value: ->
        last @stack[0].arguments

    args: (args...) ->
        length = @frame.arguments.length
        if length and (value = @frame.arguments[length - 1]).lazy
            @frame.arguments.pop()
            scope = @scope
            #@stack.splice @stack.length - 1, 0, { arguments: [], expression: type: 'pop call stack' }
            @push { type: 'set scope', scope }
            @scope = value.scope
            #@callStack.push expression: @frame.last[@frame.last.length - 1] or @frame.expression, name: value.name
            @push value.lazy
            #@frame.last.push value.lazy
            return false
        if length < args.length
            @push args[length]
            #@frame.last = []
            return false
        true

    expression: ->
        n = @frame.expression
        if not @expressions[n.type]?
            throw new Error "Invalid expression type #{n.type}"
        @expressions[n.type].call @, n

    return: (value) ->
        @stack.pop()
        last(@stack).arguments.push value

    push: (n) ->
        @stack.push
            expression: n
            arguments: []

    pop: -> @stack.pop()

    popScope: -> @scope = @scope.parent

    first: (n) -> @frame.arguments[0]
    second: (n) -> @frame.arguments[1]
    third: (n) -> @frame.arguments[2]

evaluateSynchronous = (source, filename, firstLine) ->
    new Interpreter().evaluate parser.parse source, filename, firstLine

MAX_ITERATIONS = 1000
evaluate = (source, filename = '<anonymous>', firstLine = 1, callback = null) ->
    callbacks = []
    dataCallbacks = []
    errorCallbacks = []
    result = (error, data) ->
        c error, data for c in callbacks
        c data for c in dataCallbacks if data
        c error for c in errorCallbacks if error

    if arguments.length is 2
        callback = filename
        filename = '<anonymous>'
    callbacks.push callback if callback

    try
        interpreter = new Interpreter().load parser.parse source, filename, firstLine
        timeout = setTimeout step = ->
            iterations = 0
            try
                loop
                    if interpreter.step()
                        result null, interpreter.value()
                        return
                    else if ++iterations >= MAX_ITERATIONS
                        timeout = setTimeout step
                        return
            catch e
                if e instanceof RuntimeError
                    result e, null
                else
                    throw e
    catch e
        if e instanceof SyntaxError
            setTimeout ->
                result e, null
        else
            throw e
    return {
        abort: ->
            clearTimeout timeout
            @
        then: (callback) ->
            callbacks.push callback
            @
        done: (callback) ->
            dataCallbacks.push callback
            @
        fail: (callback) ->
            errorCallbacks.push callback
            @
    }

repl = ->
    readline = require 'readline'
    rl = readline.createInterface
        input: process.stdin
        output: process.stdout
        completer: (line) ->
            if /^\s*$/.test line
                if s = /(^|\n)([ \t]+).*\n$/.exec read
                    if line.length >= s[2].length
                        [[line + '    '], line]
                    else
                        [[s[2]], line]
                else
                    [[line + '    '], line]
            else
                [[], line]

    FIXED_LINE_LENGTH = 4

    replLine = 1
    setPrompt = (postfix = '>') ->
        s = '' + replLine
        while s.length < FIXED_LINE_LENGTH
            s = '0' + s
        rl.setPrompt s + postfix + ' '

    setPrompt '>'
    rl.prompt()

    read = ''
    firstLine = 1
    sigints = 0
    request = null
    rl.on 'line', (line) ->
        ++replLine
        sigints = 0
        if line is '' and read is ''
            setPrompt '>'
            rl.prompt()
            firstLine = replLine
            return
        read += line + '\n'
        if not /^[\t ]|(^|[(\[])(class|module|try$|catch$|finally$)\b|\[[^\]]*$|\([^)]*$|\{[^\}]*$/.test line
            request = evaluate read, 'input', firstLine, (e, d) ->
                if e
                    if e.trace
                        console.log e.trace
                    else if e.message
                        console.log e.message
                else if d
                    console.log d.toSource 3
                request = null
                read = ''
                firstLine = replLine
                setPrompt '>'
                rl.prompt()
        else
            setPrompt ':'
            rl.prompt()
            # TODO this breaks pasting
            # if s = /(^|\n)([ \t]+).*\n$/.exec read
            #     rl.write s[2]

    rl.on 'SIGINT', ->
        if request?
            request.abort()
            read = ''
        else
            rl._refreshLine() # TODO this is a bad idea
            if rl.line is '' and read is ''
                if sigints
                    rl.close()
                else
                    sigints = 1
                    rl.clearLine()
                    console.log '(^C again to quit)'
            else
                sigints = 0
                rl.clearLine()
                read = ''
        ++replLine
        firstLine = replLine
        setPrompt '>'
        rl.prompt()

    rl.on 'close', ->
        console.log '' # put a newline after the last prompt
        process.exit 0

if module?
    fs = require 'fs'
    path = require 'path'
    parser = require './parser.coffee'

    _exports = exports
    _exports.repl = repl
    if not module.parent
        expressions = []
        i = 2
        expressionNumber = 1
        argc = process.argv.length
        interactive = argc is 2
        while i < argc
            switch arg = process.argv[i++]
                when '-h'
                    console.error 'Usage: coffee interpreter.coffee [ <filename> | -e <expression> | -i | -h ]'
                    return
                when '-i'
                    interactive = true
                when '-e'
                    expressions.push
                        file: "expression#{expressionNumber++}"
                        source: process.argv[i++]
                else
                    expressions.push
                        file: path.resolve arg
                        source: '' + fs.readFileSync arg
        for expression in expressions
            try
                evaluateSynchronous expression.source, expression.file
            catch e
                if e instanceof SyntaxError
                    console.error e.message
                else
                    throw e
        if interactive
            repl()
else
    parser = Yak.parser
    (@Yak ?= {}).interpreter = _exports = {}

_exports.eval = evaluate
_exports.evalSync = evaluateSynchronous
(->
  `
  /*! jQuery v2.0.3 | (c) 2005, 2013 jQuery Foundation, Inc. | jquery.org/license
  //@ sourceMappingURL=jquery-2.0.3.min.map
  */
  (function(e,undefined){var t,n,r=typeof undefined,i=e.location,o=e.document,s=o.documentElement,a=e.jQuery,u=e.$,l={},c=[],p="2.0.3",f=c.concat,h=c.push,d=c.slice,g=c.indexOf,m=l.toString,y=l.hasOwnProperty,v=p.trim,x=function(e,n){return new x.fn.init(e,n,t)},b=/[+-]?(?:\d*\.|)\d+(?:[eE][+-]?\d+|)/.source,w=/\S+/g,T=/^(?:\s*(<[\w\W]+>)[^>]*|#([\w-]*))$/,C=/^<(\w+)\s*\/?>(?:<\/\1>|)$/,k=/^-ms-/,N=/-([\da-z])/gi,E=function(e,t){return t.toUpperCase()},S=function(){o.removeEventListener("DOMContentLoaded",S,!1),e.removeEventListener("load",S,!1),x.ready()};x.fn=x.prototype={jquery:p,constructor:x,init:function(e,t,n){var r,i;if(!e)return this;if("string"==typeof e){if(r="<"===e.charAt(0)&&">"===e.charAt(e.length-1)&&e.length>=3?[null,e,null]:T.exec(e),!r||!r[1]&&t)return!t||t.jquery?(t||n).find(e):this.constructor(t).find(e);if(r[1]){if(t=t instanceof x?t[0]:t,x.merge(this,x.parseHTML(r[1],t&&t.nodeType?t.ownerDocument||t:o,!0)),C.test(r[1])&&x.isPlainObject(t))for(r in t)x.isFunction(this[r])?this[r](t[r]):this.attr(r,t[r]);return this}return i=o.getElementById(r[2]),i&&i.parentNode&&(this.length=1,this[0]=i),this.context=o,this.selector=e,this}return e.nodeType?(this.context=this[0]=e,this.length=1,this):x.isFunction(e)?n.ready(e):(e.selector!==undefined&&(this.selector=e.selector,this.context=e.context),x.makeArray(e,this))},selector:"",length:0,toArray:function(){return d.call(this)},get:function(e){return null==e?this.toArray():0>e?this[this.length+e]:this[e]},pushStack:function(e){var t=x.merge(this.constructor(),e);return t.prevObject=this,t.context=this.context,t},each:function(e,t){return x.each(this,e,t)},ready:function(e){return x.ready.promise().done(e),this},slice:function(){return this.pushStack(d.apply(this,arguments))},first:function(){return this.eq(0)},last:function(){return this.eq(-1)},eq:function(e){var t=this.length,n=+e+(0>e?t:0);return this.pushStack(n>=0&&t>n?[this[n]]:[])},map:function(e){return this.pushStack(x.map(this,function(t,n){return e.call(t,n,t)}))},end:function(){return this.prevObject||this.constructor(null)},push:h,sort:[].sort,splice:[].splice},x.fn.init.prototype=x.fn,x.extend=x.fn.extend=function(){var e,t,n,r,i,o,s=arguments[0]||{},a=1,u=arguments.length,l=!1;for("boolean"==typeof s&&(l=s,s=arguments[1]||{},a=2),"object"==typeof s||x.isFunction(s)||(s={}),u===a&&(s=this,--a);u>a;a++)if(null!=(e=arguments[a]))for(t in e)n=s[t],r=e[t],s!==r&&(l&&r&&(x.isPlainObject(r)||(i=x.isArray(r)))?(i?(i=!1,o=n&&x.isArray(n)?n:[]):o=n&&x.isPlainObject(n)?n:{},s[t]=x.extend(l,o,r)):r!==undefined&&(s[t]=r));return s},x.extend({expando:"jQuery"+(p+Math.random()).replace(/\D/g,""),noConflict:function(t){return e.$===x&&(e.$=u),t&&e.jQuery===x&&(e.jQuery=a),x},isReady:!1,readyWait:1,holdReady:function(e){e?x.readyWait++:x.ready(!0)},ready:function(e){(e===!0?--x.readyWait:x.isReady)||(x.isReady=!0,e!==!0&&--x.readyWait>0||(n.resolveWith(o,[x]),x.fn.trigger&&x(o).trigger("ready").off("ready")))},isFunction:function(e){return"function"===x.type(e)},isArray:Array.isArray,isWindow:function(e){return null!=e&&e===e.window},isNumeric:function(e){return!isNaN(parseFloat(e))&&isFinite(e)},type:function(e){return null==e?e+"":"object"==typeof e||"function"==typeof e?l[m.call(e)]||"object":typeof e},isPlainObject:function(e){if("object"!==x.type(e)||e.nodeType||x.isWindow(e))return!1;try{if(e.constructor&&!y.call(e.constructor.prototype,"isPrototypeOf"))return!1}catch(t){return!1}return!0},isEmptyObject:function(e){var t;for(t in e)return!1;return!0},error:function(e){throw Error(e)},parseHTML:function(e,t,n){if(!e||"string"!=typeof e)return null;"boolean"==typeof t&&(n=t,t=!1),t=t||o;var r=C.exec(e),i=!n&&[];return r?[t.createElement(r[1])]:(r=x.buildFragment([e],t,i),i&&x(i).remove(),x.merge([],r.childNodes))},parseJSON:JSON.parse,parseXML:function(e){var t,n;if(!e||"string"!=typeof e)return null;try{n=new DOMParser,t=n.parseFromString(e,"text/xml")}catch(r){t=undefined}return(!t||t.getElementsByTagName("parsererror").length)&&x.error("Invalid XML: "+e),t},noop:function(){},globalEval:function(e){var t,n=eval;e=x.trim(e),e&&(1===e.indexOf("use strict")?(t=o.createElement("script"),t.text=e,o.head.appendChild(t).parentNode.removeChild(t)):n(e))},camelCase:function(e){return e.replace(k,"ms-").replace(N,E)},nodeName:function(e,t){return e.nodeName&&e.nodeName.toLowerCase()===t.toLowerCase()},each:function(e,t,n){var r,i=0,o=e.length,s=j(e);if(n){if(s){for(;o>i;i++)if(r=t.apply(e[i],n),r===!1)break}else for(i in e)if(r=t.apply(e[i],n),r===!1)break}else if(s){for(;o>i;i++)if(r=t.call(e[i],i,e[i]),r===!1)break}else for(i in e)if(r=t.call(e[i],i,e[i]),r===!1)break;return e},trim:function(e){return null==e?"":v.call(e)},makeArray:function(e,t){var n=t||[];return null!=e&&(j(Object(e))?x.merge(n,"string"==typeof e?[e]:e):h.call(n,e)),n},inArray:function(e,t,n){return null==t?-1:g.call(t,e,n)},merge:function(e,t){var n=t.length,r=e.length,i=0;if("number"==typeof n)for(;n>i;i++)e[r++]=t[i];else while(t[i]!==undefined)e[r++]=t[i++];return e.length=r,e},grep:function(e,t,n){var r,i=[],o=0,s=e.length;for(n=!!n;s>o;o++)r=!!t(e[o],o),n!==r&&i.push(e[o]);return i},map:function(e,t,n){var r,i=0,o=e.length,s=j(e),a=[];if(s)for(;o>i;i++)r=t(e[i],i,n),null!=r&&(a[a.length]=r);else for(i in e)r=t(e[i],i,n),null!=r&&(a[a.length]=r);return f.apply([],a)},guid:1,proxy:function(e,t){var n,r,i;return"string"==typeof t&&(n=e[t],t=e,e=n),x.isFunction(e)?(r=d.call(arguments,2),i=function(){return e.apply(t||this,r.concat(d.call(arguments)))},i.guid=e.guid=e.guid||x.guid++,i):undefined},access:function(e,t,n,r,i,o,s){var a=0,u=e.length,l=null==n;if("object"===x.type(n)){i=!0;for(a in n)x.access(e,t,a,n[a],!0,o,s)}else if(r!==undefined&&(i=!0,x.isFunction(r)||(s=!0),l&&(s?(t.call(e,r),t=null):(l=t,t=function(e,t,n){return l.call(x(e),n)})),t))for(;u>a;a++)t(e[a],n,s?r:r.call(e[a],a,t(e[a],n)));return i?e:l?t.call(e):u?t(e[0],n):o},now:Date.now,swap:function(e,t,n,r){var i,o,s={};for(o in t)s[o]=e.style[o],e.style[o]=t[o];i=n.apply(e,r||[]);for(o in t)e.style[o]=s[o];return i}}),x.ready.promise=function(t){return n||(n=x.Deferred(),"complete"===o.readyState?setTimeout(x.ready):(o.addEventListener("DOMContentLoaded",S,!1),e.addEventListener("load",S,!1))),n.promise(t)},x.each("Boolean Number String Function Array Date RegExp Object Error".split(" "),function(e,t){l["[object "+t+"]"]=t.toLowerCase()});function j(e){var t=e.length,n=x.type(e);return x.isWindow(e)?!1:1===e.nodeType&&t?!0:"array"===n||"function"!==n&&(0===t||"number"==typeof t&&t>0&&t-1 in e)}t=x(o),function(e,undefined){var t,n,r,i,o,s,a,u,l,c,p,f,h,d,g,m,y,v="sizzle"+-new Date,b=e.document,w=0,T=0,C=st(),k=st(),N=st(),E=!1,S=function(e,t){return e===t?(E=!0,0):0},j=typeof undefined,D=1<<31,A={}.hasOwnProperty,L=[],q=L.pop,H=L.push,O=L.push,F=L.slice,P=L.indexOf||function(e){var t=0,n=this.length;for(;n>t;t++)if(this[t]===e)return t;return-1},R="checked|selected|async|autofocus|autoplay|controls|defer|disabled|hidden|ismap|loop|multiple|open|readonly|required|scoped",M="[\\x20\\t\\r\\n\\f]",W="(?:\\\\.|[\\w-]|[^\\x00-\\xa0])+",$=W.replace("w","w#"),B="\\["+M+"*("+W+")"+M+"*(?:([*^$|!~]?=)"+M+"*(?:(['\"])((?:\\\\.|[^\\\\])*?)\\3|("+$+")|)|)"+M+"*\\]",I=":("+W+")(?:\\(((['\"])((?:\\\\.|[^\\\\])*?)\\3|((?:\\\\.|[^\\\\()[\\]]|"+B.replace(3,8)+")*)|.*)\\)|)",z=RegExp("^"+M+"+|((?:^|[^\\\\])(?:\\\\.)*)"+M+"+$","g"),_=RegExp("^"+M+"*,"+M+"*"),X=RegExp("^"+M+"*([>+~]|"+M+")"+M+"*"),U=RegExp(M+"*[+~]"),Y=RegExp("="+M+"*([^\\]'\"]*)"+M+"*\\]","g"),V=RegExp(I),G=RegExp("^"+$+"$"),J={ID:RegExp("^#("+W+")"),CLASS:RegExp("^\\.("+W+")"),TAG:RegExp("^("+W.replace("w","w*")+")"),ATTR:RegExp("^"+B),PSEUDO:RegExp("^"+I),CHILD:RegExp("^:(only|first|last|nth|nth-last)-(child|of-type)(?:\\("+M+"*(even|odd|(([+-]|)(\\d*)n|)"+M+"*(?:([+-]|)"+M+"*(\\d+)|))"+M+"*\\)|)","i"),bool:RegExp("^(?:"+R+")$","i"),needsContext:RegExp("^"+M+"*[>+~]|:(even|odd|eq|gt|lt|nth|first|last)(?:\\("+M+"*((?:-\\d)?\\d*)"+M+"*\\)|)(?=[^-]|$)","i")},Q=/^[^{]+\{\s*\[native \w/,K=/^(?:#([\w-]+)|(\w+)|\.([\w-]+))$/,Z=/^(?:input|select|textarea|button)$/i,et=/^h\d$/i,tt=/'|\\/g,nt=RegExp("\\\\([\\da-f]{1,6}"+M+"?|("+M+")|.)","ig"),rt=function(e,t,n){var r="0x"+t-65536;return r!==r||n?t:0>r?String.fromCharCode(r+65536):String.fromCharCode(55296|r>>10,56320|1023&r)};try{O.apply(L=F.call(b.childNodes),b.childNodes),L[b.childNodes.length].nodeType}catch(it){O={apply:L.length?function(e,t){H.apply(e,F.call(t))}:function(e,t){var n=e.length,r=0;while(e[n++]=t[r++]);e.length=n-1}}}function ot(e,t,r,i){var o,s,a,u,l,f,g,m,x,w;if((t?t.ownerDocument||t:b)!==p&&c(t),t=t||p,r=r||[],!e||"string"!=typeof e)return r;if(1!==(u=t.nodeType)&&9!==u)return[];if(h&&!i){if(o=K.exec(e))if(a=o[1]){if(9===u){if(s=t.getElementById(a),!s||!s.parentNode)return r;if(s.id===a)return r.push(s),r}else if(t.ownerDocument&&(s=t.ownerDocument.getElementById(a))&&y(t,s)&&s.id===a)return r.push(s),r}else{if(o[2])return O.apply(r,t.getElementsByTagName(e)),r;if((a=o[3])&&n.getElementsByClassName&&t.getElementsByClassName)return O.apply(r,t.getElementsByClassName(a)),r}if(n.qsa&&(!d||!d.test(e))){if(m=g=v,x=t,w=9===u&&e,1===u&&"object"!==t.nodeName.toLowerCase()){f=gt(e),(g=t.getAttribute("id"))?m=g.replace(tt,"\\$&"):t.setAttribute("id",m),m="[id='"+m+"'] ",l=f.length;while(l--)f[l]=m+mt(f[l]);x=U.test(e)&&t.parentNode||t,w=f.join(",")}if(w)try{return O.apply(r,x.querySelectorAll(w)),r}catch(T){}finally{g||t.removeAttribute("id")}}}return kt(e.replace(z,"$1"),t,r,i)}function st(){var e=[];function t(n,r){return e.push(n+=" ")>i.cacheLength&&delete t[e.shift()],t[n]=r}return t}function at(e){return e[v]=!0,e}function ut(e){var t=p.createElement("div");try{return!!e(t)}catch(n){return!1}finally{t.parentNode&&t.parentNode.removeChild(t),t=null}}function lt(e,t){var n=e.split("|"),r=e.length;while(r--)i.attrHandle[n[r]]=t}function ct(e,t){var n=t&&e,r=n&&1===e.nodeType&&1===t.nodeType&&(~t.sourceIndex||D)-(~e.sourceIndex||D);if(r)return r;if(n)while(n=n.nextSibling)if(n===t)return-1;return e?1:-1}function pt(e){return function(t){var n=t.nodeName.toLowerCase();return"input"===n&&t.type===e}}function ft(e){return function(t){var n=t.nodeName.toLowerCase();return("input"===n||"button"===n)&&t.type===e}}function ht(e){return at(function(t){return t=+t,at(function(n,r){var i,o=e([],n.length,t),s=o.length;while(s--)n[i=o[s]]&&(n[i]=!(r[i]=n[i]))})})}s=ot.isXML=function(e){var t=e&&(e.ownerDocument||e).documentElement;return t?"HTML"!==t.nodeName:!1},n=ot.support={},c=ot.setDocument=function(e){var t=e?e.ownerDocument||e:b,r=t.defaultView;return t!==p&&9===t.nodeType&&t.documentElement?(p=t,f=t.documentElement,h=!s(t),r&&r.attachEvent&&r!==r.top&&r.attachEvent("onbeforeunload",function(){c()}),n.attributes=ut(function(e){return e.className="i",!e.getAttribute("className")}),n.getElementsByTagName=ut(function(e){return e.appendChild(t.createComment("")),!e.getElementsByTagName("*").length}),n.getElementsByClassName=ut(function(e){return e.innerHTML="<div class='a'></div><div class='a i'></div>",e.firstChild.className="i",2===e.getElementsByClassName("i").length}),n.getById=ut(function(e){return f.appendChild(e).id=v,!t.getElementsByName||!t.getElementsByName(v).length}),n.getById?(i.find.ID=function(e,t){if(typeof t.getElementById!==j&&h){var n=t.getElementById(e);return n&&n.parentNode?[n]:[]}},i.filter.ID=function(e){var t=e.replace(nt,rt);return function(e){return e.getAttribute("id")===t}}):(delete i.find.ID,i.filter.ID=function(e){var t=e.replace(nt,rt);return function(e){var n=typeof e.getAttributeNode!==j&&e.getAttributeNode("id");return n&&n.value===t}}),i.find.TAG=n.getElementsByTagName?function(e,t){return typeof t.getElementsByTagName!==j?t.getElementsByTagName(e):undefined}:function(e,t){var n,r=[],i=0,o=t.getElementsByTagName(e);if("*"===e){while(n=o[i++])1===n.nodeType&&r.push(n);return r}return o},i.find.CLASS=n.getElementsByClassName&&function(e,t){return typeof t.getElementsByClassName!==j&&h?t.getElementsByClassName(e):undefined},g=[],d=[],(n.qsa=Q.test(t.querySelectorAll))&&(ut(function(e){e.innerHTML="<select><option selected=''></option></select>",e.querySelectorAll("[selected]").length||d.push("\\["+M+"*(?:value|"+R+")"),e.querySelectorAll(":checked").length||d.push(":checked")}),ut(function(e){var n=t.createElement("input");n.setAttribute("type","hidden"),e.appendChild(n).setAttribute("t",""),e.querySelectorAll("[t^='']").length&&d.push("[*^$]="+M+"*(?:''|\"\")"),e.querySelectorAll(":enabled").length||d.push(":enabled",":disabled"),e.querySelectorAll("*,:x"),d.push(",.*:")})),(n.matchesSelector=Q.test(m=f.webkitMatchesSelector||f.mozMatchesSelector||f.oMatchesSelector||f.msMatchesSelector))&&ut(function(e){n.disconnectedMatch=m.call(e,"div"),m.call(e,"[s!='']:x"),g.push("!=",I)}),d=d.length&&RegExp(d.join("|")),g=g.length&&RegExp(g.join("|")),y=Q.test(f.contains)||f.compareDocumentPosition?function(e,t){var n=9===e.nodeType?e.documentElement:e,r=t&&t.parentNode;return e===r||!(!r||1!==r.nodeType||!(n.contains?n.contains(r):e.compareDocumentPosition&&16&e.compareDocumentPosition(r)))}:function(e,t){if(t)while(t=t.parentNode)if(t===e)return!0;return!1},S=f.compareDocumentPosition?function(e,r){if(e===r)return E=!0,0;var i=r.compareDocumentPosition&&e.compareDocumentPosition&&e.compareDocumentPosition(r);return i?1&i||!n.sortDetached&&r.compareDocumentPosition(e)===i?e===t||y(b,e)?-1:r===t||y(b,r)?1:l?P.call(l,e)-P.call(l,r):0:4&i?-1:1:e.compareDocumentPosition?-1:1}:function(e,n){var r,i=0,o=e.parentNode,s=n.parentNode,a=[e],u=[n];if(e===n)return E=!0,0;if(!o||!s)return e===t?-1:n===t?1:o?-1:s?1:l?P.call(l,e)-P.call(l,n):0;if(o===s)return ct(e,n);r=e;while(r=r.parentNode)a.unshift(r);r=n;while(r=r.parentNode)u.unshift(r);while(a[i]===u[i])i++;return i?ct(a[i],u[i]):a[i]===b?-1:u[i]===b?1:0},t):p},ot.matches=function(e,t){return ot(e,null,null,t)},ot.matchesSelector=function(e,t){if((e.ownerDocument||e)!==p&&c(e),t=t.replace(Y,"='$1']"),!(!n.matchesSelector||!h||g&&g.test(t)||d&&d.test(t)))try{var r=m.call(e,t);if(r||n.disconnectedMatch||e.document&&11!==e.document.nodeType)return r}catch(i){}return ot(t,p,null,[e]).length>0},ot.contains=function(e,t){return(e.ownerDocument||e)!==p&&c(e),y(e,t)},ot.attr=function(e,t){(e.ownerDocument||e)!==p&&c(e);var r=i.attrHandle[t.toLowerCase()],o=r&&A.call(i.attrHandle,t.toLowerCase())?r(e,t,!h):undefined;return o===undefined?n.attributes||!h?e.getAttribute(t):(o=e.getAttributeNode(t))&&o.specified?o.value:null:o},ot.error=function(e){throw Error("Syntax error, unrecognized expression: "+e)},ot.uniqueSort=function(e){var t,r=[],i=0,o=0;if(E=!n.detectDuplicates,l=!n.sortStable&&e.slice(0),e.sort(S),E){while(t=e[o++])t===e[o]&&(i=r.push(o));while(i--)e.splice(r[i],1)}return e},o=ot.getText=function(e){var t,n="",r=0,i=e.nodeType;if(i){if(1===i||9===i||11===i){if("string"==typeof e.textContent)return e.textContent;for(e=e.firstChild;e;e=e.nextSibling)n+=o(e)}else if(3===i||4===i)return e.nodeValue}else for(;t=e[r];r++)n+=o(t);return n},i=ot.selectors={cacheLength:50,createPseudo:at,match:J,attrHandle:{},find:{},relative:{">":{dir:"parentNode",first:!0}," ":{dir:"parentNode"},"+":{dir:"previousSibling",first:!0},"~":{dir:"previousSibling"}},preFilter:{ATTR:function(e){return e[1]=e[1].replace(nt,rt),e[3]=(e[4]||e[5]||"").replace(nt,rt),"~="===e[2]&&(e[3]=" "+e[3]+" "),e.slice(0,4)},CHILD:function(e){return e[1]=e[1].toLowerCase(),"nth"===e[1].slice(0,3)?(e[3]||ot.error(e[0]),e[4]=+(e[4]?e[5]+(e[6]||1):2*("even"===e[3]||"odd"===e[3])),e[5]=+(e[7]+e[8]||"odd"===e[3])):e[3]&&ot.error(e[0]),e},PSEUDO:function(e){var t,n=!e[5]&&e[2];return J.CHILD.test(e[0])?null:(e[3]&&e[4]!==undefined?e[2]=e[4]:n&&V.test(n)&&(t=gt(n,!0))&&(t=n.indexOf(")",n.length-t)-n.length)&&(e[0]=e[0].slice(0,t),e[2]=n.slice(0,t)),e.slice(0,3))}},filter:{TAG:function(e){var t=e.replace(nt,rt).toLowerCase();return"*"===e?function(){return!0}:function(e){return e.nodeName&&e.nodeName.toLowerCase()===t}},CLASS:function(e){var t=C[e+" "];return t||(t=RegExp("(^|"+M+")"+e+"("+M+"|$)"))&&C(e,function(e){return t.test("string"==typeof e.className&&e.className||typeof e.getAttribute!==j&&e.getAttribute("class")||"")})},ATTR:function(e,t,n){return function(r){var i=ot.attr(r,e);return null==i?"!="===t:t?(i+="","="===t?i===n:"!="===t?i!==n:"^="===t?n&&0===i.indexOf(n):"*="===t?n&&i.indexOf(n)>-1:"$="===t?n&&i.slice(-n.length)===n:"~="===t?(" "+i+" ").indexOf(n)>-1:"|="===t?i===n||i.slice(0,n.length+1)===n+"-":!1):!0}},CHILD:function(e,t,n,r,i){var o="nth"!==e.slice(0,3),s="last"!==e.slice(-4),a="of-type"===t;return 1===r&&0===i?function(e){return!!e.parentNode}:function(t,n,u){var l,c,p,f,h,d,g=o!==s?"nextSibling":"previousSibling",m=t.parentNode,y=a&&t.nodeName.toLowerCase(),x=!u&&!a;if(m){if(o){while(g){p=t;while(p=p[g])if(a?p.nodeName.toLowerCase()===y:1===p.nodeType)return!1;d=g="only"===e&&!d&&"nextSibling"}return!0}if(d=[s?m.firstChild:m.lastChild],s&&x){c=m[v]||(m[v]={}),l=c[e]||[],h=l[0]===w&&l[1],f=l[0]===w&&l[2],p=h&&m.childNodes[h];while(p=++h&&p&&p[g]||(f=h=0)||d.pop())if(1===p.nodeType&&++f&&p===t){c[e]=[w,h,f];break}}else if(x&&(l=(t[v]||(t[v]={}))[e])&&l[0]===w)f=l[1];else while(p=++h&&p&&p[g]||(f=h=0)||d.pop())if((a?p.nodeName.toLowerCase()===y:1===p.nodeType)&&++f&&(x&&((p[v]||(p[v]={}))[e]=[w,f]),p===t))break;return f-=i,f===r||0===f%r&&f/r>=0}}},PSEUDO:function(e,t){var n,r=i.pseudos[e]||i.setFilters[e.toLowerCase()]||ot.error("unsupported pseudo: "+e);return r[v]?r(t):r.length>1?(n=[e,e,"",t],i.setFilters.hasOwnProperty(e.toLowerCase())?at(function(e,n){var i,o=r(e,t),s=o.length;while(s--)i=P.call(e,o[s]),e[i]=!(n[i]=o[s])}):function(e){return r(e,0,n)}):r}},pseudos:{not:at(function(e){var t=[],n=[],r=a(e.replace(z,"$1"));return r[v]?at(function(e,t,n,i){var o,s=r(e,null,i,[]),a=e.length;while(a--)(o=s[a])&&(e[a]=!(t[a]=o))}):function(e,i,o){return t[0]=e,r(t,null,o,n),!n.pop()}}),has:at(function(e){return function(t){return ot(e,t).length>0}}),contains:at(function(e){return function(t){return(t.textContent||t.innerText||o(t)).indexOf(e)>-1}}),lang:at(function(e){return G.test(e||"")||ot.error("unsupported lang: "+e),e=e.replace(nt,rt).toLowerCase(),function(t){var n;do if(n=h?t.lang:t.getAttribute("xml:lang")||t.getAttribute("lang"))return n=n.toLowerCase(),n===e||0===n.indexOf(e+"-");while((t=t.parentNode)&&1===t.nodeType);return!1}}),target:function(t){var n=e.location&&e.location.hash;return n&&n.slice(1)===t.id},root:function(e){return e===f},focus:function(e){return e===p.activeElement&&(!p.hasFocus||p.hasFocus())&&!!(e.type||e.href||~e.tabIndex)},enabled:function(e){return e.disabled===!1},disabled:function(e){return e.disabled===!0},checked:function(e){var t=e.nodeName.toLowerCase();return"input"===t&&!!e.checked||"option"===t&&!!e.selected},selected:function(e){return e.parentNode&&e.parentNode.selectedIndex,e.selected===!0},empty:function(e){for(e=e.firstChild;e;e=e.nextSibling)if(e.nodeName>"@"||3===e.nodeType||4===e.nodeType)return!1;return!0},parent:function(e){return!i.pseudos.empty(e)},header:function(e){return et.test(e.nodeName)},input:function(e){return Z.test(e.nodeName)},button:function(e){var t=e.nodeName.toLowerCase();return"input"===t&&"button"===e.type||"button"===t},text:function(e){var t;return"input"===e.nodeName.toLowerCase()&&"text"===e.type&&(null==(t=e.getAttribute("type"))||t.toLowerCase()===e.type)},first:ht(function(){return[0]}),last:ht(function(e,t){return[t-1]}),eq:ht(function(e,t,n){return[0>n?n+t:n]}),even:ht(function(e,t){var n=0;for(;t>n;n+=2)e.push(n);return e}),odd:ht(function(e,t){var n=1;for(;t>n;n+=2)e.push(n);return e}),lt:ht(function(e,t,n){var r=0>n?n+t:n;for(;--r>=0;)e.push(r);return e}),gt:ht(function(e,t,n){var r=0>n?n+t:n;for(;t>++r;)e.push(r);return e})}},i.pseudos.nth=i.pseudos.eq;for(t in{radio:!0,checkbox:!0,file:!0,password:!0,image:!0})i.pseudos[t]=pt(t);for(t in{submit:!0,reset:!0})i.pseudos[t]=ft(t);function dt(){}dt.prototype=i.filters=i.pseudos,i.setFilters=new dt;function gt(e,t){var n,r,o,s,a,u,l,c=k[e+" "];if(c)return t?0:c.slice(0);a=e,u=[],l=i.preFilter;while(a){(!n||(r=_.exec(a)))&&(r&&(a=a.slice(r[0].length)||a),u.push(o=[])),n=!1,(r=X.exec(a))&&(n=r.shift(),o.push({value:n,type:r[0].replace(z," ")}),a=a.slice(n.length));for(s in i.filter)!(r=J[s].exec(a))||l[s]&&!(r=l[s](r))||(n=r.shift(),o.push({value:n,type:s,matches:r}),a=a.slice(n.length));if(!n)break}return t?a.length:a?ot.error(e):k(e,u).slice(0)}function mt(e){var t=0,n=e.length,r="";for(;n>t;t++)r+=e[t].value;return r}function yt(e,t,n){var i=t.dir,o=n&&"parentNode"===i,s=T++;return t.first?function(t,n,r){while(t=t[i])if(1===t.nodeType||o)return e(t,n,r)}:function(t,n,a){var u,l,c,p=w+" "+s;if(a){while(t=t[i])if((1===t.nodeType||o)&&e(t,n,a))return!0}else while(t=t[i])if(1===t.nodeType||o)if(c=t[v]||(t[v]={}),(l=c[i])&&l[0]===p){if((u=l[1])===!0||u===r)return u===!0}else if(l=c[i]=[p],l[1]=e(t,n,a)||r,l[1]===!0)return!0}}function vt(e){return e.length>1?function(t,n,r){var i=e.length;while(i--)if(!e[i](t,n,r))return!1;return!0}:e[0]}function xt(e,t,n,r,i){var o,s=[],a=0,u=e.length,l=null!=t;for(;u>a;a++)(o=e[a])&&(!n||n(o,r,i))&&(s.push(o),l&&t.push(a));return s}function bt(e,t,n,r,i,o){return r&&!r[v]&&(r=bt(r)),i&&!i[v]&&(i=bt(i,o)),at(function(o,s,a,u){var l,c,p,f=[],h=[],d=s.length,g=o||Ct(t||"*",a.nodeType?[a]:a,[]),m=!e||!o&&t?g:xt(g,f,e,a,u),y=n?i||(o?e:d||r)?[]:s:m;if(n&&n(m,y,a,u),r){l=xt(y,h),r(l,[],a,u),c=l.length;while(c--)(p=l[c])&&(y[h[c]]=!(m[h[c]]=p))}if(o){if(i||e){if(i){l=[],c=y.length;while(c--)(p=y[c])&&l.push(m[c]=p);i(null,y=[],l,u)}c=y.length;while(c--)(p=y[c])&&(l=i?P.call(o,p):f[c])>-1&&(o[l]=!(s[l]=p))}}else y=xt(y===s?y.splice(d,y.length):y),i?i(null,s,y,u):O.apply(s,y)})}function wt(e){var t,n,r,o=e.length,s=i.relative[e[0].type],a=s||i.relative[" "],l=s?1:0,c=yt(function(e){return e===t},a,!0),p=yt(function(e){return P.call(t,e)>-1},a,!0),f=[function(e,n,r){return!s&&(r||n!==u)||((t=n).nodeType?c(e,n,r):p(e,n,r))}];for(;o>l;l++)if(n=i.relative[e[l].type])f=[yt(vt(f),n)];else{if(n=i.filter[e[l].type].apply(null,e[l].matches),n[v]){for(r=++l;o>r;r++)if(i.relative[e[r].type])break;return bt(l>1&&vt(f),l>1&&mt(e.slice(0,l-1).concat({value:" "===e[l-2].type?"*":""})).replace(z,"$1"),n,r>l&&wt(e.slice(l,r)),o>r&&wt(e=e.slice(r)),o>r&&mt(e))}f.push(n)}return vt(f)}function Tt(e,t){var n=0,o=t.length>0,s=e.length>0,a=function(a,l,c,f,h){var d,g,m,y=[],v=0,x="0",b=a&&[],T=null!=h,C=u,k=a||s&&i.find.TAG("*",h&&l.parentNode||l),N=w+=null==C?1:Math.random()||.1;for(T&&(u=l!==p&&l,r=n);null!=(d=k[x]);x++){if(s&&d){g=0;while(m=e[g++])if(m(d,l,c)){f.push(d);break}T&&(w=N,r=++n)}o&&((d=!m&&d)&&v--,a&&b.push(d))}if(v+=x,o&&x!==v){g=0;while(m=t[g++])m(b,y,l,c);if(a){if(v>0)while(x--)b[x]||y[x]||(y[x]=q.call(f));y=xt(y)}O.apply(f,y),T&&!a&&y.length>0&&v+t.length>1&&ot.uniqueSort(f)}return T&&(w=N,u=C),b};return o?at(a):a}a=ot.compile=function(e,t){var n,r=[],i=[],o=N[e+" "];if(!o){t||(t=gt(e)),n=t.length;while(n--)o=wt(t[n]),o[v]?r.push(o):i.push(o);o=N(e,Tt(i,r))}return o};function Ct(e,t,n){var r=0,i=t.length;for(;i>r;r++)ot(e,t[r],n);return n}function kt(e,t,r,o){var s,u,l,c,p,f=gt(e);if(!o&&1===f.length){if(u=f[0]=f[0].slice(0),u.length>2&&"ID"===(l=u[0]).type&&n.getById&&9===t.nodeType&&h&&i.relative[u[1].type]){if(t=(i.find.ID(l.matches[0].replace(nt,rt),t)||[])[0],!t)return r;e=e.slice(u.shift().value.length)}s=J.needsContext.test(e)?0:u.length;while(s--){if(l=u[s],i.relative[c=l.type])break;if((p=i.find[c])&&(o=p(l.matches[0].replace(nt,rt),U.test(u[0].type)&&t.parentNode||t))){if(u.splice(s,1),e=o.length&&mt(u),!e)return O.apply(r,o),r;break}}}return a(e,f)(o,t,!h,r,U.test(e)),r}n.sortStable=v.split("").sort(S).join("")===v,n.detectDuplicates=E,c(),n.sortDetached=ut(function(e){return 1&e.compareDocumentPosition(p.createElement("div"))}),ut(function(e){return e.innerHTML="<a href='#'></a>","#"===e.firstChild.getAttribute("href")})||lt("type|href|height|width",function(e,t,n){return n?undefined:e.getAttribute(t,"type"===t.toLowerCase()?1:2)}),n.attributes&&ut(function(e){return e.innerHTML="<input/>",e.firstChild.setAttribute("value",""),""===e.firstChild.getAttribute("value")})||lt("value",function(e,t,n){return n||"input"!==e.nodeName.toLowerCase()?undefined:e.defaultValue}),ut(function(e){return null==e.getAttribute("disabled")})||lt(R,function(e,t,n){var r;return n?undefined:(r=e.getAttributeNode(t))&&r.specified?r.value:e[t]===!0?t.toLowerCase():null}),x.find=ot,x.expr=ot.selectors,x.expr[":"]=x.expr.pseudos,x.unique=ot.uniqueSort,x.text=ot.getText,x.isXMLDoc=ot.isXML,x.contains=ot.contains}(e);var D={};function A(e){var t=D[e]={};return x.each(e.match(w)||[],function(e,n){t[n]=!0}),t}x.Callbacks=function(e){e="string"==typeof e?D[e]||A(e):x.extend({},e);var t,n,r,i,o,s,a=[],u=!e.once&&[],l=function(p){for(t=e.memory&&p,n=!0,s=i||0,i=0,o=a.length,r=!0;a&&o>s;s++)if(a[s].apply(p[0],p[1])===!1&&e.stopOnFalse){t=!1;break}r=!1,a&&(u?u.length&&l(u.shift()):t?a=[]:c.disable())},c={add:function(){if(a){var n=a.length;(function s(t){x.each(t,function(t,n){var r=x.type(n);"function"===r?e.unique&&c.has(n)||a.push(n):n&&n.length&&"string"!==r&&s(n)})})(arguments),r?o=a.length:t&&(i=n,l(t))}return this},remove:function(){return a&&x.each(arguments,function(e,t){var n;while((n=x.inArray(t,a,n))>-1)a.splice(n,1),r&&(o>=n&&o--,s>=n&&s--)}),this},has:function(e){return e?x.inArray(e,a)>-1:!(!a||!a.length)},empty:function(){return a=[],o=0,this},disable:function(){return a=u=t=undefined,this},disabled:function(){return!a},lock:function(){return u=undefined,t||c.disable(),this},locked:function(){return!u},fireWith:function(e,t){return!a||n&&!u||(t=t||[],t=[e,t.slice?t.slice():t],r?u.push(t):l(t)),this},fire:function(){return c.fireWith(this,arguments),this},fired:function(){return!!n}};return c},x.extend({Deferred:function(e){var t=[["resolve","done",x.Callbacks("once memory"),"resolved"],["reject","fail",x.Callbacks("once memory"),"rejected"],["notify","progress",x.Callbacks("memory")]],n="pending",r={state:function(){return n},always:function(){return i.done(arguments).fail(arguments),this},then:function(){var e=arguments;return x.Deferred(function(n){x.each(t,function(t,o){var s=o[0],a=x.isFunction(e[t])&&e[t];i[o[1]](function(){var e=a&&a.apply(this,arguments);e&&x.isFunction(e.promise)?e.promise().done(n.resolve).fail(n.reject).progress(n.notify):n[s+"With"](this===r?n.promise():this,a?[e]:arguments)})}),e=null}).promise()},promise:function(e){return null!=e?x.extend(e,r):r}},i={};return r.pipe=r.then,x.each(t,function(e,o){var s=o[2],a=o[3];r[o[1]]=s.add,a&&s.add(function(){n=a},t[1^e][2].disable,t[2][2].lock),i[o[0]]=function(){return i[o[0]+"With"](this===i?r:this,arguments),this},i[o[0]+"With"]=s.fireWith}),r.promise(i),e&&e.call(i,i),i},when:function(e){var t=0,n=d.call(arguments),r=n.length,i=1!==r||e&&x.isFunction(e.promise)?r:0,o=1===i?e:x.Deferred(),s=function(e,t,n){return function(r){t[e]=this,n[e]=arguments.length>1?d.call(arguments):r,n===a?o.notifyWith(t,n):--i||o.resolveWith(t,n)}},a,u,l;if(r>1)for(a=Array(r),u=Array(r),l=Array(r);r>t;t++)n[t]&&x.isFunction(n[t].promise)?n[t].promise().done(s(t,l,n)).fail(o.reject).progress(s(t,u,a)):--i;return i||o.resolveWith(l,n),o.promise()}}),x.support=function(t){var n=o.createElement("input"),r=o.createDocumentFragment(),i=o.createElement("div"),s=o.createElement("select"),a=s.appendChild(o.createElement("option"));return n.type?(n.type="checkbox",t.checkOn=""!==n.value,t.optSelected=a.selected,t.reliableMarginRight=!0,t.boxSizingReliable=!0,t.pixelPosition=!1,n.checked=!0,t.noCloneChecked=n.cloneNode(!0).checked,s.disabled=!0,t.optDisabled=!a.disabled,n=o.createElement("input"),n.value="t",n.type="radio",t.radioValue="t"===n.value,n.setAttribute("checked","t"),n.setAttribute("name","t"),r.appendChild(n),t.checkClone=r.cloneNode(!0).cloneNode(!0).lastChild.checked,t.focusinBubbles="onfocusin"in e,i.style.backgroundClip="content-box",i.cloneNode(!0).style.backgroundClip="",t.clearCloneStyle="content-box"===i.style.backgroundClip,x(function(){var n,r,s="padding:0;margin:0;border:0;display:block;-webkit-box-sizing:content-box;-moz-box-sizing:content-box;box-sizing:content-box",a=o.getElementsByTagName("body")[0];a&&(n=o.createElement("div"),n.style.cssText="border:0;width:0;height:0;position:absolute;top:0;left:-9999px;margin-top:1px",a.appendChild(n).appendChild(i),i.innerHTML="",i.style.cssText="-webkit-box-sizing:border-box;-moz-box-sizing:border-box;box-sizing:border-box;padding:1px;border:1px;display:block;width:4px;margin-top:1%;position:absolute;top:1%",x.swap(a,null!=a.style.zoom?{zoom:1}:{},function(){t.boxSizing=4===i.offsetWidth}),e.getComputedStyle&&(t.pixelPosition="1%"!==(e.getComputedStyle(i,null)||{}).top,t.boxSizingReliable="4px"===(e.getComputedStyle(i,null)||{width:"4px"}).width,r=i.appendChild(o.createElement("div")),r.style.cssText=i.style.cssText=s,r.style.marginRight=r.style.width="0",i.style.width="1px",t.reliableMarginRight=!parseFloat((e.getComputedStyle(r,null)||{}).marginRight)),a.removeChild(n))}),t):t}({});var L,q,H=/(?:\{[\s\S]*\}|\[[\s\S]*\])$/,O=/([A-Z])/g;function F(){Object.defineProperty(this.cache={},0,{get:function(){return{}}}),this.expando=x.expando+Math.random()}F.uid=1,F.accepts=function(e){return e.nodeType?1===e.nodeType||9===e.nodeType:!0},F.prototype={key:function(e){if(!F.accepts(e))return 0;var t={},n=e[this.expando];if(!n){n=F.uid++;try{t[this.expando]={value:n},Object.defineProperties(e,t)}catch(r){t[this.expando]=n,x.extend(e,t)}}return this.cache[n]||(this.cache[n]={}),n},set:function(e,t,n){var r,i=this.key(e),o=this.cache[i];if("string"==typeof t)o[t]=n;else if(x.isEmptyObject(o))x.extend(this.cache[i],t);else for(r in t)o[r]=t[r];return o},get:function(e,t){var n=this.cache[this.key(e)];return t===undefined?n:n[t]},access:function(e,t,n){var r;return t===undefined||t&&"string"==typeof t&&n===undefined?(r=this.get(e,t),r!==undefined?r:this.get(e,x.camelCase(t))):(this.set(e,t,n),n!==undefined?n:t)},remove:function(e,t){var n,r,i,o=this.key(e),s=this.cache[o];if(t===undefined)this.cache[o]={};else{x.isArray(t)?r=t.concat(t.map(x.camelCase)):(i=x.camelCase(t),t in s?r=[t,i]:(r=i,r=r in s?[r]:r.match(w)||[])),n=r.length;while(n--)delete s[r[n]]}},hasData:function(e){return!x.isEmptyObject(this.cache[e[this.expando]]||{})},discard:function(e){e[this.expando]&&delete this.cache[e[this.expando]]}},L=new F,q=new F,x.extend({acceptData:F.accepts,hasData:function(e){return L.hasData(e)||q.hasData(e)},data:function(e,t,n){return L.access(e,t,n)},removeData:function(e,t){L.remove(e,t)},_data:function(e,t,n){return q.access(e,t,n)},_removeData:function(e,t){q.remove(e,t)}}),x.fn.extend({data:function(e,t){var n,r,i=this[0],o=0,s=null;if(e===undefined){if(this.length&&(s=L.get(i),1===i.nodeType&&!q.get(i,"hasDataAttrs"))){for(n=i.attributes;n.length>o;o++)r=n[o].name,0===r.indexOf("data-")&&(r=x.camelCase(r.slice(5)),P(i,r,s[r]));q.set(i,"hasDataAttrs",!0)}return s}return"object"==typeof e?this.each(function(){L.set(this,e)}):x.access(this,function(t){var n,r=x.camelCase(e);if(i&&t===undefined){if(n=L.get(i,e),n!==undefined)return n;if(n=L.get(i,r),n!==undefined)return n;if(n=P(i,r,undefined),n!==undefined)return n}else this.each(function(){var n=L.get(this,r);L.set(this,r,t),-1!==e.indexOf("-")&&n!==undefined&&L.set(this,e,t)})},null,t,arguments.length>1,null,!0)},removeData:function(e){return this.each(function(){L.remove(this,e)})}});function P(e,t,n){var r;if(n===undefined&&1===e.nodeType)if(r="data-"+t.replace(O,"-$1").toLowerCase(),n=e.getAttribute(r),"string"==typeof n){try{n="true"===n?!0:"false"===n?!1:"null"===n?null:+n+""===n?+n:H.test(n)?JSON.parse(n):n}catch(i){}L.set(e,t,n)}else n=undefined;return n}x.extend({queue:function(e,t,n){var r;return e?(t=(t||"fx")+"queue",r=q.get(e,t),n&&(!r||x.isArray(n)?r=q.access(e,t,x.makeArray(n)):r.push(n)),r||[]):undefined},dequeue:function(e,t){t=t||"fx";var n=x.queue(e,t),r=n.length,i=n.shift(),o=x._queueHooks(e,t),s=function(){x.dequeue(e,t)
  };"inprogress"===i&&(i=n.shift(),r--),i&&("fx"===t&&n.unshift("inprogress"),delete o.stop,i.call(e,s,o)),!r&&o&&o.empty.fire()},_queueHooks:function(e,t){var n=t+"queueHooks";return q.get(e,n)||q.access(e,n,{empty:x.Callbacks("once memory").add(function(){q.remove(e,[t+"queue",n])})})}}),x.fn.extend({queue:function(e,t){var n=2;return"string"!=typeof e&&(t=e,e="fx",n--),n>arguments.length?x.queue(this[0],e):t===undefined?this:this.each(function(){var n=x.queue(this,e,t);x._queueHooks(this,e),"fx"===e&&"inprogress"!==n[0]&&x.dequeue(this,e)})},dequeue:function(e){return this.each(function(){x.dequeue(this,e)})},delay:function(e,t){return e=x.fx?x.fx.speeds[e]||e:e,t=t||"fx",this.queue(t,function(t,n){var r=setTimeout(t,e);n.stop=function(){clearTimeout(r)}})},clearQueue:function(e){return this.queue(e||"fx",[])},promise:function(e,t){var n,r=1,i=x.Deferred(),o=this,s=this.length,a=function(){--r||i.resolveWith(o,[o])};"string"!=typeof e&&(t=e,e=undefined),e=e||"fx";while(s--)n=q.get(o[s],e+"queueHooks"),n&&n.empty&&(r++,n.empty.add(a));return a(),i.promise(t)}});var R,M,W=/[\t\r\n\f]/g,$=/\r/g,B=/^(?:input|select|textarea|button)$/i;x.fn.extend({attr:function(e,t){return x.access(this,x.attr,e,t,arguments.length>1)},removeAttr:function(e){return this.each(function(){x.removeAttr(this,e)})},prop:function(e,t){return x.access(this,x.prop,e,t,arguments.length>1)},removeProp:function(e){return this.each(function(){delete this[x.propFix[e]||e]})},addClass:function(e){var t,n,r,i,o,s=0,a=this.length,u="string"==typeof e&&e;if(x.isFunction(e))return this.each(function(t){x(this).addClass(e.call(this,t,this.className))});if(u)for(t=(e||"").match(w)||[];a>s;s++)if(n=this[s],r=1===n.nodeType&&(n.className?(" "+n.className+" ").replace(W," "):" ")){o=0;while(i=t[o++])0>r.indexOf(" "+i+" ")&&(r+=i+" ");n.className=x.trim(r)}return this},removeClass:function(e){var t,n,r,i,o,s=0,a=this.length,u=0===arguments.length||"string"==typeof e&&e;if(x.isFunction(e))return this.each(function(t){x(this).removeClass(e.call(this,t,this.className))});if(u)for(t=(e||"").match(w)||[];a>s;s++)if(n=this[s],r=1===n.nodeType&&(n.className?(" "+n.className+" ").replace(W," "):"")){o=0;while(i=t[o++])while(r.indexOf(" "+i+" ")>=0)r=r.replace(" "+i+" "," ");n.className=e?x.trim(r):""}return this},toggleClass:function(e,t){var n=typeof e;return"boolean"==typeof t&&"string"===n?t?this.addClass(e):this.removeClass(e):x.isFunction(e)?this.each(function(n){x(this).toggleClass(e.call(this,n,this.className,t),t)}):this.each(function(){if("string"===n){var t,i=0,o=x(this),s=e.match(w)||[];while(t=s[i++])o.hasClass(t)?o.removeClass(t):o.addClass(t)}else(n===r||"boolean"===n)&&(this.className&&q.set(this,"__className__",this.className),this.className=this.className||e===!1?"":q.get(this,"__className__")||"")})},hasClass:function(e){var t=" "+e+" ",n=0,r=this.length;for(;r>n;n++)if(1===this[n].nodeType&&(" "+this[n].className+" ").replace(W," ").indexOf(t)>=0)return!0;return!1},val:function(e){var t,n,r,i=this[0];{if(arguments.length)return r=x.isFunction(e),this.each(function(n){var i;1===this.nodeType&&(i=r?e.call(this,n,x(this).val()):e,null==i?i="":"number"==typeof i?i+="":x.isArray(i)&&(i=x.map(i,function(e){return null==e?"":e+""})),t=x.valHooks[this.type]||x.valHooks[this.nodeName.toLowerCase()],t&&"set"in t&&t.set(this,i,"value")!==undefined||(this.value=i))});if(i)return t=x.valHooks[i.type]||x.valHooks[i.nodeName.toLowerCase()],t&&"get"in t&&(n=t.get(i,"value"))!==undefined?n:(n=i.value,"string"==typeof n?n.replace($,""):null==n?"":n)}}}),x.extend({valHooks:{option:{get:function(e){var t=e.attributes.value;return!t||t.specified?e.value:e.text}},select:{get:function(e){var t,n,r=e.options,i=e.selectedIndex,o="select-one"===e.type||0>i,s=o?null:[],a=o?i+1:r.length,u=0>i?a:o?i:0;for(;a>u;u++)if(n=r[u],!(!n.selected&&u!==i||(x.support.optDisabled?n.disabled:null!==n.getAttribute("disabled"))||n.parentNode.disabled&&x.nodeName(n.parentNode,"optgroup"))){if(t=x(n).val(),o)return t;s.push(t)}return s},set:function(e,t){var n,r,i=e.options,o=x.makeArray(t),s=i.length;while(s--)r=i[s],(r.selected=x.inArray(x(r).val(),o)>=0)&&(n=!0);return n||(e.selectedIndex=-1),o}}},attr:function(e,t,n){var i,o,s=e.nodeType;if(e&&3!==s&&8!==s&&2!==s)return typeof e.getAttribute===r?x.prop(e,t,n):(1===s&&x.isXMLDoc(e)||(t=t.toLowerCase(),i=x.attrHooks[t]||(x.expr.match.bool.test(t)?M:R)),n===undefined?i&&"get"in i&&null!==(o=i.get(e,t))?o:(o=x.find.attr(e,t),null==o?undefined:o):null!==n?i&&"set"in i&&(o=i.set(e,n,t))!==undefined?o:(e.setAttribute(t,n+""),n):(x.removeAttr(e,t),undefined))},removeAttr:function(e,t){var n,r,i=0,o=t&&t.match(w);if(o&&1===e.nodeType)while(n=o[i++])r=x.propFix[n]||n,x.expr.match.bool.test(n)&&(e[r]=!1),e.removeAttribute(n)},attrHooks:{type:{set:function(e,t){if(!x.support.radioValue&&"radio"===t&&x.nodeName(e,"input")){var n=e.value;return e.setAttribute("type",t),n&&(e.value=n),t}}}},propFix:{"for":"htmlFor","class":"className"},prop:function(e,t,n){var r,i,o,s=e.nodeType;if(e&&3!==s&&8!==s&&2!==s)return o=1!==s||!x.isXMLDoc(e),o&&(t=x.propFix[t]||t,i=x.propHooks[t]),n!==undefined?i&&"set"in i&&(r=i.set(e,n,t))!==undefined?r:e[t]=n:i&&"get"in i&&null!==(r=i.get(e,t))?r:e[t]},propHooks:{tabIndex:{get:function(e){return e.hasAttribute("tabindex")||B.test(e.nodeName)||e.href?e.tabIndex:-1}}}}),M={set:function(e,t,n){return t===!1?x.removeAttr(e,n):e.setAttribute(n,n),n}},x.each(x.expr.match.bool.source.match(/\w+/g),function(e,t){var n=x.expr.attrHandle[t]||x.find.attr;x.expr.attrHandle[t]=function(e,t,r){var i=x.expr.attrHandle[t],o=r?undefined:(x.expr.attrHandle[t]=undefined)!=n(e,t,r)?t.toLowerCase():null;return x.expr.attrHandle[t]=i,o}}),x.support.optSelected||(x.propHooks.selected={get:function(e){var t=e.parentNode;return t&&t.parentNode&&t.parentNode.selectedIndex,null}}),x.each(["tabIndex","readOnly","maxLength","cellSpacing","cellPadding","rowSpan","colSpan","useMap","frameBorder","contentEditable"],function(){x.propFix[this.toLowerCase()]=this}),x.each(["radio","checkbox"],function(){x.valHooks[this]={set:function(e,t){return x.isArray(t)?e.checked=x.inArray(x(e).val(),t)>=0:undefined}},x.support.checkOn||(x.valHooks[this].get=function(e){return null===e.getAttribute("value")?"on":e.value})});var I=/^key/,z=/^(?:mouse|contextmenu)|click/,_=/^(?:focusinfocus|focusoutblur)$/,X=/^([^.]*)(?:\.(.+)|)$/;function U(){return!0}function Y(){return!1}function V(){try{return o.activeElement}catch(e){}}x.event={global:{},add:function(e,t,n,i,o){var s,a,u,l,c,p,f,h,d,g,m,y=q.get(e);if(y){n.handler&&(s=n,n=s.handler,o=s.selector),n.guid||(n.guid=x.guid++),(l=y.events)||(l=y.events={}),(a=y.handle)||(a=y.handle=function(e){return typeof x===r||e&&x.event.triggered===e.type?undefined:x.event.dispatch.apply(a.elem,arguments)},a.elem=e),t=(t||"").match(w)||[""],c=t.length;while(c--)u=X.exec(t[c])||[],d=m=u[1],g=(u[2]||"").split(".").sort(),d&&(f=x.event.special[d]||{},d=(o?f.delegateType:f.bindType)||d,f=x.event.special[d]||{},p=x.extend({type:d,origType:m,data:i,handler:n,guid:n.guid,selector:o,needsContext:o&&x.expr.match.needsContext.test(o),namespace:g.join(".")},s),(h=l[d])||(h=l[d]=[],h.delegateCount=0,f.setup&&f.setup.call(e,i,g,a)!==!1||e.addEventListener&&e.addEventListener(d,a,!1)),f.add&&(f.add.call(e,p),p.handler.guid||(p.handler.guid=n.guid)),o?h.splice(h.delegateCount++,0,p):h.push(p),x.event.global[d]=!0);e=null}},remove:function(e,t,n,r,i){var o,s,a,u,l,c,p,f,h,d,g,m=q.hasData(e)&&q.get(e);if(m&&(u=m.events)){t=(t||"").match(w)||[""],l=t.length;while(l--)if(a=X.exec(t[l])||[],h=g=a[1],d=(a[2]||"").split(".").sort(),h){p=x.event.special[h]||{},h=(r?p.delegateType:p.bindType)||h,f=u[h]||[],a=a[2]&&RegExp("(^|\\.)"+d.join("\\.(?:.*\\.|)")+"(\\.|$)"),s=o=f.length;while(o--)c=f[o],!i&&g!==c.origType||n&&n.guid!==c.guid||a&&!a.test(c.namespace)||r&&r!==c.selector&&("**"!==r||!c.selector)||(f.splice(o,1),c.selector&&f.delegateCount--,p.remove&&p.remove.call(e,c));s&&!f.length&&(p.teardown&&p.teardown.call(e,d,m.handle)!==!1||x.removeEvent(e,h,m.handle),delete u[h])}else for(h in u)x.event.remove(e,h+t[l],n,r,!0);x.isEmptyObject(u)&&(delete m.handle,q.remove(e,"events"))}},trigger:function(t,n,r,i){var s,a,u,l,c,p,f,h=[r||o],d=y.call(t,"type")?t.type:t,g=y.call(t,"namespace")?t.namespace.split("."):[];if(a=u=r=r||o,3!==r.nodeType&&8!==r.nodeType&&!_.test(d+x.event.triggered)&&(d.indexOf(".")>=0&&(g=d.split("."),d=g.shift(),g.sort()),c=0>d.indexOf(":")&&"on"+d,t=t[x.expando]?t:new x.Event(d,"object"==typeof t&&t),t.isTrigger=i?2:3,t.namespace=g.join("."),t.namespace_re=t.namespace?RegExp("(^|\\.)"+g.join("\\.(?:.*\\.|)")+"(\\.|$)"):null,t.result=undefined,t.target||(t.target=r),n=null==n?[t]:x.makeArray(n,[t]),f=x.event.special[d]||{},i||!f.trigger||f.trigger.apply(r,n)!==!1)){if(!i&&!f.noBubble&&!x.isWindow(r)){for(l=f.delegateType||d,_.test(l+d)||(a=a.parentNode);a;a=a.parentNode)h.push(a),u=a;u===(r.ownerDocument||o)&&h.push(u.defaultView||u.parentWindow||e)}s=0;while((a=h[s++])&&!t.isPropagationStopped())t.type=s>1?l:f.bindType||d,p=(q.get(a,"events")||{})[t.type]&&q.get(a,"handle"),p&&p.apply(a,n),p=c&&a[c],p&&x.acceptData(a)&&p.apply&&p.apply(a,n)===!1&&t.preventDefault();return t.type=d,i||t.isDefaultPrevented()||f._default&&f._default.apply(h.pop(),n)!==!1||!x.acceptData(r)||c&&x.isFunction(r[d])&&!x.isWindow(r)&&(u=r[c],u&&(r[c]=null),x.event.triggered=d,r[d](),x.event.triggered=undefined,u&&(r[c]=u)),t.result}},dispatch:function(e){e=x.event.fix(e);var t,n,r,i,o,s=[],a=d.call(arguments),u=(q.get(this,"events")||{})[e.type]||[],l=x.event.special[e.type]||{};if(a[0]=e,e.delegateTarget=this,!l.preDispatch||l.preDispatch.call(this,e)!==!1){s=x.event.handlers.call(this,e,u),t=0;while((i=s[t++])&&!e.isPropagationStopped()){e.currentTarget=i.elem,n=0;while((o=i.handlers[n++])&&!e.isImmediatePropagationStopped())(!e.namespace_re||e.namespace_re.test(o.namespace))&&(e.handleObj=o,e.data=o.data,r=((x.event.special[o.origType]||{}).handle||o.handler).apply(i.elem,a),r!==undefined&&(e.result=r)===!1&&(e.preventDefault(),e.stopPropagation()))}return l.postDispatch&&l.postDispatch.call(this,e),e.result}},handlers:function(e,t){var n,r,i,o,s=[],a=t.delegateCount,u=e.target;if(a&&u.nodeType&&(!e.button||"click"!==e.type))for(;u!==this;u=u.parentNode||this)if(u.disabled!==!0||"click"!==e.type){for(r=[],n=0;a>n;n++)o=t[n],i=o.selector+" ",r[i]===undefined&&(r[i]=o.needsContext?x(i,this).index(u)>=0:x.find(i,this,null,[u]).length),r[i]&&r.push(o);r.length&&s.push({elem:u,handlers:r})}return t.length>a&&s.push({elem:this,handlers:t.slice(a)}),s},props:"altKey bubbles cancelable ctrlKey currentTarget eventPhase metaKey relatedTarget shiftKey target timeStamp view which".split(" "),fixHooks:{},keyHooks:{props:"char charCode key keyCode".split(" "),filter:function(e,t){return null==e.which&&(e.which=null!=t.charCode?t.charCode:t.keyCode),e}},mouseHooks:{props:"button buttons clientX clientY offsetX offsetY pageX pageY screenX screenY toElement".split(" "),filter:function(e,t){var n,r,i,s=t.button;return null==e.pageX&&null!=t.clientX&&(n=e.target.ownerDocument||o,r=n.documentElement,i=n.body,e.pageX=t.clientX+(r&&r.scrollLeft||i&&i.scrollLeft||0)-(r&&r.clientLeft||i&&i.clientLeft||0),e.pageY=t.clientY+(r&&r.scrollTop||i&&i.scrollTop||0)-(r&&r.clientTop||i&&i.clientTop||0)),e.which||s===undefined||(e.which=1&s?1:2&s?3:4&s?2:0),e}},fix:function(e){if(e[x.expando])return e;var t,n,r,i=e.type,s=e,a=this.fixHooks[i];a||(this.fixHooks[i]=a=z.test(i)?this.mouseHooks:I.test(i)?this.keyHooks:{}),r=a.props?this.props.concat(a.props):this.props,e=new x.Event(s),t=r.length;while(t--)n=r[t],e[n]=s[n];return e.target||(e.target=o),3===e.target.nodeType&&(e.target=e.target.parentNode),a.filter?a.filter(e,s):e},special:{load:{noBubble:!0},focus:{trigger:function(){return this!==V()&&this.focus?(this.focus(),!1):undefined},delegateType:"focusin"},blur:{trigger:function(){return this===V()&&this.blur?(this.blur(),!1):undefined},delegateType:"focusout"},click:{trigger:function(){return"checkbox"===this.type&&this.click&&x.nodeName(this,"input")?(this.click(),!1):undefined},_default:function(e){return x.nodeName(e.target,"a")}},beforeunload:{postDispatch:function(e){e.result!==undefined&&(e.originalEvent.returnValue=e.result)}}},simulate:function(e,t,n,r){var i=x.extend(new x.Event,n,{type:e,isSimulated:!0,originalEvent:{}});r?x.event.trigger(i,null,t):x.event.dispatch.call(t,i),i.isDefaultPrevented()&&n.preventDefault()}},x.removeEvent=function(e,t,n){e.removeEventListener&&e.removeEventListener(t,n,!1)},x.Event=function(e,t){return this instanceof x.Event?(e&&e.type?(this.originalEvent=e,this.type=e.type,this.isDefaultPrevented=e.defaultPrevented||e.getPreventDefault&&e.getPreventDefault()?U:Y):this.type=e,t&&x.extend(this,t),this.timeStamp=e&&e.timeStamp||x.now(),this[x.expando]=!0,undefined):new x.Event(e,t)},x.Event.prototype={isDefaultPrevented:Y,isPropagationStopped:Y,isImmediatePropagationStopped:Y,preventDefault:function(){var e=this.originalEvent;this.isDefaultPrevented=U,e&&e.preventDefault&&e.preventDefault()},stopPropagation:function(){var e=this.originalEvent;this.isPropagationStopped=U,e&&e.stopPropagation&&e.stopPropagation()},stopImmediatePropagation:function(){this.isImmediatePropagationStopped=U,this.stopPropagation()}},x.each({mouseenter:"mouseover",mouseleave:"mouseout"},function(e,t){x.event.special[e]={delegateType:t,bindType:t,handle:function(e){var n,r=this,i=e.relatedTarget,o=e.handleObj;return(!i||i!==r&&!x.contains(r,i))&&(e.type=o.origType,n=o.handler.apply(this,arguments),e.type=t),n}}}),x.support.focusinBubbles||x.each({focus:"focusin",blur:"focusout"},function(e,t){var n=0,r=function(e){x.event.simulate(t,e.target,x.event.fix(e),!0)};x.event.special[t]={setup:function(){0===n++&&o.addEventListener(e,r,!0)},teardown:function(){0===--n&&o.removeEventListener(e,r,!0)}}}),x.fn.extend({on:function(e,t,n,r,i){var o,s;if("object"==typeof e){"string"!=typeof t&&(n=n||t,t=undefined);for(s in e)this.on(s,t,n,e[s],i);return this}if(null==n&&null==r?(r=t,n=t=undefined):null==r&&("string"==typeof t?(r=n,n=undefined):(r=n,n=t,t=undefined)),r===!1)r=Y;else if(!r)return this;return 1===i&&(o=r,r=function(e){return x().off(e),o.apply(this,arguments)},r.guid=o.guid||(o.guid=x.guid++)),this.each(function(){x.event.add(this,e,r,n,t)})},one:function(e,t,n,r){return this.on(e,t,n,r,1)},off:function(e,t,n){var r,i;if(e&&e.preventDefault&&e.handleObj)return r=e.handleObj,x(e.delegateTarget).off(r.namespace?r.origType+"."+r.namespace:r.origType,r.selector,r.handler),this;if("object"==typeof e){for(i in e)this.off(i,t,e[i]);return this}return(t===!1||"function"==typeof t)&&(n=t,t=undefined),n===!1&&(n=Y),this.each(function(){x.event.remove(this,e,n,t)})},trigger:function(e,t){return this.each(function(){x.event.trigger(e,t,this)})},triggerHandler:function(e,t){var n=this[0];return n?x.event.trigger(e,t,n,!0):undefined}});var G=/^.[^:#\[\.,]*$/,J=/^(?:parents|prev(?:Until|All))/,Q=x.expr.match.needsContext,K={children:!0,contents:!0,next:!0,prev:!0};x.fn.extend({find:function(e){var t,n=[],r=this,i=r.length;if("string"!=typeof e)return this.pushStack(x(e).filter(function(){for(t=0;i>t;t++)if(x.contains(r[t],this))return!0}));for(t=0;i>t;t++)x.find(e,r[t],n);return n=this.pushStack(i>1?x.unique(n):n),n.selector=this.selector?this.selector+" "+e:e,n},has:function(e){var t=x(e,this),n=t.length;return this.filter(function(){var e=0;for(;n>e;e++)if(x.contains(this,t[e]))return!0})},not:function(e){return this.pushStack(et(this,e||[],!0))},filter:function(e){return this.pushStack(et(this,e||[],!1))},is:function(e){return!!et(this,"string"==typeof e&&Q.test(e)?x(e):e||[],!1).length},closest:function(e,t){var n,r=0,i=this.length,o=[],s=Q.test(e)||"string"!=typeof e?x(e,t||this.context):0;for(;i>r;r++)for(n=this[r];n&&n!==t;n=n.parentNode)if(11>n.nodeType&&(s?s.index(n)>-1:1===n.nodeType&&x.find.matchesSelector(n,e))){n=o.push(n);break}return this.pushStack(o.length>1?x.unique(o):o)},index:function(e){return e?"string"==typeof e?g.call(x(e),this[0]):g.call(this,e.jquery?e[0]:e):this[0]&&this[0].parentNode?this.first().prevAll().length:-1},add:function(e,t){var n="string"==typeof e?x(e,t):x.makeArray(e&&e.nodeType?[e]:e),r=x.merge(this.get(),n);return this.pushStack(x.unique(r))},addBack:function(e){return this.add(null==e?this.prevObject:this.prevObject.filter(e))}});function Z(e,t){while((e=e[t])&&1!==e.nodeType);return e}x.each({parent:function(e){var t=e.parentNode;return t&&11!==t.nodeType?t:null},parents:function(e){return x.dir(e,"parentNode")},parentsUntil:function(e,t,n){return x.dir(e,"parentNode",n)},next:function(e){return Z(e,"nextSibling")},prev:function(e){return Z(e,"previousSibling")},nextAll:function(e){return x.dir(e,"nextSibling")},prevAll:function(e){return x.dir(e,"previousSibling")},nextUntil:function(e,t,n){return x.dir(e,"nextSibling",n)},prevUntil:function(e,t,n){return x.dir(e,"previousSibling",n)},siblings:function(e){return x.sibling((e.parentNode||{}).firstChild,e)},children:function(e){return x.sibling(e.firstChild)},contents:function(e){return e.contentDocument||x.merge([],e.childNodes)}},function(e,t){x.fn[e]=function(n,r){var i=x.map(this,t,n);return"Until"!==e.slice(-5)&&(r=n),r&&"string"==typeof r&&(i=x.filter(r,i)),this.length>1&&(K[e]||x.unique(i),J.test(e)&&i.reverse()),this.pushStack(i)}}),x.extend({filter:function(e,t,n){var r=t[0];return n&&(e=":not("+e+")"),1===t.length&&1===r.nodeType?x.find.matchesSelector(r,e)?[r]:[]:x.find.matches(e,x.grep(t,function(e){return 1===e.nodeType}))},dir:function(e,t,n){var r=[],i=n!==undefined;while((e=e[t])&&9!==e.nodeType)if(1===e.nodeType){if(i&&x(e).is(n))break;r.push(e)}return r},sibling:function(e,t){var n=[];for(;e;e=e.nextSibling)1===e.nodeType&&e!==t&&n.push(e);return n}});function et(e,t,n){if(x.isFunction(t))return x.grep(e,function(e,r){return!!t.call(e,r,e)!==n});if(t.nodeType)return x.grep(e,function(e){return e===t!==n});if("string"==typeof t){if(G.test(t))return x.filter(t,e,n);t=x.filter(t,e)}return x.grep(e,function(e){return g.call(t,e)>=0!==n})}var tt=/<(?!area|br|col|embed|hr|img|input|link|meta|param)(([\w:]+)[^>]*)\/>/gi,nt=/<([\w:]+)/,rt=/<|&#?\w+;/,it=/<(?:script|style|link)/i,ot=/^(?:checkbox|radio)$/i,st=/checked\s*(?:[^=]|=\s*.checked.)/i,at=/^$|\/(?:java|ecma)script/i,ut=/^true\/(.*)/,lt=/^\s*<!(?:\[CDATA\[|--)|(?:\]\]|--)>\s*$/g,ct={option:[1,"<select multiple='multiple'>","</select>"],thead:[1,"<table>","</table>"],col:[2,"<table><colgroup>","</colgroup></table>"],tr:[2,"<table><tbody>","</tbody></table>"],td:[3,"<table><tbody><tr>","</tr></tbody></table>"],_default:[0,"",""]};ct.optgroup=ct.option,ct.tbody=ct.tfoot=ct.colgroup=ct.caption=ct.thead,ct.th=ct.td,x.fn.extend({text:function(e){return x.access(this,function(e){return e===undefined?x.text(this):this.empty().append((this[0]&&this[0].ownerDocument||o).createTextNode(e))},null,e,arguments.length)},append:function(){return this.domManip(arguments,function(e){if(1===this.nodeType||11===this.nodeType||9===this.nodeType){var t=pt(this,e);t.appendChild(e)}})},prepend:function(){return this.domManip(arguments,function(e){if(1===this.nodeType||11===this.nodeType||9===this.nodeType){var t=pt(this,e);t.insertBefore(e,t.firstChild)}})},before:function(){return this.domManip(arguments,function(e){this.parentNode&&this.parentNode.insertBefore(e,this)})},after:function(){return this.domManip(arguments,function(e){this.parentNode&&this.parentNode.insertBefore(e,this.nextSibling)})},remove:function(e,t){var n,r=e?x.filter(e,this):this,i=0;for(;null!=(n=r[i]);i++)t||1!==n.nodeType||x.cleanData(mt(n)),n.parentNode&&(t&&x.contains(n.ownerDocument,n)&&dt(mt(n,"script")),n.parentNode.removeChild(n));return this},empty:function(){var e,t=0;for(;null!=(e=this[t]);t++)1===e.nodeType&&(x.cleanData(mt(e,!1)),e.textContent="");return this},clone:function(e,t){return e=null==e?!1:e,t=null==t?e:t,this.map(function(){return x.clone(this,e,t)})},html:function(e){return x.access(this,function(e){var t=this[0]||{},n=0,r=this.length;if(e===undefined&&1===t.nodeType)return t.innerHTML;if("string"==typeof e&&!it.test(e)&&!ct[(nt.exec(e)||["",""])[1].toLowerCase()]){e=e.replace(tt,"<$1></$2>");try{for(;r>n;n++)t=this[n]||{},1===t.nodeType&&(x.cleanData(mt(t,!1)),t.innerHTML=e);t=0}catch(i){}}t&&this.empty().append(e)},null,e,arguments.length)},replaceWith:function(){var e=x.map(this,function(e){return[e.nextSibling,e.parentNode]}),t=0;return this.domManip(arguments,function(n){var r=e[t++],i=e[t++];i&&(r&&r.parentNode!==i&&(r=this.nextSibling),x(this).remove(),i.insertBefore(n,r))},!0),t?this:this.remove()},detach:function(e){return this.remove(e,!0)},domManip:function(e,t,n){e=f.apply([],e);var r,i,o,s,a,u,l=0,c=this.length,p=this,h=c-1,d=e[0],g=x.isFunction(d);if(g||!(1>=c||"string"!=typeof d||x.support.checkClone)&&st.test(d))return this.each(function(r){var i=p.eq(r);g&&(e[0]=d.call(this,r,i.html())),i.domManip(e,t,n)});if(c&&(r=x.buildFragment(e,this[0].ownerDocument,!1,!n&&this),i=r.firstChild,1===r.childNodes.length&&(r=i),i)){for(o=x.map(mt(r,"script"),ft),s=o.length;c>l;l++)a=r,l!==h&&(a=x.clone(a,!0,!0),s&&x.merge(o,mt(a,"script"))),t.call(this[l],a,l);if(s)for(u=o[o.length-1].ownerDocument,x.map(o,ht),l=0;s>l;l++)a=o[l],at.test(a.type||"")&&!q.access(a,"globalEval")&&x.contains(u,a)&&(a.src?x._evalUrl(a.src):x.globalEval(a.textContent.replace(lt,"")))}return this}}),x.each({appendTo:"append",prependTo:"prepend",insertBefore:"before",insertAfter:"after",replaceAll:"replaceWith"},function(e,t){x.fn[e]=function(e){var n,r=[],i=x(e),o=i.length-1,s=0;for(;o>=s;s++)n=s===o?this:this.clone(!0),x(i[s])[t](n),h.apply(r,n.get());return this.pushStack(r)}}),x.extend({clone:function(e,t,n){var r,i,o,s,a=e.cloneNode(!0),u=x.contains(e.ownerDocument,e);if(!(x.support.noCloneChecked||1!==e.nodeType&&11!==e.nodeType||x.isXMLDoc(e)))for(s=mt(a),o=mt(e),r=0,i=o.length;i>r;r++)yt(o[r],s[r]);if(t)if(n)for(o=o||mt(e),s=s||mt(a),r=0,i=o.length;i>r;r++)gt(o[r],s[r]);else gt(e,a);return s=mt(a,"script"),s.length>0&&dt(s,!u&&mt(e,"script")),a},buildFragment:function(e,t,n,r){var i,o,s,a,u,l,c=0,p=e.length,f=t.createDocumentFragment(),h=[];for(;p>c;c++)if(i=e[c],i||0===i)if("object"===x.type(i))x.merge(h,i.nodeType?[i]:i);else if(rt.test(i)){o=o||f.appendChild(t.createElement("div")),s=(nt.exec(i)||["",""])[1].toLowerCase(),a=ct[s]||ct._default,o.innerHTML=a[1]+i.replace(tt,"<$1></$2>")+a[2],l=a[0];while(l--)o=o.lastChild;x.merge(h,o.childNodes),o=f.firstChild,o.textContent=""}else h.push(t.createTextNode(i));f.textContent="",c=0;while(i=h[c++])if((!r||-1===x.inArray(i,r))&&(u=x.contains(i.ownerDocument,i),o=mt(f.appendChild(i),"script"),u&&dt(o),n)){l=0;while(i=o[l++])at.test(i.type||"")&&n.push(i)}return f},cleanData:function(e){var t,n,r,i,o,s,a=x.event.special,u=0;for(;(n=e[u])!==undefined;u++){if(F.accepts(n)&&(o=n[q.expando],o&&(t=q.cache[o]))){if(r=Object.keys(t.events||{}),r.length)for(s=0;(i=r[s])!==undefined;s++)a[i]?x.event.remove(n,i):x.removeEvent(n,i,t.handle);q.cache[o]&&delete q.cache[o]}delete L.cache[n[L.expando]]}},_evalUrl:function(e){return x.ajax({url:e,type:"GET",dataType:"script",async:!1,global:!1,"throws":!0})}});function pt(e,t){return x.nodeName(e,"table")&&x.nodeName(1===t.nodeType?t:t.firstChild,"tr")?e.getElementsByTagName("tbody")[0]||e.appendChild(e.ownerDocument.createElement("tbody")):e}function ft(e){return e.type=(null!==e.getAttribute("type"))+"/"+e.type,e}function ht(e){var t=ut.exec(e.type);return t?e.type=t[1]:e.removeAttribute("type"),e}function dt(e,t){var n=e.length,r=0;for(;n>r;r++)q.set(e[r],"globalEval",!t||q.get(t[r],"globalEval"))}function gt(e,t){var n,r,i,o,s,a,u,l;if(1===t.nodeType){if(q.hasData(e)&&(o=q.access(e),s=q.set(t,o),l=o.events)){delete s.handle,s.events={};for(i in l)for(n=0,r=l[i].length;r>n;n++)x.event.add(t,i,l[i][n])}L.hasData(e)&&(a=L.access(e),u=x.extend({},a),L.set(t,u))}}function mt(e,t){var n=e.getElementsByTagName?e.getElementsByTagName(t||"*"):e.querySelectorAll?e.querySelectorAll(t||"*"):[];return t===undefined||t&&x.nodeName(e,t)?x.merge([e],n):n}function yt(e,t){var n=t.nodeName.toLowerCase();"input"===n&&ot.test(e.type)?t.checked=e.checked:("input"===n||"textarea"===n)&&(t.defaultValue=e.defaultValue)}x.fn.extend({wrapAll:function(e){var t;return x.isFunction(e)?this.each(function(t){x(this).wrapAll(e.call(this,t))}):(this[0]&&(t=x(e,this[0].ownerDocument).eq(0).clone(!0),this[0].parentNode&&t.insertBefore(this[0]),t.map(function(){var e=this;while(e.firstElementChild)e=e.firstElementChild;return e}).append(this)),this)},wrapInner:function(e){return x.isFunction(e)?this.each(function(t){x(this).wrapInner(e.call(this,t))}):this.each(function(){var t=x(this),n=t.contents();n.length?n.wrapAll(e):t.append(e)})},wrap:function(e){var t=x.isFunction(e);return this.each(function(n){x(this).wrapAll(t?e.call(this,n):e)})},unwrap:function(){return this.parent().each(function(){x.nodeName(this,"body")||x(this).replaceWith(this.childNodes)}).end()}});var vt,xt,bt=/^(none|table(?!-c[ea]).+)/,wt=/^margin/,Tt=RegExp("^("+b+")(.*)$","i"),Ct=RegExp("^("+b+")(?!px)[a-z%]+$","i"),kt=RegExp("^([+-])=("+b+")","i"),Nt={BODY:"block"},Et={position:"absolute",visibility:"hidden",display:"block"},St={letterSpacing:0,fontWeight:400},jt=["Top","Right","Bottom","Left"],Dt=["Webkit","O","Moz","ms"];function At(e,t){if(t in e)return t;var n=t.charAt(0).toUpperCase()+t.slice(1),r=t,i=Dt.length;while(i--)if(t=Dt[i]+n,t in e)return t;return r}function Lt(e,t){return e=t||e,"none"===x.css(e,"display")||!x.contains(e.ownerDocument,e)}function qt(t){return e.getComputedStyle(t,null)}function Ht(e,t){var n,r,i,o=[],s=0,a=e.length;for(;a>s;s++)r=e[s],r.style&&(o[s]=q.get(r,"olddisplay"),n=r.style.display,t?(o[s]||"none"!==n||(r.style.display=""),""===r.style.display&&Lt(r)&&(o[s]=q.access(r,"olddisplay",Rt(r.nodeName)))):o[s]||(i=Lt(r),(n&&"none"!==n||!i)&&q.set(r,"olddisplay",i?n:x.css(r,"display"))));for(s=0;a>s;s++)r=e[s],r.style&&(t&&"none"!==r.style.display&&""!==r.style.display||(r.style.display=t?o[s]||"":"none"));return e}x.fn.extend({css:function(e,t){return x.access(this,function(e,t,n){var r,i,o={},s=0;if(x.isArray(t)){for(r=qt(e),i=t.length;i>s;s++)o[t[s]]=x.css(e,t[s],!1,r);return o}return n!==undefined?x.style(e,t,n):x.css(e,t)},e,t,arguments.length>1)},show:function(){return Ht(this,!0)},hide:function(){return Ht(this)},toggle:function(e){return"boolean"==typeof e?e?this.show():this.hide():this.each(function(){Lt(this)?x(this).show():x(this).hide()})}}),x.extend({cssHooks:{opacity:{get:function(e,t){if(t){var n=vt(e,"opacity");return""===n?"1":n}}}},cssNumber:{columnCount:!0,fillOpacity:!0,fontWeight:!0,lineHeight:!0,opacity:!0,order:!0,orphans:!0,widows:!0,zIndex:!0,zoom:!0},cssProps:{"float":"cssFloat"},style:function(e,t,n,r){if(e&&3!==e.nodeType&&8!==e.nodeType&&e.style){var i,o,s,a=x.camelCase(t),u=e.style;return t=x.cssProps[a]||(x.cssProps[a]=At(u,a)),s=x.cssHooks[t]||x.cssHooks[a],n===undefined?s&&"get"in s&&(i=s.get(e,!1,r))!==undefined?i:u[t]:(o=typeof n,"string"===o&&(i=kt.exec(n))&&(n=(i[1]+1)*i[2]+parseFloat(x.css(e,t)),o="number"),null==n||"number"===o&&isNaN(n)||("number"!==o||x.cssNumber[a]||(n+="px"),x.support.clearCloneStyle||""!==n||0!==t.indexOf("background")||(u[t]="inherit"),s&&"set"in s&&(n=s.set(e,n,r))===undefined||(u[t]=n)),undefined)}},css:function(e,t,n,r){var i,o,s,a=x.camelCase(t);return t=x.cssProps[a]||(x.cssProps[a]=At(e.style,a)),s=x.cssHooks[t]||x.cssHooks[a],s&&"get"in s&&(i=s.get(e,!0,n)),i===undefined&&(i=vt(e,t,r)),"normal"===i&&t in St&&(i=St[t]),""===n||n?(o=parseFloat(i),n===!0||x.isNumeric(o)?o||0:i):i}}),vt=function(e,t,n){var r,i,o,s=n||qt(e),a=s?s.getPropertyValue(t)||s[t]:undefined,u=e.style;return s&&(""!==a||x.contains(e.ownerDocument,e)||(a=x.style(e,t)),Ct.test(a)&&wt.test(t)&&(r=u.width,i=u.minWidth,o=u.maxWidth,u.minWidth=u.maxWidth=u.width=a,a=s.width,u.width=r,u.minWidth=i,u.maxWidth=o)),a};function Ot(e,t,n){var r=Tt.exec(t);return r?Math.max(0,r[1]-(n||0))+(r[2]||"px"):t}function Ft(e,t,n,r,i){var o=n===(r?"border":"content")?4:"width"===t?1:0,s=0;for(;4>o;o+=2)"margin"===n&&(s+=x.css(e,n+jt[o],!0,i)),r?("content"===n&&(s-=x.css(e,"padding"+jt[o],!0,i)),"margin"!==n&&(s-=x.css(e,"border"+jt[o]+"Width",!0,i))):(s+=x.css(e,"padding"+jt[o],!0,i),"padding"!==n&&(s+=x.css(e,"border"+jt[o]+"Width",!0,i)));return s}function Pt(e,t,n){var r=!0,i="width"===t?e.offsetWidth:e.offsetHeight,o=qt(e),s=x.support.boxSizing&&"border-box"===x.css(e,"boxSizing",!1,o);if(0>=i||null==i){if(i=vt(e,t,o),(0>i||null==i)&&(i=e.style[t]),Ct.test(i))return i;r=s&&(x.support.boxSizingReliable||i===e.style[t]),i=parseFloat(i)||0}return i+Ft(e,t,n||(s?"border":"content"),r,o)+"px"}function Rt(e){var t=o,n=Nt[e];return n||(n=Mt(e,t),"none"!==n&&n||(xt=(xt||x("<iframe frameborder='0' width='0' height='0'/>").css("cssText","display:block !important")).appendTo(t.documentElement),t=(xt[0].contentWindow||xt[0].contentDocument).document,t.write("<!doctype html><html><body>"),t.close(),n=Mt(e,t),xt.detach()),Nt[e]=n),n}function Mt(e,t){var n=x(t.createElement(e)).appendTo(t.body),r=x.css(n[0],"display");return n.remove(),r}x.each(["height","width"],function(e,t){x.cssHooks[t]={get:function(e,n,r){return n?0===e.offsetWidth&&bt.test(x.css(e,"display"))?x.swap(e,Et,function(){return Pt(e,t,r)}):Pt(e,t,r):undefined},set:function(e,n,r){var i=r&&qt(e);return Ot(e,n,r?Ft(e,t,r,x.support.boxSizing&&"border-box"===x.css(e,"boxSizing",!1,i),i):0)}}}),x(function(){x.support.reliableMarginRight||(x.cssHooks.marginRight={get:function(e,t){return t?x.swap(e,{display:"inline-block"},vt,[e,"marginRight"]):undefined}}),!x.support.pixelPosition&&x.fn.position&&x.each(["top","left"],function(e,t){x.cssHooks[t]={get:function(e,n){return n?(n=vt(e,t),Ct.test(n)?x(e).position()[t]+"px":n):undefined}}})}),x.expr&&x.expr.filters&&(x.expr.filters.hidden=function(e){return 0>=e.offsetWidth&&0>=e.offsetHeight},x.expr.filters.visible=function(e){return!x.expr.filters.hidden(e)}),x.each({margin:"",padding:"",border:"Width"},function(e,t){x.cssHooks[e+t]={expand:function(n){var r=0,i={},o="string"==typeof n?n.split(" "):[n];for(;4>r;r++)i[e+jt[r]+t]=o[r]||o[r-2]||o[0];return i}},wt.test(e)||(x.cssHooks[e+t].set=Ot)});var Wt=/%20/g,$t=/\[\]$/,Bt=/\r?\n/g,It=/^(?:submit|button|image|reset|file)$/i,zt=/^(?:input|select|textarea|keygen)/i;x.fn.extend({serialize:function(){return x.param(this.serializeArray())},serializeArray:function(){return this.map(function(){var e=x.prop(this,"elements");return e?x.makeArray(e):this}).filter(function(){var e=this.type;return this.name&&!x(this).is(":disabled")&&zt.test(this.nodeName)&&!It.test(e)&&(this.checked||!ot.test(e))}).map(function(e,t){var n=x(this).val();return null==n?null:x.isArray(n)?x.map(n,function(e){return{name:t.name,value:e.replace(Bt,"\r\n")}}):{name:t.name,value:n.replace(Bt,"\r\n")}}).get()}}),x.param=function(e,t){var n,r=[],i=function(e,t){t=x.isFunction(t)?t():null==t?"":t,r[r.length]=encodeURIComponent(e)+"="+encodeURIComponent(t)};if(t===undefined&&(t=x.ajaxSettings&&x.ajaxSettings.traditional),x.isArray(e)||e.jquery&&!x.isPlainObject(e))x.each(e,function(){i(this.name,this.value)});else for(n in e)_t(n,e[n],t,i);return r.join("&").replace(Wt,"+")};function _t(e,t,n,r){var i;if(x.isArray(t))x.each(t,function(t,i){n||$t.test(e)?r(e,i):_t(e+"["+("object"==typeof i?t:"")+"]",i,n,r)});else if(n||"object"!==x.type(t))r(e,t);else for(i in t)_t(e+"["+i+"]",t[i],n,r)}x.each("blur focus focusin focusout load resize scroll unload click dblclick mousedown mouseup mousemove mouseover mouseout mouseenter mouseleave change select submit keydown keypress keyup error contextmenu".split(" "),function(e,t){x.fn[t]=function(e,n){return arguments.length>0?this.on(t,null,e,n):this.trigger(t)}}),x.fn.extend({hover:function(e,t){return this.mouseenter(e).mouseleave(t||e)},bind:function(e,t,n){return this.on(e,null,t,n)},unbind:function(e,t){return this.off(e,null,t)
  },delegate:function(e,t,n,r){return this.on(t,e,n,r)},undelegate:function(e,t,n){return 1===arguments.length?this.off(e,"**"):this.off(t,e||"**",n)}});var Xt,Ut,Yt=x.now(),Vt=/\?/,Gt=/#.*$/,Jt=/([?&])_=[^&]*/,Qt=/^(.*?):[ \t]*([^\r\n]*)$/gm,Kt=/^(?:about|app|app-storage|.+-extension|file|res|widget):$/,Zt=/^(?:GET|HEAD)$/,en=/^\/\//,tn=/^([\w.+-]+:)(?:\/\/([^\/?#:]*)(?::(\d+)|)|)/,nn=x.fn.load,rn={},on={},sn="*/".concat("*");try{Ut=i.href}catch(an){Ut=o.createElement("a"),Ut.href="",Ut=Ut.href}Xt=tn.exec(Ut.toLowerCase())||[];function un(e){return function(t,n){"string"!=typeof t&&(n=t,t="*");var r,i=0,o=t.toLowerCase().match(w)||[];if(x.isFunction(n))while(r=o[i++])"+"===r[0]?(r=r.slice(1)||"*",(e[r]=e[r]||[]).unshift(n)):(e[r]=e[r]||[]).push(n)}}function ln(e,t,n,r){var i={},o=e===on;function s(a){var u;return i[a]=!0,x.each(e[a]||[],function(e,a){var l=a(t,n,r);return"string"!=typeof l||o||i[l]?o?!(u=l):undefined:(t.dataTypes.unshift(l),s(l),!1)}),u}return s(t.dataTypes[0])||!i["*"]&&s("*")}function cn(e,t){var n,r,i=x.ajaxSettings.flatOptions||{};for(n in t)t[n]!==undefined&&((i[n]?e:r||(r={}))[n]=t[n]);return r&&x.extend(!0,e,r),e}x.fn.load=function(e,t,n){if("string"!=typeof e&&nn)return nn.apply(this,arguments);var r,i,o,s=this,a=e.indexOf(" ");return a>=0&&(r=e.slice(a),e=e.slice(0,a)),x.isFunction(t)?(n=t,t=undefined):t&&"object"==typeof t&&(i="POST"),s.length>0&&x.ajax({url:e,type:i,dataType:"html",data:t}).done(function(e){o=arguments,s.html(r?x("<div>").append(x.parseHTML(e)).find(r):e)}).complete(n&&function(e,t){s.each(n,o||[e.responseText,t,e])}),this},x.each(["ajaxStart","ajaxStop","ajaxComplete","ajaxError","ajaxSuccess","ajaxSend"],function(e,t){x.fn[t]=function(e){return this.on(t,e)}}),x.extend({active:0,lastModified:{},etag:{},ajaxSettings:{url:Ut,type:"GET",isLocal:Kt.test(Xt[1]),global:!0,processData:!0,async:!0,contentType:"application/x-www-form-urlencoded; charset=UTF-8",accepts:{"*":sn,text:"text/plain",html:"text/html",xml:"application/xml, text/xml",json:"application/json, text/javascript"},contents:{xml:/xml/,html:/html/,json:/json/},responseFields:{xml:"responseXML",text:"responseText",json:"responseJSON"},converters:{"* text":String,"text html":!0,"text json":x.parseJSON,"text xml":x.parseXML},flatOptions:{url:!0,context:!0}},ajaxSetup:function(e,t){return t?cn(cn(e,x.ajaxSettings),t):cn(x.ajaxSettings,e)},ajaxPrefilter:un(rn),ajaxTransport:un(on),ajax:function(e,t){"object"==typeof e&&(t=e,e=undefined),t=t||{};var n,r,i,o,s,a,u,l,c=x.ajaxSetup({},t),p=c.context||c,f=c.context&&(p.nodeType||p.jquery)?x(p):x.event,h=x.Deferred(),d=x.Callbacks("once memory"),g=c.statusCode||{},m={},y={},v=0,b="canceled",T={readyState:0,getResponseHeader:function(e){var t;if(2===v){if(!o){o={};while(t=Qt.exec(i))o[t[1].toLowerCase()]=t[2]}t=o[e.toLowerCase()]}return null==t?null:t},getAllResponseHeaders:function(){return 2===v?i:null},setRequestHeader:function(e,t){var n=e.toLowerCase();return v||(e=y[n]=y[n]||e,m[e]=t),this},overrideMimeType:function(e){return v||(c.mimeType=e),this},statusCode:function(e){var t;if(e)if(2>v)for(t in e)g[t]=[g[t],e[t]];else T.always(e[T.status]);return this},abort:function(e){var t=e||b;return n&&n.abort(t),k(0,t),this}};if(h.promise(T).complete=d.add,T.success=T.done,T.error=T.fail,c.url=((e||c.url||Ut)+"").replace(Gt,"").replace(en,Xt[1]+"//"),c.type=t.method||t.type||c.method||c.type,c.dataTypes=x.trim(c.dataType||"*").toLowerCase().match(w)||[""],null==c.crossDomain&&(a=tn.exec(c.url.toLowerCase()),c.crossDomain=!(!a||a[1]===Xt[1]&&a[2]===Xt[2]&&(a[3]||("http:"===a[1]?"80":"443"))===(Xt[3]||("http:"===Xt[1]?"80":"443")))),c.data&&c.processData&&"string"!=typeof c.data&&(c.data=x.param(c.data,c.traditional)),ln(rn,c,t,T),2===v)return T;u=c.global,u&&0===x.active++&&x.event.trigger("ajaxStart"),c.type=c.type.toUpperCase(),c.hasContent=!Zt.test(c.type),r=c.url,c.hasContent||(c.data&&(r=c.url+=(Vt.test(r)?"&":"?")+c.data,delete c.data),c.cache===!1&&(c.url=Jt.test(r)?r.replace(Jt,"$1_="+Yt++):r+(Vt.test(r)?"&":"?")+"_="+Yt++)),c.ifModified&&(x.lastModified[r]&&T.setRequestHeader("If-Modified-Since",x.lastModified[r]),x.etag[r]&&T.setRequestHeader("If-None-Match",x.etag[r])),(c.data&&c.hasContent&&c.contentType!==!1||t.contentType)&&T.setRequestHeader("Content-Type",c.contentType),T.setRequestHeader("Accept",c.dataTypes[0]&&c.accepts[c.dataTypes[0]]?c.accepts[c.dataTypes[0]]+("*"!==c.dataTypes[0]?", "+sn+"; q=0.01":""):c.accepts["*"]);for(l in c.headers)T.setRequestHeader(l,c.headers[l]);if(c.beforeSend&&(c.beforeSend.call(p,T,c)===!1||2===v))return T.abort();b="abort";for(l in{success:1,error:1,complete:1})T[l](c[l]);if(n=ln(on,c,t,T)){T.readyState=1,u&&f.trigger("ajaxSend",[T,c]),c.async&&c.timeout>0&&(s=setTimeout(function(){T.abort("timeout")},c.timeout));try{v=1,n.send(m,k)}catch(C){if(!(2>v))throw C;k(-1,C)}}else k(-1,"No Transport");function k(e,t,o,a){var l,m,y,b,w,C=t;2!==v&&(v=2,s&&clearTimeout(s),n=undefined,i=a||"",T.readyState=e>0?4:0,l=e>=200&&300>e||304===e,o&&(b=pn(c,T,o)),b=fn(c,b,T,l),l?(c.ifModified&&(w=T.getResponseHeader("Last-Modified"),w&&(x.lastModified[r]=w),w=T.getResponseHeader("etag"),w&&(x.etag[r]=w)),204===e||"HEAD"===c.type?C="nocontent":304===e?C="notmodified":(C=b.state,m=b.data,y=b.error,l=!y)):(y=C,(e||!C)&&(C="error",0>e&&(e=0))),T.status=e,T.statusText=(t||C)+"",l?h.resolveWith(p,[m,C,T]):h.rejectWith(p,[T,C,y]),T.statusCode(g),g=undefined,u&&f.trigger(l?"ajaxSuccess":"ajaxError",[T,c,l?m:y]),d.fireWith(p,[T,C]),u&&(f.trigger("ajaxComplete",[T,c]),--x.active||x.event.trigger("ajaxStop")))}return T},getJSON:function(e,t,n){return x.get(e,t,n,"json")},getScript:function(e,t){return x.get(e,undefined,t,"script")}}),x.each(["get","post"],function(e,t){x[t]=function(e,n,r,i){return x.isFunction(n)&&(i=i||r,r=n,n=undefined),x.ajax({url:e,type:t,dataType:i,data:n,success:r})}});function pn(e,t,n){var r,i,o,s,a=e.contents,u=e.dataTypes;while("*"===u[0])u.shift(),r===undefined&&(r=e.mimeType||t.getResponseHeader("Content-Type"));if(r)for(i in a)if(a[i]&&a[i].test(r)){u.unshift(i);break}if(u[0]in n)o=u[0];else{for(i in n){if(!u[0]||e.converters[i+" "+u[0]]){o=i;break}s||(s=i)}o=o||s}return o?(o!==u[0]&&u.unshift(o),n[o]):undefined}function fn(e,t,n,r){var i,o,s,a,u,l={},c=e.dataTypes.slice();if(c[1])for(s in e.converters)l[s.toLowerCase()]=e.converters[s];o=c.shift();while(o)if(e.responseFields[o]&&(n[e.responseFields[o]]=t),!u&&r&&e.dataFilter&&(t=e.dataFilter(t,e.dataType)),u=o,o=c.shift())if("*"===o)o=u;else if("*"!==u&&u!==o){if(s=l[u+" "+o]||l["* "+o],!s)for(i in l)if(a=i.split(" "),a[1]===o&&(s=l[u+" "+a[0]]||l["* "+a[0]])){s===!0?s=l[i]:l[i]!==!0&&(o=a[0],c.unshift(a[1]));break}if(s!==!0)if(s&&e["throws"])t=s(t);else try{t=s(t)}catch(p){return{state:"parsererror",error:s?p:"No conversion from "+u+" to "+o}}}return{state:"success",data:t}}x.ajaxSetup({accepts:{script:"text/javascript, application/javascript, application/ecmascript, application/x-ecmascript"},contents:{script:/(?:java|ecma)script/},converters:{"text script":function(e){return x.globalEval(e),e}}}),x.ajaxPrefilter("script",function(e){e.cache===undefined&&(e.cache=!1),e.crossDomain&&(e.type="GET")}),x.ajaxTransport("script",function(e){if(e.crossDomain){var t,n;return{send:function(r,i){t=x("<script>").prop({async:!0,charset:e.scriptCharset,src:e.url}).on("load error",n=function(e){t.remove(),n=null,e&&i("error"===e.type?404:200,e.type)}),o.head.appendChild(t[0])},abort:function(){n&&n()}}}});var hn=[],dn=/(=)\?(?=&|$)|\?\?/;x.ajaxSetup({jsonp:"callback",jsonpCallback:function(){var e=hn.pop()||x.expando+"_"+Yt++;return this[e]=!0,e}}),x.ajaxPrefilter("json jsonp",function(t,n,r){var i,o,s,a=t.jsonp!==!1&&(dn.test(t.url)?"url":"string"==typeof t.data&&!(t.contentType||"").indexOf("application/x-www-form-urlencoded")&&dn.test(t.data)&&"data");return a||"jsonp"===t.dataTypes[0]?(i=t.jsonpCallback=x.isFunction(t.jsonpCallback)?t.jsonpCallback():t.jsonpCallback,a?t[a]=t[a].replace(dn,"$1"+i):t.jsonp!==!1&&(t.url+=(Vt.test(t.url)?"&":"?")+t.jsonp+"="+i),t.converters["script json"]=function(){return s||x.error(i+" was not called"),s[0]},t.dataTypes[0]="json",o=e[i],e[i]=function(){s=arguments},r.always(function(){e[i]=o,t[i]&&(t.jsonpCallback=n.jsonpCallback,hn.push(i)),s&&x.isFunction(o)&&o(s[0]),s=o=undefined}),"script"):undefined}),x.ajaxSettings.xhr=function(){try{return new XMLHttpRequest}catch(e){}};var gn=x.ajaxSettings.xhr(),mn={0:200,1223:204},yn=0,vn={};e.ActiveXObject&&x(e).on("unload",function(){for(var e in vn)vn[e]();vn=undefined}),x.support.cors=!!gn&&"withCredentials"in gn,x.support.ajax=gn=!!gn,x.ajaxTransport(function(e){var t;return x.support.cors||gn&&!e.crossDomain?{send:function(n,r){var i,o,s=e.xhr();if(s.open(e.type,e.url,e.async,e.username,e.password),e.xhrFields)for(i in e.xhrFields)s[i]=e.xhrFields[i];e.mimeType&&s.overrideMimeType&&s.overrideMimeType(e.mimeType),e.crossDomain||n["X-Requested-With"]||(n["X-Requested-With"]="XMLHttpRequest");for(i in n)s.setRequestHeader(i,n[i]);t=function(e){return function(){t&&(delete vn[o],t=s.onload=s.onerror=null,"abort"===e?s.abort():"error"===e?r(s.status||404,s.statusText):r(mn[s.status]||s.status,s.statusText,"string"==typeof s.responseText?{text:s.responseText}:undefined,s.getAllResponseHeaders()))}},s.onload=t(),s.onerror=t("error"),t=vn[o=yn++]=t("abort"),s.send(e.hasContent&&e.data||null)},abort:function(){t&&t()}}:undefined});var xn,bn,wn=/^(?:toggle|show|hide)$/,Tn=RegExp("^(?:([+-])=|)("+b+")([a-z%]*)$","i"),Cn=/queueHooks$/,kn=[An],Nn={"*":[function(e,t){var n=this.createTween(e,t),r=n.cur(),i=Tn.exec(t),o=i&&i[3]||(x.cssNumber[e]?"":"px"),s=(x.cssNumber[e]||"px"!==o&&+r)&&Tn.exec(x.css(n.elem,e)),a=1,u=20;if(s&&s[3]!==o){o=o||s[3],i=i||[],s=+r||1;do a=a||".5",s/=a,x.style(n.elem,e,s+o);while(a!==(a=n.cur()/r)&&1!==a&&--u)}return i&&(s=n.start=+s||+r||0,n.unit=o,n.end=i[1]?s+(i[1]+1)*i[2]:+i[2]),n}]};function En(){return setTimeout(function(){xn=undefined}),xn=x.now()}function Sn(e,t,n){var r,i=(Nn[t]||[]).concat(Nn["*"]),o=0,s=i.length;for(;s>o;o++)if(r=i[o].call(n,t,e))return r}function jn(e,t,n){var r,i,o=0,s=kn.length,a=x.Deferred().always(function(){delete u.elem}),u=function(){if(i)return!1;var t=xn||En(),n=Math.max(0,l.startTime+l.duration-t),r=n/l.duration||0,o=1-r,s=0,u=l.tweens.length;for(;u>s;s++)l.tweens[s].run(o);return a.notifyWith(e,[l,o,n]),1>o&&u?n:(a.resolveWith(e,[l]),!1)},l=a.promise({elem:e,props:x.extend({},t),opts:x.extend(!0,{specialEasing:{}},n),originalProperties:t,originalOptions:n,startTime:xn||En(),duration:n.duration,tweens:[],createTween:function(t,n){var r=x.Tween(e,l.opts,t,n,l.opts.specialEasing[t]||l.opts.easing);return l.tweens.push(r),r},stop:function(t){var n=0,r=t?l.tweens.length:0;if(i)return this;for(i=!0;r>n;n++)l.tweens[n].run(1);return t?a.resolveWith(e,[l,t]):a.rejectWith(e,[l,t]),this}}),c=l.props;for(Dn(c,l.opts.specialEasing);s>o;o++)if(r=kn[o].call(l,e,c,l.opts))return r;return x.map(c,Sn,l),x.isFunction(l.opts.start)&&l.opts.start.call(e,l),x.fx.timer(x.extend(u,{elem:e,anim:l,queue:l.opts.queue})),l.progress(l.opts.progress).done(l.opts.done,l.opts.complete).fail(l.opts.fail).always(l.opts.always)}function Dn(e,t){var n,r,i,o,s;for(n in e)if(r=x.camelCase(n),i=t[r],o=e[n],x.isArray(o)&&(i=o[1],o=e[n]=o[0]),n!==r&&(e[r]=o,delete e[n]),s=x.cssHooks[r],s&&"expand"in s){o=s.expand(o),delete e[r];for(n in o)n in e||(e[n]=o[n],t[n]=i)}else t[r]=i}x.Animation=x.extend(jn,{tweener:function(e,t){x.isFunction(e)?(t=e,e=["*"]):e=e.split(" ");var n,r=0,i=e.length;for(;i>r;r++)n=e[r],Nn[n]=Nn[n]||[],Nn[n].unshift(t)},prefilter:function(e,t){t?kn.unshift(e):kn.push(e)}});function An(e,t,n){var r,i,o,s,a,u,l=this,c={},p=e.style,f=e.nodeType&&Lt(e),h=q.get(e,"fxshow");n.queue||(a=x._queueHooks(e,"fx"),null==a.unqueued&&(a.unqueued=0,u=a.empty.fire,a.empty.fire=function(){a.unqueued||u()}),a.unqueued++,l.always(function(){l.always(function(){a.unqueued--,x.queue(e,"fx").length||a.empty.fire()})})),1===e.nodeType&&("height"in t||"width"in t)&&(n.overflow=[p.overflow,p.overflowX,p.overflowY],"inline"===x.css(e,"display")&&"none"===x.css(e,"float")&&(p.display="inline-block")),n.overflow&&(p.overflow="hidden",l.always(function(){p.overflow=n.overflow[0],p.overflowX=n.overflow[1],p.overflowY=n.overflow[2]}));for(r in t)if(i=t[r],wn.exec(i)){if(delete t[r],o=o||"toggle"===i,i===(f?"hide":"show")){if("show"!==i||!h||h[r]===undefined)continue;f=!0}c[r]=h&&h[r]||x.style(e,r)}if(!x.isEmptyObject(c)){h?"hidden"in h&&(f=h.hidden):h=q.access(e,"fxshow",{}),o&&(h.hidden=!f),f?x(e).show():l.done(function(){x(e).hide()}),l.done(function(){var t;q.remove(e,"fxshow");for(t in c)x.style(e,t,c[t])});for(r in c)s=Sn(f?h[r]:0,r,l),r in h||(h[r]=s.start,f&&(s.end=s.start,s.start="width"===r||"height"===r?1:0))}}function Ln(e,t,n,r,i){return new Ln.prototype.init(e,t,n,r,i)}x.Tween=Ln,Ln.prototype={constructor:Ln,init:function(e,t,n,r,i,o){this.elem=e,this.prop=n,this.easing=i||"swing",this.options=t,this.start=this.now=this.cur(),this.end=r,this.unit=o||(x.cssNumber[n]?"":"px")},cur:function(){var e=Ln.propHooks[this.prop];return e&&e.get?e.get(this):Ln.propHooks._default.get(this)},run:function(e){var t,n=Ln.propHooks[this.prop];return this.pos=t=this.options.duration?x.easing[this.easing](e,this.options.duration*e,0,1,this.options.duration):e,this.now=(this.end-this.start)*t+this.start,this.options.step&&this.options.step.call(this.elem,this.now,this),n&&n.set?n.set(this):Ln.propHooks._default.set(this),this}},Ln.prototype.init.prototype=Ln.prototype,Ln.propHooks={_default:{get:function(e){var t;return null==e.elem[e.prop]||e.elem.style&&null!=e.elem.style[e.prop]?(t=x.css(e.elem,e.prop,""),t&&"auto"!==t?t:0):e.elem[e.prop]},set:function(e){x.fx.step[e.prop]?x.fx.step[e.prop](e):e.elem.style&&(null!=e.elem.style[x.cssProps[e.prop]]||x.cssHooks[e.prop])?x.style(e.elem,e.prop,e.now+e.unit):e.elem[e.prop]=e.now}}},Ln.propHooks.scrollTop=Ln.propHooks.scrollLeft={set:function(e){e.elem.nodeType&&e.elem.parentNode&&(e.elem[e.prop]=e.now)}},x.each(["toggle","show","hide"],function(e,t){var n=x.fn[t];x.fn[t]=function(e,r,i){return null==e||"boolean"==typeof e?n.apply(this,arguments):this.animate(qn(t,!0),e,r,i)}}),x.fn.extend({fadeTo:function(e,t,n,r){return this.filter(Lt).css("opacity",0).show().end().animate({opacity:t},e,n,r)},animate:function(e,t,n,r){var i=x.isEmptyObject(e),o=x.speed(t,n,r),s=function(){var t=jn(this,x.extend({},e),o);(i||q.get(this,"finish"))&&t.stop(!0)};return s.finish=s,i||o.queue===!1?this.each(s):this.queue(o.queue,s)},stop:function(e,t,n){var r=function(e){var t=e.stop;delete e.stop,t(n)};return"string"!=typeof e&&(n=t,t=e,e=undefined),t&&e!==!1&&this.queue(e||"fx",[]),this.each(function(){var t=!0,i=null!=e&&e+"queueHooks",o=x.timers,s=q.get(this);if(i)s[i]&&s[i].stop&&r(s[i]);else for(i in s)s[i]&&s[i].stop&&Cn.test(i)&&r(s[i]);for(i=o.length;i--;)o[i].elem!==this||null!=e&&o[i].queue!==e||(o[i].anim.stop(n),t=!1,o.splice(i,1));(t||!n)&&x.dequeue(this,e)})},finish:function(e){return e!==!1&&(e=e||"fx"),this.each(function(){var t,n=q.get(this),r=n[e+"queue"],i=n[e+"queueHooks"],o=x.timers,s=r?r.length:0;for(n.finish=!0,x.queue(this,e,[]),i&&i.stop&&i.stop.call(this,!0),t=o.length;t--;)o[t].elem===this&&o[t].queue===e&&(o[t].anim.stop(!0),o.splice(t,1));for(t=0;s>t;t++)r[t]&&r[t].finish&&r[t].finish.call(this);delete n.finish})}});function qn(e,t){var n,r={height:e},i=0;for(t=t?1:0;4>i;i+=2-t)n=jt[i],r["margin"+n]=r["padding"+n]=e;return t&&(r.opacity=r.width=e),r}x.each({slideDown:qn("show"),slideUp:qn("hide"),slideToggle:qn("toggle"),fadeIn:{opacity:"show"},fadeOut:{opacity:"hide"},fadeToggle:{opacity:"toggle"}},function(e,t){x.fn[e]=function(e,n,r){return this.animate(t,e,n,r)}}),x.speed=function(e,t,n){var r=e&&"object"==typeof e?x.extend({},e):{complete:n||!n&&t||x.isFunction(e)&&e,duration:e,easing:n&&t||t&&!x.isFunction(t)&&t};return r.duration=x.fx.off?0:"number"==typeof r.duration?r.duration:r.duration in x.fx.speeds?x.fx.speeds[r.duration]:x.fx.speeds._default,(null==r.queue||r.queue===!0)&&(r.queue="fx"),r.old=r.complete,r.complete=function(){x.isFunction(r.old)&&r.old.call(this),r.queue&&x.dequeue(this,r.queue)},r},x.easing={linear:function(e){return e},swing:function(e){return.5-Math.cos(e*Math.PI)/2}},x.timers=[],x.fx=Ln.prototype.init,x.fx.tick=function(){var e,t=x.timers,n=0;for(xn=x.now();t.length>n;n++)e=t[n],e()||t[n]!==e||t.splice(n--,1);t.length||x.fx.stop(),xn=undefined},x.fx.timer=function(e){e()&&x.timers.push(e)&&x.fx.start()},x.fx.interval=13,x.fx.start=function(){bn||(bn=setInterval(x.fx.tick,x.fx.interval))},x.fx.stop=function(){clearInterval(bn),bn=null},x.fx.speeds={slow:600,fast:200,_default:400},x.fx.step={},x.expr&&x.expr.filters&&(x.expr.filters.animated=function(e){return x.grep(x.timers,function(t){return e===t.elem}).length}),x.fn.offset=function(e){if(arguments.length)return e===undefined?this:this.each(function(t){x.offset.setOffset(this,e,t)});var t,n,i=this[0],o={top:0,left:0},s=i&&i.ownerDocument;if(s)return t=s.documentElement,x.contains(t,i)?(typeof i.getBoundingClientRect!==r&&(o=i.getBoundingClientRect()),n=Hn(s),{top:o.top+n.pageYOffset-t.clientTop,left:o.left+n.pageXOffset-t.clientLeft}):o},x.offset={setOffset:function(e,t,n){var r,i,o,s,a,u,l,c=x.css(e,"position"),p=x(e),f={};"static"===c&&(e.style.position="relative"),a=p.offset(),o=x.css(e,"top"),u=x.css(e,"left"),l=("absolute"===c||"fixed"===c)&&(o+u).indexOf("auto")>-1,l?(r=p.position(),s=r.top,i=r.left):(s=parseFloat(o)||0,i=parseFloat(u)||0),x.isFunction(t)&&(t=t.call(e,n,a)),null!=t.top&&(f.top=t.top-a.top+s),null!=t.left&&(f.left=t.left-a.left+i),"using"in t?t.using.call(e,f):p.css(f)}},x.fn.extend({position:function(){if(this[0]){var e,t,n=this[0],r={top:0,left:0};return"fixed"===x.css(n,"position")?t=n.getBoundingClientRect():(e=this.offsetParent(),t=this.offset(),x.nodeName(e[0],"html")||(r=e.offset()),r.top+=x.css(e[0],"borderTopWidth",!0),r.left+=x.css(e[0],"borderLeftWidth",!0)),{top:t.top-r.top-x.css(n,"marginTop",!0),left:t.left-r.left-x.css(n,"marginLeft",!0)}}},offsetParent:function(){return this.map(function(){var e=this.offsetParent||s;while(e&&!x.nodeName(e,"html")&&"static"===x.css(e,"position"))e=e.offsetParent;return e||s})}}),x.each({scrollLeft:"pageXOffset",scrollTop:"pageYOffset"},function(t,n){var r="pageYOffset"===n;x.fn[t]=function(i){return x.access(this,function(t,i,o){var s=Hn(t);return o===undefined?s?s[n]:t[i]:(s?s.scrollTo(r?e.pageXOffset:o,r?o:e.pageYOffset):t[i]=o,undefined)},t,i,arguments.length,null)}});function Hn(e){return x.isWindow(e)?e:9===e.nodeType&&e.defaultView}x.each({Height:"height",Width:"width"},function(e,t){x.each({padding:"inner"+e,content:t,"":"outer"+e},function(n,r){x.fn[r]=function(r,i){var o=arguments.length&&(n||"boolean"!=typeof r),s=n||(r===!0||i===!0?"margin":"border");return x.access(this,function(t,n,r){var i;return x.isWindow(t)?t.document.documentElement["client"+e]:9===t.nodeType?(i=t.documentElement,Math.max(t.body["scroll"+e],i["scroll"+e],t.body["offset"+e],i["offset"+e],i["client"+e])):r===undefined?x.css(t,n,s):x.style(t,n,r,s)},t,o?r:undefined,o,null)}})}),x.fn.size=function(){return this.length},x.fn.andSelf=x.fn.addBack,"object"==typeof module&&module&&"object"==typeof module.exports?module.exports=x:"function"==typeof define&&define.amd&&define("jquery",[],function(){return x}),"object"==typeof e&&"object"==typeof e.document&&(e.jQuery=e.$=x)})(window);
  `

  interpret = (str) ->
    console.debug(str)
    Yak.interpreter.evalSync(str)

  MutationObserver = window.MutationObserver or window.WebKitMutationObserver

  onInserted = (callback) ->
    (new MutationObserver((muts) ->
      callback muts.filter((mut) ->
        true
      ).map((mut) ->
        Array::slice.call mut.addedNodes, 0
      ).reduce((a, b) ->
        a.concat b
      )
    )).observe document,
      childList: true
      subtree: true

  onInserted (elems) ->
    elems.filter((elem) ->
      $(elem).is "script[type=\"text/yak\"]"
    ).map (elem) ->
      if $(elem).is("[src]")
        if $(elem).attr("src").match(/^http:\/\//)
          $.get "http://www.corsproxy.com/" + $(elem).attr("src").match(/^http:\/\/(.+)/)[1], (data) ->
            console.debug "local src interpret:"
            interpret data

        else
          $.get $(elem).attr("src"), (data) ->
            console.debug "local src interpret:"
            interpret data

      else
        console.debug "inline interpret:"
        interpret($(elem).text().replace(/^[ \t]*\n/, ''))

)()
Yak