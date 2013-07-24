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
                            if space isnt l
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
                if value is 'nil' or value is 'unknown' or value is 'class' or value is 'module'
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
                body: body
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
    exports.parse = parse
    exports.stringify = stringify
    exports.tokenizer = tokenizer
    exports.parseForRacket = parseForRacket
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
