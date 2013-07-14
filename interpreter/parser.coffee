parseError = (token, message) ->
    throw new SyntaxError "#{message} at :#{token.line}:#{token.character}"

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

    isSpace = (c) -> c is ' ' or c is '\t'

    isDigit = (c) -> -1 isnt '0123456789'.indexOf c

    isIdentifier = (c) -> -1 isnt '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-+*/%_$<>=?!'.indexOf c

    (raw) ->
        token = (type, value = '') -> {
            type, value, line
            character: character }

        syntaxError = (message) ->
            throw new SyntaxError "#{message} at :#{line}:#{character}"

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
            else
                j = raw.indexOf '\n', i
                break if j is -1
                i = j

        s += raw.substring i

        i = 0
        character = 0
        lastNewline = 0
        line = 1
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
            if i >= length
                if indent.length > 1
                    indent.pop()
                    return token 'outdent'
                return token 'end'
            character = 1 + i - lastNewline
            if (q = s.substr i, 3) is '|:=' or (q = s.substr i, 2) is ':=' or q is '|=' or q is '<-' or q is '<<'
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
                if c is '.'
                    value += c
                    ++i
                    c = s[i]
                    if not isDigit c
                        character =
                        if value isnt '.'
                            t = token 'number', +value.substr 0, value.length - 1
                        character = i - lastNewline
                        d = token 'dot application', '.'
                        tokens.push d if t
                        return t ? d
                    loop
                        value += c
                        ++i
                        c = s[i]
                        break unless isDigit c
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

            if c is '@' or isIdentifier c
                type = if c is '@' then 'formal parameter' else 'identifier'
                value = ''
                loop
                    value += c
                    ++i
                    c = s[i]
                    break unless isIdentifier c
                if value is 'true' or value is 'false'
                    return token 'boolean', value is 'true'
                if value is 'nil' or value is 'dot' or value is 'unknown'
                    return token value, value
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

            if c is '.'
                ++i
                return token 'dot application'

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

    class Symbol
        null: ->
            parseError "Unexpected operator"
        left: ->
            parseError "Unexpected value"

    parse = (s) ->
        tokens = tokenizer s
        result = sequence tokens
        tokens.require 'end'
        result

    sequence = (tokens) ->
        n = tokens.here()
        result = []
        loop
            s = expression tokens, true
            break unless s
            result.push s
            break if not tokens.match 'newline'
        line: n.line
        character: n.character
        type: 'sequence'
        expressions: result

    expression = (tokens, optional) ->
        if tokens.match 'indent'
            result = sequence tokens
            tokens.require 'outdent'
            return result
        e = value tokens
        if not e
            if optional
                return null
            else
                parseError tokens.here(), "Expected expression"
        loop
            if -1 isnt ['identifier', 'formal parameter', 'string', 'number', 'boolean', 'nil', 'dot', 'unknown', 'funject start', 'group start', 'list start'].indexOf tokens.here().type
                t = tokens.here()
                e =
                    type: 'application'
                    line: t.line
                    character: t.character
                    funject: e
                    argument: value tokens
                continue
            if 'indent' is tokens.here().type
                t = tokens.here()
                e =
                    type: 'application'
                    line: t.line
                    character: t.character
                    funject: e
                    argument: expression tokens
                # do not continue, since we cannot directly apply sequences to values which would normally be expressions
            if t = tokens.match 'dot application'
                name = tokens.require 'identifier'
                e =
                    type: 'application'
                    line: t.line
                    character: t.character
                    funject: e
                    argument:
                        type: 'list'
                        line: t.line
                        character: t.character
                        values: [
                            {
                                type: 'dot'
                                value: '.'
                                line: t.line
                                character: t.character
                            }
                            {
                                type: 'string'
                                value: name.value
                                line: name.line
                                character: name.character
                            }
                        ]
                continue
            break
        if assignment = tokens.match 'strict assignment', 'lazy assignment', 'reset strict assignment', 'reset lazy assignment', 'inverse assignment', 'inheritance assignment'
            if assignment.type isnt 'inheritance assignment' and assignment.type isnt 'inverse assignment' and e.type isnt 'identifier' and (e.type isnt 'application' or assignment.type isnt 'strict assignment' and assignment.type isnt 'lazy assignment')
                parseError assignment, 'Invalid left-hand side of assignment'
            e =
                type: 'assignment'
                operator: assignment.type
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
                line: t.line
                character: t.character
                type: 'funject'
                patterns
            }
        if t = tokens.match 'identifier', 'formal parameter', 'string', 'number', 'boolean', 'nil', 'dot', 'unknown'
            return t

    pattern = (tokens) ->
        if t = tokens.match 'dot application'
            name = tokens.require 'identifier'
            match =
                type: 'list'
                line: t.line
                character: t.character
                values: [
                    {
                        type: 'dot'
                        value: '.'
                        line: t.line
                        character: t.character
                    }
                    {
                        type: 'string'
                        value: name.value
                        line: name.line
                        character: name.character
                    }
                ]
        else
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
            when 'number' then ['Number', n.value]
            when 'string' then ['String', n.value]
            when 'boolean' then ['Boolean', n.value]
            when 'nil' then ['Nil']
            when 'dot' then ['Dot']
            when 'unknown' then ['Unknown']
            when 'identifier' then ['Identifier', n.value]
            when 'formal parameter' then ['Parameter', n.value]
            when 'list' then ['List', transform x for x in n.values]
            when 'funject' then ['Funject', [transform(p.pattern), transform(p.value)] for p in n.patterns]
            when 'sequence' then ['Sequence', transform x for x in n.expressions]
            when 'application' then ['Invocation', transform(n.funject), transform(n.argument)]
            when 'assignment'
                switch n.operator
                    when 'strict assignment'
                        if n.left.type is 'application'
                            ['Funject-strict-assignment', transform(n.left), transform(n.right)]
                        else
                            ['Strict-assignment', transform(n.left), transform(n.right)]
                    when 'lazy assignment'
                        if n.left.type is 'application'
                            ['Funject-lazy-assignment', transform(n.left), transform(n.right)]
                        else
                            ['Lazy-assignment', transform(n.left), transform(n.right)]
                    when 'reset strict assignment' then ['Reset-strict-assignment', transform(n.left), transform(n.right)]
                    when 'reset lazy assignment' then ['Reset-lazy-assignment', transform(n.left), transform(n.right)]
                    when 'inheritance assignment' then ['Funject-inheritance', transform(n.left), transform(n.right)]
                    when 'inverse assignment' then ['Inverse-definition', transform(n.left), transform(n.right)]

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

if module?
    exports.parse = parse
    exports.tokenizer = tokenizer
    exports.parseForRacket = parseForRacket
    if not module.parent
        expression = null
        racket = false
        i = 2
        tokens = false
        argc = process.argv.length
        while i < argc
            switch arg = process.argv[i++]
                when '-r' then racket = true
                when '-t' then tokens = true
                when '-e'
                    expression = process.argv[i++]
                    break
                else
                    expression = '' + require('fs').readFileSync arg
                    break
        if i < argc or not expression?
            console.error 'Usage: coffee parser.coffee [ -r | -t ] [ <filename> | -e <expression> ]'
            return
        try
            if tokens
                printTokens expression
            else
                p = if racket then parseForRacket else parse
                console.log JSON.stringify p(expression), undefined, 2
        catch e
            if e instanceof SyntaxError
                console.error e.message
            else
                throw e
