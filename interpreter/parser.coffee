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

    isIdentifier = (c) -> -1 isnt '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-=?!'.indexOf c

    (s) ->
        token = (type, value = '') -> {
            type, value, line
            character: character }

        syntaxError = (message) ->
            throw new SyntaxError "#{message} at :#{line}:#{character}"

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
                when '\n'
                    loop
                        while c is '\n'
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
                        break unless c is '\n'
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
                        if value is '.'
                            return token 'dot application', '.'
                        character = 1 + i - lastNewline
                        syntaxError 'Expected digit'
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
                        c = s[i]
                        escape = true
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
        result = statements tokens
        tokens.require 'end'
        result

    statements = (tokens) ->
        n = tokens.here()
        result = []
        loop
            s = statement tokens
            break unless s
            result.push s
            break if not tokens.match 'newline'
        {
            line: n.line
            character: n.character
            type: 'sequence'
            statements: result
        }

    statement = (tokens) ->
        e = expression tokens, true
        return if not e
        if assignment = tokens.match 'strict assignment', 'lazy assignment', 'reset strict assignment', 'reset lazy assignment', 'inverse assignment', 'inheritance assignment'
            if e.type isnt 'identifier' and (e.type isnt 'application' or assignment.type is 'inheritance assignment' or assignment.type is 'reset strict assignment' or assignment.type is 'reset lazy assignment')
                parseError assignment, 'Invalid left-hand side of assignment'
            e =
                type: 'assignment'
                operator: assignment.type
                line: assignment.line
                character: assignment.character
                left: e
                right: expression tokens
        e

    expression = (tokens, optional) ->
        if t = tokens.match 'indent'
            result = statements tokens
            tokens.require 'outdent'
            return result
        e = value tokens
        if not e
            if optional
                return null
            else
                parseError tokens.here(), "Expected expression"
        loop
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
            if -1 isnt ['identifier', 'formal parameter', 'string', 'number', 'boolean', 'nil', 'dot', 'unknown', 'funject start', 'group start', 'list start'].indexOf tokens.here().type
                t = tokens.here()
                e =
                    type: 'application'
                    line: t.line
                    character: t.character
                    funject: e
                    argument: value tokens
                continue
            break
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
    if not module.parent
        expression = null
        i = 2
        argc = process.argv.length
        while i < argc
            switch arg = process.argv[i++]
                when '-e'
                    expression = process.argv[i++]
                    break
                else
                    expression = '' + require('fs').readFileSync arg
                    break
        if i < argc or not expression?
            console.error 'Usage: coffee parser.coffee [ <filename> | -e <expression> ]'
            return
        try
            console.log JSON.stringify parse(expression), undefined, 2
        catch e
            if e instanceof SyntaxError
                console.error e.message
            else
                throw e
