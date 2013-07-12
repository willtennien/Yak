environment = global ? @
parser = require './parser.coffee'

last = (thing) -> thing[thing.length - 1]

extend = (object, properties) ->
    object[key] = value for key, value of properties
    object

equal = (a, b) ->
    if a.type isnt b.type
        return false
    switch a.type
        when 'list'
            if a.values.length isnt b.values.length
                return false
            for x, i in a
                if not equal x, b[i]
                    return false
            return true
        when 'funject'
            return a is b
        when 'number', 'boolean', 'string'
            return a.value is b.value
        when 'nil', 'dot', 'unknown'
            return true

class InterpreterError
    constructor: (@message) ->

class RuntimeError
    constructor: (@stack) ->

class MatchError extends InterpreterError
    constructor: (funject, argument) ->
        super "No match for #{funject} applied to #{argument}"

class Scope
    constructor: (@parent, vars) ->
        @vars = Object.create null
        if vars?
            for name, value of vars
                @vars[name] = value

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
            @vars[name] = value

    reset: (name, value) ->
        if Object::hasOwnProperty.call @vars, name
            return @vars[name] = value
        if @parent
            return @parent.set name, value
        throw new InterpreterError "Can't reset undefined variable #{name}"

class Funject
    type: 'funject'

    apply: (interpreter, own, argument) ->
        throw new MatchError own, argument

    toString: -> '#<funject>'

class UserFunject extends Funject
    constructor: (@patterns = []) ->

    scan: (applications, pattern, argument) ->
        if pattern.type is 'list'
            bindings = {}
            return false unless argument.isList and argument.values.length is pattern.values.length
            for x, i in pattern.values
                return false unless sub = @scan applications, x, argument.values[i]
                bindings = extend bindings, sub
            return bindings
        if pattern.type is 'formal parameter'
            bindings = {}
            bindings[pattern.value] = argument
            return bindings
        if pattern.type is 'application'
            applications.push
                funject: pattern.funject
                argument: pattern.argument
                value: argument
            return {}
        if pattern.type is 'number' or pattern.type is 'string' or pattern.type is 'boolean'
            return pattern.type is argument.type and pattern.value is argument.value and {}
        if pattern.type is 'dot' or pattern.type is 'nil' or pattern.type is 'unknown'
            return pattern.type is argument.type and {}
        throw new InterpreterError "Invalid pattern"

    match: (interpreter, bound, pattern, argument) ->
        if pattern.type is 'list'
            bindings = {}
            for x, i in pattern.values
                return false unless sub = @match interpreter, bound, x, argument.values[i]
                extend bindings, sub
            return bindings
        if pattern.type is 'application'
            arg = interpreter.first().values[interpreter.frame.arg++]
            if not arg.isList
                throw new InterpreterError 'Invalid inverse'
            return false unless arg.values.length
            return @bind arg.values[0], pattern, {}, bound
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
            return new ListFunject (@substitute bindings, x for x in argument.values)
        if argument.type is 'number'
            return new NumberFunject argument.value
        if argument.type is 'string'
            return new StringFunject argument.value
        if argument.type is 'formal parameter'
            return constant.unknown
        if argument.type is 'application'
            throw new InterpreterError 'Nested applications are unimplemented'
        if argument.type is 'boolean' or argument.type is 'dot' or argument.type is 'nil' or argument.type is 'unknown'
            return constant[argument.value]

    apply: (interpreter, own, argument) ->
        for p, i in @patterns.slice interpreter.frame.index ? 0
            applications = []
            if interpreter.frame.bindings
                bindings = interpreter.frame.bindings
                applications = interpreter.frame.applications
                interpreter.frame.applications = null
                interpreter.frame.bindings = null
            else
                continue unless bindings = @scan applications, p.pattern, argument
            if applications.length
                if interpreter.frame.step is 'match'
                    continue unless bound = @match interpreter, bindings, p.pattern, argument
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
                        arg: 0
                        index: i
                    }
                    interpreter.push
                        line: p.pattern.line
                        character: p.pattern.character
                        type: 'list'
                        values:
                            for a, i in interpreter.first().values
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
                        index: i
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
                interpreter.scope = new Scope p.scope, bindings
                interpreter.push { type: 'set scope', scope }
                interpreter.push p.expression
            return

        return @parent.apply interpreter, own, argument if @parent

        throw new MatchError own, argument

class PrimitiveFunject extends Funject
    constructor: (properties) ->
        if properties
            @[key] = value for key, value of properties

    match: (pattern, argument) ->
        if pattern instanceof Array
            args = []
            if not argument.isList or pattern.length isnt argument.values.length
                return false
            for x, i in argument.values
                if a = @match pattern[i], x
                    args = args.concat a
                else
                    return false
            return args
        if pattern is '*'
            return [argument]
        if constant[pattern]
            if argument is constant[pattern]
                return []
            else
                return false
        if typeof pattern is 'string'
            if pattern[0] is '.'
                return @match ['dot', '"' + pattern.substr 1], argument
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

    apply: (interpreter, own, argument) ->
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
                if args = @match pattern, argument
                    if provideSelf
                        args.unshift own
                    if provideInterpreter
                        args.unshift interpreter
                    return interpreter.return value.apply @, args

        return @parent.apply interpreter, own, argument if @parent

        throw new MatchError own, argument

class BaseFunject extends PrimitiveFunject
    parent: null

    call: [
        'own', '.equals', (own) -> new PrimitiveFunject
            call: [
                ['*'], (x) -> new BooleanFunject equal own, x]]

Funject::parent = new BaseFunject

class StringFunject extends PrimitiveFunject
    type: 'string'
    isString: true

    constructor: (@value) ->

    ###
    call: [
        'own', '&+', (own) -> new PrimitiveFunject
            call: [
                'string', (x) -> new StringFunject own.value + x.value]]
    ###

    # The [] are there to avoid a syntax highlighting bug
    toString: -> "'" + @value.replace(/[\\]/g, '\\\\').replace(/'/g, "\\'").replace(/\n/g, '\\n').replace(/\r/g, '\\r').replace(/\t/g, '\\t') + "'"

class NumberFunject extends PrimitiveFunject
    type: 'number'
    isNumber: true

    constructor: (@value) ->

    call: [
        'own', '&+', (own) -> new PrimitiveFunject
            call: [
                'number', (x) -> new NumberFunject own + x]
            inverse: new PrimitiveFunject
                call: [
                    ['number', 'unknown'], (x) ->
                        new ListFunject [new NumberFunject x - own]]
        'own', '&-', (own) -> new PrimitiveFunject
            call: [
                'number', (x) -> new NumberFunject own - x]
            inverse: new PrimitiveFunject
                call: [
                    ['number', 'unknown'], (x) ->
                        new ListFunject [new NumberFunject x - own]]
        'own', '&*', (own) -> new PrimitiveFunject
            call: [
                'number', (x) => new NumberFunject own * x]
            inverse: new PrimitiveFunject
                call: [
                    ['number', 'unknown'], (x) =>
                        new ListFunject [new NumberFunject x / own]]
        'own', '&/', (own) -> new PrimitiveFunject
            call: [
                'number', (x) => new NumberFunject own / x]
            inverse: new PrimitiveFunject
                call: [
                    ['number', 'unknown'], (x) =>
                        new ListFunject [new NumberFunject own / x]]
        'own', '.plus', (own) -> new PrimitiveFunject
            call: [
                ['number'], (x) -> new NumberFunject own + x]
            inverse: new PrimitiveFunject
                call: [
                    ['number', ['unknown']], (x) ->
                        new ListFunject [new NumberFunject x - own]]
        'own', '.minus', (own) -> new PrimitiveFunject
            call: [
                ['number'], (x) -> new NumberFunject own - x]
            inverse: new PrimitiveFunject
                call: [
                    ['number', ['unknown']], (x) ->
                        new ListFunject [new NumberFunject x - own]]
        'own', '.times', (own) -> new PrimitiveFunject
            call: [
                ['number'], (x) => new NumberFunject own * x]
            inverse: new PrimitiveFunject
                call: [
                    ['number', ['unknown']], (x) =>
                        new ListFunject [new NumberFunject x / own]]
        'own', '.div', (own) -> new PrimitiveFunject
            call: [
                ['number'], (x) => new NumberFunject own / x]
            inverse: new PrimitiveFunject
                call: [
                    ['number', ['unknown']], (x) =>
                        new ListFunject [new NumberFunject own / x]]]

    toString: -> '' + @value
    valueOf: -> @value

class ListFunject extends PrimitiveFunject
    type: 'list'
    isList: true

    constructor: (@values) ->

    toString: -> "[#{@values.join ', '}]"

class BooleanFunject extends PrimitiveFunject
    type: 'boolean'
    isBoolean: true

    constructor: (@value) ->

    toString: -> '' + @value

class NilFunject extends PrimitiveFunject
    type: 'nil'
    isNil: true

    toString: -> 'nil'

class DotFunject extends PrimitiveFunject
    type: 'dot'
    isDot: true

    toString: -> 'dot'

class UnknownFunject extends PrimitiveFunject
    type: 'unknown'
    isUnkown: true

    toString: -> 'unknown'

constant =
    nil: new NilFunject
    dot: new DotFunject
    unknown: new UnknownFunject
    true: new BooleanFunject true
    false: new BooleanFunject false

Funject.bridge = (v, context = environment) ->
    if not v?
        return constant.nil
    if v instanceof Array
        return new ListFunject (Funject.bridge x for x in v)
    switch typeof v
        when 'number' then new NumberFunject v
        when 'string' then new StringFunject v
        when 'boolean' then constant[v]
        when 'function' then new PrimitiveFunject
            call: [
                'list', (list) -> Funject.bridge v.apply context, Funject.unbridge list]
        when 'object' then new PrimitiveFunject
            call: [
                ['dot', 'string'], (property) -> Funject.bridge v[property.value], v]

Funject.unbridge = (f) ->
    switch f.type
        when 'nil' then null
        when 'dot', 'unknown' then throw new InterpreterError "Cannot unbridge #{f.type}"
        when 'number', 'string', 'boolean' then f.value
        when 'list' then Funject.unbridge v for v in f.values
        when 'funject' then ->
            Funject.unbridge new Interpreter().evaluate
                type: 'application'
                funject:
                    type: 'value'
                    value: f
                argument:
                    type: 'value'
                    value: Funject.bridge [].slice.call arguments

globalScope = new class extends Scope
    name: '<global scope>'

    get: (name) ->
        try
            super name
        catch e
            if Object::hasOwnProperty.call environment, name
                Funject.bridge environment[name]
            else
                throw e

globalScope.set '+', new PrimitiveFunject
    call: [
        ['number', 'number'], (x, y) -> new NumberFunject x + y]
    inverse: new PrimitiveFunject
        call: [
            ['number', ['number', 'unknown']], (x, y) -> new ListFunject [new NumberFunject x - y],
            ['number', ['unknown', 'number']], (x, y) -> new ListFunject [new NumberFunject x - y]]

globalScope.set '-', new PrimitiveFunject
    call: [
        ['number'], (x) -> new NumberFunject -x
        'number', (x) -> new NumberFunject -x
        ['number', 'number'], (x, y) -> new NumberFunject x - y]
    inverse: new PrimitiveFunject
        call: [
            ['number', ['number', 'unknown']], (x, y) -> new ListFunject [new NumberFunject y - x]
            ['number', ['unknown', 'number']], (x, y) -> new ListFunject [new NumberFunject x + y]]

globalScope.set '*', new PrimitiveFunject
    call: [
        ['number', 'number'], (x, y) -> new NumberFunject x * y]
    inverse: new PrimitiveFunject
        call: [
            ['number', ['number', 'unknown']], (x, y) -> new ListFunject [new NumberFunject x / y]
            ['number', ['unknown', 'number']], (x, y) -> new ListFunject [new NumberFunject x / y]]

globalScope.set '/', new PrimitiveFunject
    call: [
        ['number', 'number'], (x, y) -> new NumberFunject x / y]
    inverse: new PrimitiveFunject
        call: [
            ['number', ['number', 'unknown']], (x, y) -> new ListFunject [new NumberFunject y / x]
            ['number', ['unknown', 'number']], (x, y) -> new ListFunject [new NumberFunject x * y]]

globalScope.set 'cons', new PrimitiveFunject
    call: [
        ['*', '*'], (x, y) -> new ListFunject [x, y]]
    inverse: new PrimitiveFunject
        call: [
            [['*', '*'], ['unknown', '*']], (x, y) -> new ListFunject [x]
            [['*', '*'], ['*', 'unknown']], (x, y) -> new ListFunject [y]]

globalScope.set 'square', new PrimitiveFunject
    call: [
        ['number'], (x) -> new NumberFunject x * x]
    inverse: new PrimitiveFunject
        call: [
            ['number', ['unknown']], (x) -> new ListFunject [new NumberFunject Math.sqrt x]]

globalScope.set 'error', new PrimitiveFunject
    call: [['string'], (message) -> throw new InterpreterError message.value]

itself = (n) -> @return constant[n.type]
variable = (n) -> @return @scope.get n.value

class Interpreter
    expressions:
        number: (n) -> @return new NumberFunject n.value
        string: (n) -> @return new StringFunject n.value
        boolean: (n) -> @return constant[n.value]
        nil: itself
        dot: itself
        unknown: itself

        value: (n) -> @return n.value

        'pop scope': (n) ->
            @scope = @scope.parent
            if @frame.arguments.length
                @return @first()
            else
                @pop()

        'set scope': (n) ->
            @scope = n.scope
            if @frame.arguments.length
                @return @first()
            else
                @pop()

        'reapply funject': (n) ->
            n.funject.apply @, n.own, n.argument

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
                    return @return @first().parent = @second()
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
                        return @return constant.nil
                    when 'reset lazy assignment'
                        @scope.reset n.left.value,
                            lazy: n.right
                            scope: @scope
                        return @return constant.nil
            if n.left.type is 'application'
                return unless @args n.left.funject
                funject = @first()
                if n.operator isnt 'lazy assignment' and @frame.arguments.length is 1
                    @push n.right
                    return
                funject.patterns.unshift (
                    if n.operator is 'lazy assignment'
                        pattern: n.left.argument
                        expression: n.right
                        scope: @scope
                    else
                        pattern: n.left.argument
                        value: @second())
                return @return (if n.operator is 'lazy assignment' then constant.nil else @second())
            throw new InterpreterError "Unimplemented: #{n.operator}"

        identifier: variable
        'formal parameter': variable
        list: (n) ->
            return unless @args n.values...
            @return new ListFunject @frame.arguments
        funject: (n) ->
            @return new UserFunject (for p in n.patterns
                pattern: p.pattern
                expression: p.value
                scope: @scope)
        application: (n) ->
            return unless @args n.funject, n.argument
            if @frame.arguments.length > 2
                return @frame.arguments[2]
            funject = @first()
            argument = @second()
            funject.apply @, funject, argument

    evaluate: (n) ->
        @scope = globalScope
        @stack = [ arguments: [] ]
        n.isProxy = true
        @push n
        while @stack.length > 1
            @frame = last @stack
            try
                @expression()
            catch e
                if e instanceof InterpreterError
                    n = @frame.expression
                    stack = "#{e.message} at :#{n.line}:#{n.character}"
                    scope = @scope
                    while scope and scope isnt globalScope
                        stack += "\n at #{(scope.name ? "") + (if scope.line? then ":#{scope.line}:#{scope.character}" else "")}"
                        scope = scope.parent
                    throw new RuntimeError stack
                else
                    console.log require('util').inspect @stack, depth: 10
                    throw e
        last @stack[0].arguments

    args: (args...) ->
        length = @frame.arguments.length
        if length and (value = @frame.arguments[length - 1]).lazy
            @frame.arguments.pop()
            scope = @scope
            @push { type: 'set scope', scope }
            @scope = value.scope
            @push value.lazy
            return false
        if length < args.length
            @push args[length]
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

evaluate = (s) ->
    new Interpreter().evaluate parser.parse s

repl = ->
    process.stdin.resume()
    process.stdin.setEncoding 'utf8'
    process.stdout.write '> '
    read = ''
    brackets = 0
    process.stdin.on 'data', (string) ->
        read += string
        brackets += (string.match(/[\[({]/g) || []).length - (string.match(/[})\]]/g) || []).length
        if brackets is 0
            try
                process.stdout.write '' + evaluate read
            catch e
                if e instanceof RuntimeError
                    process.stdout.write e.stack
                else if e instanceof SyntaxError
                    process.stdout.write e.message
                else
                    throw e
            read = ''
            process.stdout.write '\n> '
        else
            process.stdout.write '? '

if module?
    exports.Funject = Funject
    exports.repl = repl
    exports.evaluate = evaluate
    if not module.parent
        expression = null
        i = 2
        argc = process.argv.length
        while i < argc
            switch arg = process.argv[i++]
                when '-i'
                    break
                when '-e'
                    expression = process.argv[i++]
                    break
                else
                    expression = require('fs').readFileSync arg
                    break
        if i < argc
            console.error 'Usage: coffee interpreter.coffee [ <filename> | -e <expression> | -i ]'
            return
        if expression?
            try
                console.log '' + evaluate expression
            catch e
                if e instanceof SyntaxError
                    console.error e.message
                else
                    throw e
        else
            repl()
