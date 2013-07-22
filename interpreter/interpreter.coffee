util = require 'util'
readline = require 'readline'

environment = global ? @
parser = require './parser.coffee'

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
            if a.values.length isnt b.values.length
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
            @vars[name] = value

    reset: (name, value) ->
        if Object::hasOwnProperty.call @vars, name
            return @vars[name] = value
        if @parent
            return @parent.set name, value
        throw new InterpreterError "Can't reset undefined variable #{name}"

class Funject
    type: 'funject'

    toString: ->
        if @name?
            @name
        else
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
                    '#<funject>'
            catch
                '#<funject>'

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
                    if p.type is 'list' and p.values.length is 1
                        answer false
                    if p.type is 'list' and p.values.length is 2
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

    native: (pattern, argument) ->
        if pattern instanceof Array
            if pattern[0] is '|'
                for p in pattern[1..]
                    if args = @native p, argument
                        return args
                return false
            args = []
            if not argument.isList or pattern.length isnt argument.values.length
                return false
            for x, i in argument.values
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
            return false unless argument.isList and argument.values.length is pattern.values.length
            for x, i in pattern.values
                return false unless sub = @scan scope, bindings, applications, x, argument.values[i]
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
        if argument.type is 'symbol'
            return new SymbolFunject argument.value
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
            for p, i in @patterns.slice interpreter.frame.index ? 0
                applications = []
                if interpreter.frame.bindings
                    bindings = interpreter.frame.bindings
                    applications = interpreter.frame.applications
                    interpreter.frame.applications = null
                    interpreter.frame.bindings = null
                else
                    bindings = {}
                    continue unless @scan p.scope, bindings, applications, p.pattern, argument
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

        if instance and argument.isSymbol and @instance and @instance.hasSymbol argument.value
            self = @
            interpreter.pop()
            interpreter.push
                type: 'native'
                value: ->
                    method = @first()
                    hasArgs = method.methodHasArgs()
                    if hasArgs
                        @return new Funject
                            call: ['interpreter', '*', (interpreter, arg) ->
                                interpreter.pop()
                                interpreter.push
                                    type: 'application'
                                    funject:
                                        type: 'value'
                                        value: method
                                    argument:
                                        type: 'value'
                                        value: new ListFunject [self, arg]
                                SPECIAL_FORM]
                    else
                        @pop()
                        @push
                            type: 'application'
                            funject:
                                type: 'value'
                                value: method
                            argument:
                                type: 'value'
                                value: new ListFunject [self]
            interpreter.push
                type: 'application'
                funject:
                    type: 'value'
                    value: @instance
                argument:
                    type: 'value'
                    value: argument
                instance: false
            return

        return @parent.apply interpreter, own, argument if @parent and @parent isnt @

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

yakObject = (parent, properties) ->
    call = []
    if properties
        for k, v of properties
            do (v) ->
                call.push '.' + k, -> v
    return new Funject {call, parent}

yakFunction = (pattern, value) ->
    return new Funject call: [pattern, value]

yakClass = ({exports, instance}) ->
    ((exports ?= new Funject).call ?= []).unshift(
        '.instance', -> instance,
        '.new', -> throw new InterpreterError "Unimplemented")
    exports.$instance = instance
    exports

yakBoolean = (value) -> lang[!!value]

lang = {}

BaseFunject = yakObject null,
    initialize: yakFunction ['*'], (x) -> lang.nil
    equals: yakFunction ['*', '*'], (x, y) -> yakBoolean equal x, y
    on: yakFunction ['*', '*'], (x, y) -> lazy:
            type: 'application'
            funject:
                type: 'value'
                value: y
            argument:
                type: 'value'
                value: x
    'symbol?': yakFunction ['*'], (x) -> yakBoolean x.isSymbol
    'string?': yakFunction ['*'], (x) -> yakBoolean x.isString
    'number?': yakFunction ['*'], (x) -> yakBoolean x.isNumber
    'list?': yakFunction ['*'], (x) -> yakBoolean x.isList
    'boolean?': yakFunction ['*'], (x) -> yakBoolean x.isBoolean
    'nil?': yakFunction ['*'], (x) -> yakBoolean x.isNil
    'unknown?': yakFunction ['*'], (x) -> yakBoolean x.isUnknown
    'to-string': yakFunction ['*'], (x) -> new StringFunject '' + x

Funject::instance = BaseFunject
BaseFunject.instance = null

lang.Funject = yakClass
    instance: BaseFunject

lang.Symbol = yakClass
    instance: yakObject BaseFunject

lang.String = yakClass
    instance: yakObject BaseFunject,
        '+': new Funject
            call: [['string', 'string'], (x, y) -> new StringFunject x.value + y.value]

lang.Number = yakClass
    instance: yakObject BaseFunject,
        '+': new Funject
            call: [
                ['number', 'number'], (x, y) -> new NumberFunject x + y]
            inverse: new Funject
                call: [
                    ['number', ['|', ['number', 'unknown'], ['unknown', 'number']]], (r, x) ->
                        new ListFunject [new NumberFunject r - x]]
        '-': new Funject
            call: [
                ['number', 'number'], (x, y) -> new NumberFunject x - y]
            inverse: new Funject
                call: [
                    ['number', ['number', 'unknown']], (r, x) ->
                        new ListFunject [new NumberFunject x - r],
                    ['number', ['unknown', 'number']], (r, x) ->
                        new ListFunject [new NumberFunject r + x]]
        '*': new Funject
            call: [
                ['number', 'number'], (x, y) => new NumberFunject x * y]
            inverse: new Funject
                call: [
                    ['number', ['|', ['number', 'unknown'], ['unknown', 'number']]], (r, x) =>
                        new ListFunject [new NumberFunject r / x]]
        '/': new Funject
            call: [
                ['number', 'number'], (x, y) => new NumberFunject x / y]
            inverse: new Funject
                call: [
                    ['number', ['number', 'unknown']], (r, x) =>
                        new ListFunject [new NumberFunject x / r],
                    ['number', ['unknown', 'number']], (r, x) =>
                        new ListFunject [new NumberFunject r * x]]

lang.List = yakClass
    instance: yakObject BaseFunject,
        head: yakFunction ['list'], (x) ->
            if x.values.length
                x.values[0]
            else
                throw new InterpreterError 'Cannot take the head of the empty list'
        tail: yakFunction ['list'], (x) ->
            if x.values.length > 1
                new ListFunject x.values[1..]
            else
                lang.nil
        'empty?': yakFunction ['list'], (x) ->
            yakBoolean x.values.length is 0

lang.Boolean = yakClass
    instance: yakObject BaseFunject

class SymbolFunject extends Funject
    instance: lang.Symbol.$instance
    type: 'symbol'
    isSymbol: true

    constructor: (@value) ->
    toString: -> "." + @value

class StringFunject extends Funject
    instance: lang.String.$instance
    type: 'string'
    isString: true

    constructor: (@value) ->

    # The [] are there to avoid a syntax highlighting bug
    toString: -> "'" + @value.replace(/[\\]/g, '\\\\').replace(/'/g, "\\'").replace(/\n/g, '\\n').replace(/\r/g, '\\r').replace(/\t/g, '\\t') + "'"

class NumberFunject extends Funject
    instance: lang.Number.$instance
    type: 'number'
    isNumber: true

    constructor: (@value) ->
    toString: -> '' + @value
    valueOf: -> @value

class ListFunject extends Funject
    instance: lang.List.$instance
    type: 'list'
    isList: true

    constructor: (@values) ->
    toString: -> "[#{@values.join ', '}]"

class BooleanFunject extends Funject
    instance: lang.Boolean.$instance
    type: 'boolean'
    isBoolean: true

    constructor: (value) -> @value = !!value
    toString: -> '' + @value

lang.nil = new Funject
    type: 'nil'
    isNil: true
    toString: -> 'nil'
lang.unknown = new Funject
    type: 'unknown'
    isUnknown: true
    toString: -> 'unknown'
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

globalScope.set '+', new SymbolFunject '+'
globalScope.set '-', new SymbolFunject '-'
globalScope.set '*', new SymbolFunject '*'
globalScope.set '/', new SymbolFunject '/'

globalScope.set 'cons', new Funject
    call: [
        ['*', '*'], (x, y) -> new ListFunject [x, y]]
    inverse: new Funject
        call: [
            [['*', '*'], ['unknown', '*']], (x, y) -> new ListFunject [x]
            [['*', '*'], ['*', 'unknown']], (x, y) -> new ListFunject [y]]

globalScope.set 'square', new Funject
    call: [
        ['number'], (x) -> new NumberFunject x * x]
    inverse: new Funject
        call: [
            ['number', ['unknown']], (x) -> new ListFunject [new NumberFunject Math.sqrt x]]

globalScope.set 'error', new Funject
    call: [['string'], (message) -> throw new InterpreterError message.value]

globalScope.set 'print', new Funject
    call: [['string'], (message) ->
        console.log message.value
        lang.nil]

globalScope.set 'debug', new Funject
    call: [['*'], (thing) ->
        console.log '' + thing
        lang.nil]

itself = (n) -> @return lang[n.type]
variable = (n) -> @return @scope.get n.value

class Interpreter
    expressions:
        number: (n) -> @return new NumberFunject n.value
        symbol: (n)-> @return new SymbolFunject n.value
        string: (n) -> @return new StringFunject n.value
        boolean: (n) -> @return lang[n.value]
        nil: itself
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
            f.patterns = (for p in n.patterns
                pattern: p.pattern
                expression: p.value
                scope: @scope)
            @return f
        module: (n) ->
            p = n.parent?
            return if p and not @args n.parent
            argc = if p then 1 else 0
            if @frame.arguments.length is argc
                @frame.super = if p then @first() else lang.Funject
                @push type: 'pop scope'
                @scope = @frame.scope = new Scope @scope,
                    exports: new Funject parent: @frame.super
                    super: @frame.super
                if p
                    @args n.parent, n.body
                else
                    @args n.body
                return
            exports = @frame.scope.vars.exports
            exports.parent = @frame.super
            @return exports
        class: (n) ->
            p = n.parent?
            return if p and not @args n.parent
            argc = if p then 1 else 0
            if @frame.arguments.length is argc
                @frame.super = if p then @first() else lang.Funject
                superInstance =
                    type: 'application'
                    funject:
                        type: 'value'
                        value: @frame.super
                    argument:
                        type: 'symbol'
                        value: 'instance'
                @args (if p then [n.parent, superInstance] else [superInstance])...
                return
            if @frame.arguments.length is argc + 1
                @frame.superInstance = if p then @second() else @first()
                @push type: 'pop scope'
                @scope = @frame.scope = new Scope @scope,
                    exports: new Funject parent: @frame.super
                    super: @frame.super
                    instance: new Funject parent: @frame.superInstance
                if p
                    @args 0, 0, n.body
                else
                    @args 0, n.body
                return
            exports = @frame.scope.vars.exports
            exports.parent = @frame.super
            instance = @frame.scope.vars.instance
            instance.parent = @frame.superInstance
            prototype = yakObject null,
                class: exports
            (exports.call ?= []).unshift(
                '.instance', -> instance
                '.new', -> lazy:
                    type: 'native'
                    value: ->
                        initializeCall =
                            type: 'application'
                            instance: false
                            funject:
                                type: 'value'
                                value: instance
                            argument:
                                type: 'symbol'
                                value: 'initialize'
                        return unless @args initializeCall
                        initialize = @first()
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
                                funject:
                                    type: 'value'
                                    value: initialize
                                argument:
                                    type: 'value'
                                    value: new ListFunject [result]
            )
            @return exports
        application: (n) ->
            return unless @args n.funject, n.argument
            if @frame.arguments.length > 2
                return @return @frame.arguments[2]
            funject = @first()
            argument = @second()
            funject.apply @, n.own ? funject, argument, n.instance

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
                    console.log util.inspect @stack, depth: 10
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
    third: (n) -> @frame.arguments[2]

evaluate = (s) ->
    new Interpreter().evaluate parser.parse s

repl = ->
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

    rl.setPrompt '> '
    rl.prompt()

    read = ''
    sigints = 0
    rl.on 'line', (line) ->
        sigints = 0
        read += line + '\n'
        if not /^[\t ]|(^|[(\[])(class|module)\b|\[[^\]]*$|\([^)]*$|\{[^\}]*$/.test line
            try
                console.log '' + evaluate read
            catch e
                if e instanceof RuntimeError
                    console.log e.stack
                else if e instanceof SyntaxError
                    console.log e.message
                else
                    throw e
            read = ''
            rl.setPrompt '> '
            rl.prompt()
        else
            rl.setPrompt '? '
            rl.prompt()
            # TODO this breaks pasting
            # if s = /(^|\n)([ \t]+).*\n$/.exec read
            #     rl.write s[2]

    rl.on 'SIGINT', ->
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
        rl.setPrompt '> '
        rl.prompt()

    rl.on 'close', ->
        console.log '' # put a newline after the last prompt
        process.exit 0

if module?
    exports.Funject = Funject
    exports.repl = repl
    exports.evaluate = evaluate
    if not module.parent
        expressions = []
        i = 2
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
                    expressions.push process.argv[i++]
                else
                    expressions.push '' + require('fs').readFileSync arg
        for expression in expressions
            try
                evaluate expression
            catch e
                if e instanceof SyntaxError
                    console.error e.message
                else
                    throw e
        if interactive
            repl()
