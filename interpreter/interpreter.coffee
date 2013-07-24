util = require 'util'
readline = require 'readline'
fs = require 'fs'
path = require 'path'

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
            if e instanceof InterpreterError
                @basicToString()
            else
                throw e

    basicToString: ->
        if @call and @call.length isnt 0 then '#primitive' else '#funject'

    toSource: (depth) ->
        if depth is 0
            @simpleToString()
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
                s.value
            else
                @getSource()
        catch e
            if e instanceof InterpreterError
                @getSource depth
            else
                throw e

    getSource: (depth) ->
        if depth is 0
            @simpleToString()
        else if @call and @call.length isnt 0
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

    keys: ->
        result = []
        if @patterns
            for p in @patterns when p.pattern.type is 'symbol'
                result.push p.pattern.value
        if @call
            i = 0
            length = @call.length
            while i < length
                p = @call[i++]
                while p is 'interpreter' or p is 'own'
                    p = @call[i++]
                if p[0] is '.'
                    result.push p.substr 1
        result

    allKeys: ->
        result = {}
        f = @
        while f
            for k in f.keys()
                result[k] = true
            f = f.parent
        Object.keys result

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

yakSymbol = (value) ->
    if Object::hasOwnProperty.call SymbolFunject.instances, value
        SymbolFunject.instances[value]
    else
        SymbolFunject.instances[value] = new SymbolFunject value

lang = {}

BaseFunject = yakObject null,
    initialize: yakFunction ['*'], (x) -> lang.nil
    clone: yakFunction ['*'], (x) -> new Funject parent: x
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
    'integer?': yakFunction ['*'], (x) ->
        yakBoolean x.isNumber and x.isInteger()
    'float?': yakFunction ['*'], (x) ->
        yakBoolean x.isNumber and x.isFloat()
    'to-string': yakFunction ['*'], (x) ->
        if x.type is 'funject'
            new StringFunject Funject::basicToString.call x
        else
            new StringFunject '' + x
    'inspect': yakFunction ['*'], (x) -> new StringFunject x.toSource -1
    keys: yakFunction ['funject'], (f) ->
        new ListFunject (new StringFunject k for k in f.keys())
    'all-keys': yakFunction ['funject'], (f) ->
        new ListFunject (new StringFunject k for k in f.allKeys())

Funject::instance = BaseFunject
BaseFunject.instance = null

lang.Funject = yakClass
    instance: BaseFunject

lang.Symbol = yakClass
    instance: yakObject BaseFunject

lang.String = yakClass
    instance: yakObject BaseFunject,
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

lang.Number = yakClass
    exports: yakObject null,
        e: 2.718281828459045
        pi: 3.141592653589793
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
    instance: yakObject BaseFunject,
        'degrees-to-radians': yakFunction ['number'], (x) ->
            new NumberFunject x.value * Math.PI / 180
        'radians-to-degrees': yakFunction ['number'], (x) ->
        sqrt: yakFunction ['number'], (x) ->
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
        map: new Funject
            call: ['interpreter', ['list', ['funject']], (interpreter, x, f) ->
                i = 0
                list = x.values.slice 0
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
        each: new Funject
            call: ['interpreter', ['list', ['funject']], (interpreter, x, f) ->
                i = 0
                list = x.values.slice 0
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

lang.Boolean = yakClass
    instance: yakObject BaseFunject,
        not: yakFunction ['boolean'], (x) ->
            yakBoolean not x.value
        and: yakFunction ['boolean', 'boolean'], (x, y) ->
            yakBoolean x.value and y.value
        or: yakFunction ['boolean', 'boolean'], (x, y) ->
            yakBoolean x.value or y.value
        xor: yakFunction ['boolean', 'boolean'], (x, y) ->
            yakBoolean x.value isnt y.value

class SymbolFunject extends Funject
    @instances: {}

    instance: lang.Symbol.$instance
    type: 'symbol'
    isSymbol: true

    constructor: (@value) ->
    toString: -> "." + @value
    toSource: -> "." + @value

class StringFunject extends Funject
    instance: lang.String.$instance
    type: 'string'
    isString: true

    constructor: (@value) ->

    call: ['own', ['number'], (s, n) ->
        i = if n.value < 0 then s.value.length + n.value else n.value
        if i < 0 or i >= s.value.length
            lang.nil
        else
            new StringFunject s.value.charAt i]

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

class ListFunject extends Funject
    instance: lang.List.$instance
    type: 'list'
    isList: true

    constructor: (@values) ->
    toString: -> "[#{@values.join ', '}]"
    toSource: (depth) -> "[#{(v.toSource depth - 1 for v in @values).join ', '}]"

class BooleanFunject extends Funject
    instance: lang.Boolean.$instance
    type: 'boolean'
    isBoolean: true

    constructor: (value) -> @value = !!value
    toString: -> '' + @value
    toSource: -> '' + @value

lang.nil = new Funject
    type: 'nil'
    isNil: true
    toString: -> 'nil'
    toSource: -> 'nil'

lang.unknown = new Funject
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

class Interpreter
    expressions:
        number: (n) -> @return new NumberFunject n.value
        symbol: (n)-> @return yakSymbol n.value
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

        'pop call stack': ->
            @callStack.pop()
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
            f.expression = n
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
                @scope = @frame.scope = new Scope @scope, {
                    exports: new ClassFunject
                        parent: @frame.super
                        name: @frame.name
                        expression: n
                    super: @frame.super
                    instance: new Funject
                        parent: @frame.superInstance
                        name: "#{@frame.name}.instance"
                        expression: n
                }, true
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

    evaluate: (n) ->
        @scope = globalScope
        @stack = [ arguments: [] ]
        @callStack = []
        n.isProxy = true
        @push n
        while @stack.length > 1
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
                    throw new RuntimeError stack
                else
                    console.log util.inspect @stack, depth: 10
                    throw error
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

evaluate = (s, file, firstLine) ->
    new Interpreter().evaluate parser.parse s, file, firstLine

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

    replLine = 1
    FIXED_LINE_LENGTH = 4
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
    rl.on 'line', (line) ->
        ++replLine
        sigints = 0
        if line is '' and read is ''
            setPrompt '>'
            rl.prompt()
            firstLine = replLine
            return
        read += line + '\n'
        if not /^[\t ]|(^|[(\[])(class|module)\b|\[[^\]]*$|\([^)]*$|\{[^\}]*$/.test line
            try
                console.log evaluate(read, 'input', firstLine).toSource 3
            catch e
                if e instanceof RuntimeError
                    console.log e.stack
                else if e instanceof SyntaxError
                    console.log e.message
                else
                    throw e
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
    exports.repl = repl
    exports.evaluate = evaluate
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
                evaluate expression.source, expression.file
            catch e
                if e instanceof SyntaxError
                    console.error e.message
                else
                    throw e
        if interactive
            repl()
