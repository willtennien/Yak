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
                    if p is 'symbol' or p[0] is '.' and name is p.substr 1
                        return true
            if method.patterns
                for p in method.patterns
                    if p.pattern.type is 'formal parameter' or p.pattern.type is 'symbol' and p.pattern.value is name
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

    apply: (interpreter, own, argument, _private, instance = true) ->
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
                                private: _private
                                funject: @
                                argument
                                own
                                instance
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
                                private: _private
                                funject: @
                                argument
                                own
                                instance
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
                    bindings.private = _private if _private
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
                                    private: own.private ?= new Funject
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
                            private: own.private ?= new Funject
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

        return @parent.apply interpreter, own, argument, _private, instance if @parent and @parent isnt @

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
    methods.isInstance = true
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
            else if name is 'require' and module?
                Funject.bridge require
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
            n.funject.apply @, n.own, n.argument, n.private, n.instance

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
                if funject.isInstance
                    throw new InterpreterError 'Cannot modify class definition'
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
            instance.isInstance = true
            instance.parent = @frame.super.$instance
            prototype = yakObject null,
                class: exports
            externalInstance = new Funject
                call: ['interpreter', 'symbol', (interpreter, s) ->
                    exp = interpreter.frame.expression
                    interpreter.pop()
                    interpreter.push
                        type: 'native'
                        value: ->
                            return unless @args {
                                type: 'application'
                                funject:
                                    type: 'value'
                                    value: instance
                                argument:
                                    type: 'value'
                                    value: s
                            }
                            method = @first()
                            method.name ?= 'instance.' + s.value
                            @return new Funject
                                call: ['interpreter', ['|', ['*'], ['*', '*']], (interpreter, x, y) ->
                                    interpreter.pop()
                                    interpreter.pop()
                                    interpreter.callStack.pop()
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
                                            value: new ListFunject (if y then [x, y] else [x])
                                        private: x.private ?= new Funject
                                    SPECIAL_FORM]
                    SPECIAL_FORM]
            (exports.call ?= []).unshift(
                '.instance', -> externalInstance,
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
                                            private: result.private ?= new Funject
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
                                    private: result.private ?= new Funject
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
            funject.apply @, n.own ? funject, argument, n.private, n.instance
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
