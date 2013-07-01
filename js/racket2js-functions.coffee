Symbol = (->
  prototypeOfSymbol = {}
  prototypeOfSymbol.toString = ->
    "'|" + String(@_val) + "|"

  prototypeOfSymbol.isSymbol = ->
    true

  Symbol = (str) ->
    result = Object.create(prototypeOfSymbol)
    result._val = str
    result

  symbols = {}
  (str) ->
    symbols[str] or (symbols[str] = Symbol(str))
)()

empty = []
cons = (x, ys) ->
  [x].concat ys

car = (xs) ->
  xs[0]

cdr = (xs) ->
  xs.slice 1

list = ->
  Array::slice.call arguments_, 0

not_ = (x) ->
  not x

string_to_number = Number
number_to_string = String
boolean_to_string = String

symbol_to_string = (sym) ->
  sym.toString()

is_equal = (a, b) -> a is b

is_eq = is_equal

apply = (f, args) -> f.apply({}, args)

display = newline = console.log

raise = (x) -> throw x

error = (x) -> throw x

#type functions
is_symbol = (x) -> x.isSymbol is true

is_string = (x) -> x.constructor is String

is_boolean = (x) -> x.constructor is Boolean

is_number = (x) -> x.constructor is Number

#string functions
matches = (a, b) -> b.indexOf(a) isnt -1

string_length = (str) -> str.length

string_append = () -> Array.prototype.reduce.call(arguments, (a, b) -> a + b)

string_replace = (str, a, b) -> str.replace(a, b)

substring = (str, start, end) -> str.slice(start, end)

#list functions
count = (f, xs) -> xs.filter(f).length

is_empty = (xs) -> xs.length is 0

append = () -> Array.prototype.reduce.call(arguments, (rest, next) -> rest.concat(next))

cadr = (xs) -> xs[1]
caddr = (xs) -> xs[2]
cadddr = (xs) -> xs[3]
caddddr = (xs) -> xs[4]
cadddddr = (xs) -> xs[5]
caddddddr = (xs) -> xs[6]
cadddddddr = (xs) -> xs[7]
caddddddddr = (xs) -> xs[8]
cadddddddddr = (xs) -> xs[9]
caddddddddddr = (xs) -> xs[10]
caar = (xs) -> xs[0][0]
caadr = (xs) -> xs[1][0]
caaddr = (xs) -> xs[2][0]
caadddr = (xs) -> xs[3][0]
caaddddr = (xs) -> xs[4][0]
caadddddr = (xs) -> xs[5][0]
caaddddddr = (xs) -> xs[6][0]
caadddddddr = (xs) -> xs[7][0]
caaddddddddr = (xs) -> xs[8][0]
caadddddddddr = (xs) -> xs[9][0]
caaddddddddddr = (xs) -> xs[10][0]

#interpreter:
possibility = (result, str) -> [[result, str]]

impossibility = () -> []

is_possible = (xs) -> 0 < xs.length

is_impossible = (xs) -> xs.length is 0

shallow_flatten = (xss) ->  xss.reduce(((ys, xs) -> ys.concat(xs)), [])

given = (possibilities, f) -> shallow_flatten(possibilities.map(([result, str]) -> f(result, str)))

also = append

given_seq = (->
    iter = (possibilities, procs) ->
        if procs.length is 0
            possibilities
        else
            given(possibilities, 
                  (_, str) -> 
                    iter(procs[0](str), procs[1..]))

    return (possibilities, procs...) -> iter(possibilities, Array.prototype.slice.call(procs, 0))

)()
    
repeat = (n, c) -> if n is 0 then '' else c + repeat(n - 1, c)
least_acceptable_indent = (indent, max) ->
    candidates = [0..max].filter(indent)
    if candidates.length is 0
        return false
    else
        return repeat(candidates[0], ' ')

compose = (f, others...) -> 
    if others.length is 0
        return f 
    else 
        others_composed = apply(compose, others)
        return () -> f(apply(others_composed, arguments))