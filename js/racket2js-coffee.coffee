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

assert = (cond, message) -> throw message unless cond

#type functions
is_symbol = (x) -> x.isSymbol is true

is_string = (x) -> x.constructor is String

is_boolean = (x) -> x.constructor is Boolean

is_number = (x) -> x.constructor is Number

#number functions
_racket_plus_symbol = (a, b) -> a + b

#racket minus symbol:
_ = (a, b) -> a - b

_racket_mult_symbol = (a, b) -> a * b

_racket_division_symbol = (a, b) -> a / b

#string functions
matches = (a, b) -> b.indexOf(a) isnt -1

string_length = (str) -> str.length

string_append = () -> Array.prototype.reduce.call(arguments, (a, b) -> a + b)

string_replace = (str, a, b) -> str.replace(a, b)

substring = (str, start, end) -> 
    console.log([str, start, end])
    str.slice(start, end)

#list functions
empty = []
cons = mcons = (a, b) -> [a, b]
car = mcar = (xs) -> xs[0]
cdr = mcdr = (xs) -> xs[1]
cadr = mcdar = (xs) -> xs[1][0]
append = mappend = (xs, ys) -> 
    if xs.length is 0
        return ys
    else
        return cons(xs[0], append(xs[1..], ys))
list = mlist = () -> 
    if arguments.length is 0
        return []
    else 
        return [arguments[0], mlist.apply({}, Array::slice.call(arguments, 1))]
length = mlength = (xs) ->
    if xs.length is 0
        0
    else
        1 + length(xs[1])
is_empty = (xs) -> xs.length is 0
mlist_to_list = list_to_mlist = (xs) -> xs
set_car_racket_exclamation_point = (xs, x) -> xs[0] = x
set_cdr_racket_exclamation_point = (xs, x) -> xs[1] = x
array2nested_pairs = (arr) -> 
    if arr.length is 0
        return []
    else
        return [arr[0], array2nested_pairs(arr[1..])]
nested_pairs2array = (xs) ->
    if xs.length is 0
        return []
    else
        return [xs[0]].concat(nested_pairs2array(xs[1]))
deep_list_to_mlist = deep_mlist_to_list = (x) -> x

###More efficient list functions, that assumes cdr always returns a list.
empty = []

list = mlist = ->
  Array::slice.call arguments_, 0

count = (f, xs) -> xs.filter(f).length

is_empty = (xs) -> xs.length is 0

append = () -> Array.prototype.reduce.call(arguments, (rest, next) -> rest.concat(next))

car = (xs) -> xs[0]
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
cdr = (xs) -> xs.slice 1
cddr = (xs) -> xs.slice 2
cdddr = (xs) -> xs.slice 3
cddddr = (xs) -> xs.slice 4
cdddddr = (xs) -> xs.slice 5
cddddddr = (xs) -> xs.slice 6
cdddddddr = (xs) -> xs.slice 7
cddddddddr = (xs) -> xs.slice 8
cdddddddddr = (xs) -> xs.slice 9
cddddddddddr = (xs) -> xs.slice 10
cdddddddddddr = (xs) -> xs.slice 11
cddddddddddddr = (xs) -> xs.slice 12
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
set_cadr_racket_exclamation_point = (xs, x) -> xs[1] = x
set_caddr_racket_exclamation_point = (xs, x) -> xs[2] = x
set_cadddr_racket_exclamation_point = (xs, x) -> xs[3] = x
set_caddddr_racket_exclamation_point = (xs, x) -> xs[4] = x
set_cadddddr_racket_exclamation_point = (xs, x) -> xs[5] = x
set_caddddddr_racket_exclamation_point = (xs, x) -> xs[6] = x
set_cadddddddr_racket_exclamation_point = (xs, x) -> xs[7] = x
set_caddddddddr_racket_exclamation_point = (xs, x) -> xs[8] = x
set_cadddddddddr_racket_exclamation_point = (xs, x) -> xs[9] = x
set_caddddddddddr_racket_exclamation_point = (xs, x) -> xs[10] = x
set_caar_racket_exclamation_point = (xs, x) -> xs[0][0] = x
set_caadr_racket_exclamation_point = (xs, x) -> xs[1][0] = x
set_caaddr_racket_exclamation_point = (xs, x) -> xs[2][0] = x
set_caadddr_racket_exclamation_point = (xs, x) -> xs[3][0] = x
set_caaddddr_racket_exclamation_point = (xs, x) -> xs[4][0] = x
set_caadddddr_racket_exclamation_point = (xs, x) -> xs[5][0] = x
set_caaddddddr_racket_exclamation_point = (xs, x) -> xs[6][0] = x
set_caadddddddr_racket_exclamation_point = (xs, x) -> xs[7][0] = x
set_caaddddddddr_racket_exclamation_point = (xs, x) -> xs[8][0] = x
set_caadddddddddr_racket_exclamation_point = (xs, x) -> xs[9][0] = x
set_caaddddddddddr_racket_exclamation_point = (xs, x) -> xs[10][0] = x



mcadr = cadr
mcaddr = caddr
mcadddr = cadddr
mcaddddr = caddddr
mcadddddr = cadddddr
mcaddddddr = caddddddr
mcadddddddr = cadddddddr
mcaddddddddr = caddddddddr
mcadddddddddr = cadddddddddr
mcaddddddddddr = caddddddddddr
mcaar = caar
mcaadr = caadr
mcaaddr = caaddr
mcaadddr = caadddr
mcaaddddr = caaddddr
mcaadddddr = caadddddr
mcaaddddddr = caaddddddr
mcaadddddddr = caadddddddr
mcaaddddddddr = caaddddddddr
mcaadddddddddr = caadddddddddr
mcaaddddddddddr = caaddddddddddr
set_mcadr_racket_exclamation_point = set_cadr_racket_exclamation_point
set_mcaddr_racket_exclamation_point = set_caddr_racket_exclamation_point
set_mcadddr_racket_exclamation_point = set_cadddr_racket_exclamation_point
set_mcaddddr_racket_exclamation_point = set_caddddr_racket_exclamation_point
set_mcadddddr_racket_exclamation_point = set_cadddddr_racket_exclamation_point
set_mcaddddddr_racket_exclamation_point = set_caddddddr_racket_exclamation_point
set_mcadddddddr_racket_exclamation_point = set_cadddddddr_racket_exclamation_point
set_mcaddddddddr_racket_exclamation_point = set_caddddddddr_racket_exclamation_point
set_mcadddddddddr_racket_exclamation_point = set_cadddddddddr_racket_exclamation_point
set_mcaddddddddddr_racket_exclamation_point = set_caddddddddddr_racket_exclamation_point
set_mcaar_racket_exclamation_point = set_caar_racket_exclamation_point
set_mcaadr_racket_exclamation_point = set_caadr_racket_exclamation_point
set_mcaaddr_racket_exclamation_point = set_caaddr_racket_exclamation_point
set_mcaadddr_racket_exclamation_point = set_caadddr_racket_exclamation_point
set_mcaaddddr_racket_exclamation_point = set_caaddddr_racket_exclamation_point
set_mcaadddddr_racket_exclamation_point = set_caadddddr_racket_exclamation_point
set_mcaaddddddr_racket_exclamation_point = set_caaddddddr_racket_exclamation_point
set_mcaadddddddr_racket_exclamation_point = set_caadddddddr_racket_exclamation_point
set_mcaaddddddddr_racket_exclamation_point = set_caaddddddddr_racket_exclamation_point
set_mcaadddddddddr_racket_exclamation_point = set_caadddddddddr_racket_exclamation_point
set_mcaaddddddddddr_racket_exclamation_point = set_caaddddddddddr_racket_exclamation_point
###

#interpreter:
possibility = (result, str) -> list(list(result, str))

impossibility = () -> []

is_possible = (xs) -> 0 < xs.length

is_impossible = (xs) -> xs.length is 0

shallow_flatten = (xss) -> xss.reduce(((ys, xs) -> ys.concat(xs)), [])

given = (possibilities, f) -> array2nested_pairs(shallow_flatten(nested_pairs2array(possibilities).map(([result, str]) -> f(result, str))))

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