set_of_racket_exclamation_point = (obj, k, v) -> obj[k] = v

string_to_number = Number
number_to_string = String
boolean_to_string = String

is_equal = (a, b) -> a is b

is_eq = is_equal

apply = (f, args) -> f.apply({}, args)

display = newline = console.log

raise = (x) -> throw x

error = (x, args...) -> throw {name: x, message: args}

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
    str.slice(start, end)

string_downcase = (str) -> str.toLowerCase()

string_upcase = (str) -> str.toUpperCase()

#list functions
###Less efficient list functions, that do not assume cdr always returns a list.
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
###

#More efficient list functions that assume cdr always returns a list.
cons = (x, xs) -> [x].concat(xs)

empty = []

list_to_mlist = mlist_to_list = deep_list_to_mlist = deep_mlist_to_list = (x) -> x

is_list = is_mlist = (xs) -> xs.constructor is Array

list = mlist = ->
  Array::slice.call arguments, 0

count = (f, xs) -> xs.filter(f).length

length = mlength = (xs) -> xs.length

is_empty = (xs) -> xs.length is 0

append = mappend = () -> Array.prototype.reduce.call(arguments, (rest, next) -> rest.concat(next))

map = mmap = (f, xs) -> xs.map(f)

car = mcar = stream_first = (xs) -> xs[0]
cadr = mcadr = (xs) -> xs[1]
caddr = mcaddr = (xs) -> xs[2]
cadddr = mcadddr = (xs) -> xs[3]
caddddr = mcaddddr = (xs) -> xs[4]
cadddddr = mcadddddr = (xs) -> xs[5]
caddddddr = mcaddddddr = (xs) -> xs[6]
cadddddddr = mcadddddddr = (xs) -> xs[7]
caddddddddr = mcaddddddddr = (xs) -> xs[8]
cadddddddddr = mcadddddddddr = (xs) -> xs[9]
caddddddddddr = mcaddddddddddr = (xs) -> xs[10]
cdr = mcdr = (xs) -> xs.slice 1
cddr = mcddr = (xs) -> xs.slice 2
cdddr = mcdddr = (xs) -> xs.slice 3
cddddr = mcddddr = (xs) -> xs.slice 4
cdddddr = mcdddddr = (xs) -> xs.slice 5
cddddddr = mcddddddr = (xs) -> xs.slice 6
cdddddddr = mcdddddddr = (xs) -> xs.slice 7
cddddddddr = mcddddddddr = (xs) -> xs.slice 8
cdddddddddr = mcdddddddddr = (xs) -> xs.slice 9
cddddddddddr = mcddddddddddr = (xs) -> xs.slice 10
cdddddddddddr = mcdddddddddddr = (xs) -> xs.slice 11
cddddddddddddr = mcddddddddddddr = (xs) -> xs.slice 12
caar = mcaar = (xs) -> xs[0][0]
caadr = mcaadr = (xs) -> xs[1][0]
caaddr = mcaaddr = (xs) -> xs[2][0]
caadddr = mcaadddr = (xs) -> xs[3][0]
caaddddr = mcaaddddr = (xs) -> xs[4][0]
caadddddr = mcaadddddr = (xs) -> xs[5][0]
caaddddddr = mcaaddddddr = (xs) -> xs[6][0]
caadddddddr = mcaadddddddr = (xs) -> xs[7][0]
caaddddddddr = mcaaddddddddr = (xs) -> xs[8][0]
caadddddddddr = mcaadddddddddr = (xs) -> xs[9][0]
caaddddddddddr = mcaaddddddddddr = (xs) -> xs[10][0]
set_car_racket_exclamation_point = set_mcar_racket_exclamation_point = (xs, x) -> xs[0] = x
set_cadr_racket_exclamation_point = set_mcadr_racket_exclamation_point = (xs, x) -> xs[1] = x
set_caddr_racket_exclamation_point = set_mcaddr_racket_exclamation_point = (xs, x) -> xs[2] = x
set_cadddr_racket_exclamation_point = set_mcadddr_racket_exclamation_point = (xs, x) -> xs[3] = x
set_caddddr_racket_exclamation_point = set_mcaddddr_racket_exclamation_point = (xs, x) -> xs[4] = x
set_cadddddr_racket_exclamation_point = set_mcadddddr_racket_exclamation_point = (xs, x) -> xs[5] = x
set_caddddddr_racket_exclamation_point = set_mcaddddddr_racket_exclamation_point = (xs, x) -> xs[6] = x
set_cadddddddr_racket_exclamation_point = set_mcadddddddr_racket_exclamation_point = (xs, x) -> xs[7] = x
set_caddddddddr_racket_exclamation_point = set_mcaddddddddr_racket_exclamation_point = (xs, x) -> xs[8] = x
set_cadddddddddr_racket_exclamation_point = set_mcadddddddddr_racket_exclamation_point = (xs, x) -> xs[9] = x
set_caddddddddddr_racket_exclamation_point = set_mcaddddddddddr_racket_exclamation_point = (xs, x) -> xs[10] = x
set_caar_racket_exclamation_point = set_mcaar_racket_exclamation_point = (xs, x) -> xs[0][0] = x
set_caadr_racket_exclamation_point = set_mcaadr_racket_exclamation_point = (xs, x) -> xs[1][0] = x
set_caaddr_racket_exclamation_point = set_mcaaddr_racket_exclamation_point = (xs, x) -> xs[2][0] = x
set_caadddr_racket_exclamation_point = set_mcaadddr_racket_exclamation_point = (xs, x) -> xs[3][0] = x
set_caaddddr_racket_exclamation_point = set_mcaaddddr_racket_exclamation_point = (xs, x) -> xs[4][0] = x
set_caadddddr_racket_exclamation_point = set_mcaadddddr_racket_exclamation_point = (xs, x) -> xs[5][0] = x
set_caaddddddr_racket_exclamation_point = set_mcaaddddddr_racket_exclamation_point = (xs, x) -> xs[6][0] = x
set_caadddddddr_racket_exclamation_point = set_mcaadddddddr_racket_exclamation_point = (xs, x) -> xs[7][0] = x
set_caaddddddddr_racket_exclamation_point = set_mcaaddddddddr_racket_exclamation_point = (xs, x) -> xs[8][0] = x
set_caadddddddddr_racket_exclamation_point = set_mcaadddddddddr_racket_exclamation_point = (xs, x) -> xs[9][0] = x
set_caaddddddddddr_racket_exclamation_point = set_mcaaddddddddddr_racket_exclamation_point = (xs, x) -> xs[10][0] = x


#interpreter:
possibility = (result, str) -> 
    list(list(result, str))

impossibility = () -> []

is_possible = (xs) -> 0 < xs.length

is_impossible = (xs) -> xs.length is 0

shallow_flatten = (xss) -> xss.reduce(((ys, xs) -> ys.concat(xs)), [])

#this defn of given assumes that cdr always returns a list.
given = (possibilities, f) -> 
    shallow_flatten(possibilities.map(([result, str]) -> f(result, str)))

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