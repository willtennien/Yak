##Class Rules

Number.e # <a href=http://en.wikipedia.org/wiki/E_(mathematical_constant)>euler's number.</a>
Number.pi # <a href=http://en.wikipedia.org/wiki/Pi>π</a>—the circumference of a circle divided by its radius. 

Number.random[] # a random number on the range [0, 1)
Number.random[n] # a random integer between 0 and n - 1, including 0 and n - 1.
Number.random[a, b] # a random integer between that a and b - 1; signals an error if b <= a

##Instance Rules

+ # adds the receiver and its argument.
- # subtracts its argument from the receiver.
* # multiplies the receiver and its argument.
/ # division operator
% # modulo operator
^ # power operator

< # less than operator

&gt; # greater than operator

<= # less than or equal to operator

>= # greater than or equal to operator

.sqrt # returns the square root of the receiver.
.ln # returns the natural logarithm (base e) of the receiver.
.log[b] # the log base b of the receiver

.sin # the sin of the receiver in radians.
.cos # the cosine of the receiver in radians.
.tan # the tangent of the receiver in radians.
.sec # the secant of the receiver in radians.
.csc # the cosecant of the receiver in radians.
.cot
.asin
.acos
.atan
.atan/[n] # a.tan/[b] is (a / b).atan; this allows us to note that a.atan/[0] is 1 without using Infinity

.abs # also a prefix operator

.ceil
.floor
.round

.times[f] # invokes f[0]..f[receiver - 1] and returns a list of the results; signals an error if the receiver is not an integer