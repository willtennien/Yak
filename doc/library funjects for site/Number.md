##Class Rules

Number.e # <a href=http://en.wikipedia.org/wiki/E_(mathematical_constant)>euler's number.</a>
Number.pi # <a href=http://en.wikipedia.org/wiki/Pi>&#x03C0;</a>&#x2014;the circumference of a circle divided by its radius.

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

.sin # the <a href=http://en.wikipedia.org/wiki/Sine>sine of the receiver in radians</a>.
.cos # the <a href=http://en.wikipedia.org/wiki/Trigonometric_functions#Sine.2C_cosine.2C_and_tangent>cosine</a> of the receiver in radians.
.tan # the <a href=http://en.wikipedia.org/wiki/Trigonometric_functions#Sine.2C_cosine_and_tangent>tangent</a> of the receiver in radians.
.sec # the <a href=http://en.wikipedia.org/wiki/Trigonometric_functions#Reciprocal_functions>secant</a> of the receiver in radians.
.csc # the <a href=http://en.wikipedia.org/wiki/Trigonometric_functions#Reciprocal_functions>cosecant</a> of the receiver in radians.
.cot # the cotangent of the receiver in radians.
.asin # the arcsin of the receiver; returns an angle in radians.
.acos # the arccos of the receiver; returns an angle in radians.
.atan # the arctangent of the receiver; returns an angle in radians.
.atan/[n] # a.tan/[b] is (a / b).atan; this allows us to include that <code>a.atan/[0]</code> is <code>1</code> without using a fake number to represent 'infinity'.

.abs # also a prefix operator

.ceil
.floor
.round

.times[f] # invokes f[0]..f[receiver - 1] and returns a list of the results; signals an error if the receiver is not an integer