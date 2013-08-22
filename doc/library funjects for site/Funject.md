##Instance Rules

is x # identity comparison; when one changes, the other changes
== x # tests for deep equality between lists and identity between funjects and primitive types; subclasses may override

.apply[x] # => receiver x
.then[f] # => f[receiver]; ex: arr.lots-of-long-methods.then[{[arr]: arr[arr.length - 1]}]
.on f # => f receiver; allows conditionals and switch-cases without additional syntax

.while-true f # repeatedly invokes f[] while reciever[] evaluates to true
.do-while-true f # invokes f[], then repeatedly invokes f[] while reciever[] evaluates to true

.has?[k] # true if the receiver matches k
.copy or clone # creates a copy of the funject

.prepend![{k: v}] # sets k equal to v at the bottom of the funject
.insert![i, {k: v}] # sets k equal to v before the ith pair

.class # returns the class of which the receiver is an instance.

.number? # returns true if the receiver is a <a href='/doc/lib/Number.html'>number</a>
.string? # returns true if the receiver is a <a href='/doc/lib/String.html'>string</a>.
.boolean? # returns true if the receiver is a <a href='/doc/lib/Boolean.html'>boolean</a>.
.symbol? # returns true if the receiver is a <a href='/doc/lib/Symbol.html'>symbol</a>.
.unknown? # returns true if the receiver is <a href='/doc/lib/Unknown.html'>unknown</a>.
.nil? # returns true if the receiver is <a href='/doc/lib/Nil.html'>nil</a>.
.list? # returns true if the receiver is a <a href='/doc/lib/List.html'>list</a>.

.integer? # true if the receiver is a number and has no fractional component
.float? # true if the receiver is a number and has a fractional component

.member-of?[c] # true if receiver is an instance of class c
.kind-of?[c] # true if the receiver is an instance of class c or one of its subclasses

.to-string # a human-readable string representation
.inspect # a debugger-friendly string representation

.silently # a funject which matches the same arguments as the receiver and returns nil where an error would normally occur