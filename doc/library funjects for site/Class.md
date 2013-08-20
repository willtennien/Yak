##Instance Rules

.superclass # a class's superclass
.subclasses # the receiver's direct subclasses
.all-subclasses # all of the receiver's subclasses
.subclass?[c] # true if c is a subclass of the receiver or any of its subclasses
.superclass?[c] # true if c is the superclass of the receiver or any of its superclasses
.methods # the keys for the receivers's instance methods
.all-methods # the keys for the receiver's instance methods and those of all of its superclasses
.instance # the receiver's instance methods