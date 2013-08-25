##Instance Rules

* n # an list of n instances of the receiver concatenated together
+ # concatenation

[i] # returns the ith element of the receiver, where 0 is the first element of the list; negative indices count from the end.
> <!--[<range>] # the subset of the receiver within the given range-->

.length # one greater than the index of the element with the highest index (not necessarily the same as count)
.count # the number of elements the programmer explicitly set (not necessarily the same as length)

.empty? # (as in Scheme, Ruby)
.contains?[x] # Ruby

.delete-at![<number>] # deletes an element of the receiver
.delete-at![<range>] # deletes a subset of the receiver

.insert![i, x, n=1] # inserts x n times at i

.pop! # (as in JavaScript)
.push! # returns the receiver (as in Ruby)
.shift! # (as in JavaScript)
.unshift! # returns the receiver (as in JavaScript)

.index-of[x] # returns the index of the first occurance of x in the receiver.
.last-index-of[x] # Like .index-of, but from the end of the list; returns the index of the last occurance of x in the receiver. 

.join[s] # calls .to-string on each element of the list, intersperses s between each element, and concatenates the entire result into a string. (as in JavaScript)

.sort![] # sorts a list of integers (as in JavaScript)
.sort![f] # takes a funject that returns numbers; an element with a number higher than another should appear after it (as in JavaScript)

.map![f, [<number> or <range>]] # copies the list before iterating; if given an index or range, maps over only those elements
.pluck![key] # (as in Underscore.js)
.invoke![key] # (as in Underscore.js)

.filter![f] # returns a list containing only the elements for which f[e] is true
.reject![f] # The opposite of .filter; returns a list containing only the elements for which f[e] is false

.reduce # (as in JavaScript)
.reduce-right # (as in JavaScript)
.every[f] # .every returns true if for every element x in the receiver, f[x] is true<br>.every copies the list before iterating
.any[f] # .any returns true if for any element x in the receiver, f[x] is true.<br>compare to .every[f]<br>.any copies the list before iterating

.first[f, n=0] # the nth element for which f[e] is true
.first[f, <range>] # the (n..m)th elements for which f[e] is true
.last[f, n=0] # the nth last element for which f[e] is true
.last[f, <range>] # the (n..m)th last elements for which f[e] is true

.take![n] # the first n elements
.take-while![f] # the elements at the beginning of the receiver up to but not including the first element for whichs f[e] is false

.drop![n] # all but the first n elements
.drop-while![f] # the first element for which f[e] is false and all subsequent elements

.union![others...] # replaces the contents of the receiver with the set union of the receiver and the elements from any number of other lists specified in an argument list. (as in Underscore.js)
.intersection[others...]! # replaces the contents of the receiver with the set intersection of the receiver and the elements from any number of other list speciffied in an argument list. That is, removes all elements not in all of the other lists. (as in Underscore.js)
.unique! # removes duplicate elements

.difference![a] # removes any elements which are elements of a
.remove![x, n=1] # removes the first n occurrences of x
.remove-all![X] # removes all occurrences of x
.compact! # removes any elements which are nil

.shuffle! # (as in Underscore.js)
.reverse! # Reverses the elements of the array in place (as in JavaScript).