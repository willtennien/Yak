##Instance Rules

* n # a string of n instances of the receiver
+ s # concatenation

[<number>] # a single-length string representing the character at the given zero-based index; negative indices count from the end
[<range>] # the substring of the receiver in the given range

.length # the length of the string

.contains?[s] # true if the receiver contains the substring s
.begins-with?[s]
.ends-with?[s]

.index-of[s]
.last-index-of[s]

.split[s] # splits the receiver at occurances of <code>s</code> (as in JavaScript).
.substring[start, end]

.replace[search-for, replace-with] # replaces all occurrences of the string search-for with the string replace-with in the receiver
.replace-first[search-for, replace-with] # replaces the first occurrence of the string search-for with the string replace-with in the receiver

.repeat[n] # Returns a string containing the reciever repeated n times

.uppercase
.lowercase
.swapcase # replaces every letter with the letter of the opposite case.
.capitalize
.titlecase