##Instance Rules

* n # a string of n instances of the receiver
+ s # concatenation

[<number>] # a single-length string representing the character at the given zero-based index; negative indices count from the end
[<range>] # the substring of the receiver in the given range

.length # the number of characters in the string

.contains?[s] # true if the receiver contains the substring s
.begins-with?[s] # returns true of the receiver begins with the string s
.ends-with?[s] # returns true if the receiver ends with s

.index-of[s] # returns the index of the first occurance of s in the receiver.<br>returns <code>nil</code> if s does not appear in the receiver.
.last-index-of[s] # returns the index of the last occurance of s in the receiver.<br>returns <code>nil</code> if s does not appear in the receiver.

.split[s] # splits the receiver at occurances of <code>s</code> (as in JavaScript).
.substring[start, end] # returns all characters beginning at index start and ending just before end.

.replace[search-for, replace-with] # replaces all occurrences of the string search-for with the string replace-with in the receiver
.replace-first[search-for, replace-with] # replaces the first occurrence of the string search-for with the string replace-with in the receiver

.repeat[n] # Returns a string containing the reciever repeated n times

.uppercase
.lowercase
.swapcase # replaces every letter with the letter of the opposite case.
.capitalize
.titlecase