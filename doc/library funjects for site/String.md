##Instance Rules

* n # a string of n instances of the receiver
+ s # concatenation

[&lt;number&gt;] # a single-length string representing the character at the given zero-based index; negative indices count from the end
[&lt;range&gt;] # the substring of the receiver in the given range

.length # the number of characters in the string

.contains?[s] # true if the receiver contains the substring s
.begins-with?[s] # returns true of the receiver begins with the string s
.ends-with?[s] # returns true if the receiver ends with s

.index-of[s] # returns the index of the first occurance of s in the receiver.<br>returns <code>nil</code> if s does not appear in the receiver.
.last-index-of[s] # returns the index of the last occurance of s in the receiver.<br>returns <code>nil</code> if s does not appear in the receiver.

.split[s] # splits the receiver at occurances of <code>s</code> (as in JavaScript).
.substring[start, end] # returns all characters beginning at the <code>start</code> index and ending just before the <code>end</code> index.<br>That is, <code>.substring</code> returns a string with a length of <code>end - start</code>. In later versions of Yak, we will probably replace <code>.substring</code> with range reference, either in the form of <code>"foo"[1..2]</code> or <code>"foo"[1:2]</code>.

.replace[search-for, replace-with] # replaces all occurrences of the string search-for with the string replace-with in the receiver
.replace-first[search-for, replace-with] # replaces the first occurrence of the string search-for with the string replace-with in the receiver

.repeat[n] # returns a string containing the reciever repeated n times

.uppercase # returns a copy of the receiver containing every letter in uppercase. Leaves non-alpha characters unaltered.
.lowercase # returns a copy of the receiver containing every letter in lowercase. Leaves non-alpha characters unaltered.
.swapcase # replaces every letter with the letter of the opposite case. Leaves non-alpha characters unaltered.
.capitalize # returns a copy of the receiver but with every word capitalized. Leaves non-alpha characters unaltered.
.titlecase # returns a copy of the receiver with every major word (not of, the, etc.) capitalized. Leaves non-alpha characters unaltered.