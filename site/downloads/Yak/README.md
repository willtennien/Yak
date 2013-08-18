Yak
===

This folder contains an executable interpreter "yak". Note that this is the canonical, experimental, "edge" version of Yak. Because of this, it is quite slow.  If you want to argue with your friends about what constitutes "correct" Yak, however, this is the interpreter for you.

Usage:

You may specify files that the interpreter will interpreter in sequence:

    yak foo.yak bar.yak baz.yak

You may also pipe Yak into the interpreter's stdin:

    echo 'print["Hello, Yak!"]' | yak