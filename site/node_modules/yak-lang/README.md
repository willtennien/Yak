
Yak
=======
Yak is the first language of the funject paradigm. If you don't know about it, read about <a href='//www.yak-lang.org'>it</a>. The yak-lang module gives you a full Yak interpreter. 

<h2>Installing</h2>

<code><pre>$ npm install yak-lang</pre></code>

<h2>Usage</h2>

<code><pre>var yak = require('yak-lang');</pre></code>

You can evaluate strings of Yak:

<code><pre>yak.eval('print["Hello, Yak!"]');</pre></code>

Or evaluate entire files:

<code><pre>yak.evalFile('./awesomeness.yak', function (err, result) { 
    ... 
});</pre></code>

Both eval and evalFile pass their callbacks any errors and a JavaScript object representing the last Yak expression. 

You can also use their synchronous cousins evalSync and evalFileSync:

<code><pre>yak.evalFileSync('./app.yak');</pre></code>

<h2>Import</h2>

In Yak you can access the <code>require</code>-like funject <code>import</code>:

<code><pre>import['./another.yak']</pre><code>

In <code>another.yak</code> you have access to the <code>require</code>-esque <code>exports</code>:

<code><pre>exports.foo = 'bar'

exports[Math.sin[@x]] := Math.cos[@x]</pre></code>


