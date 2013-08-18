#!/usr/local/bin/coffee
{exec} = require('child_process')
fs = require('fs')

exec("coffee -o . -c ../interpreter/interpreter.coffee ../interpreter/parser.coffee"
     (->
        fs.writeFileSync('./interpreter.js'
                         (fs.readFileSync './interpreter.js').toString().replace('parser.coffee', 'parser.js'))))
    
