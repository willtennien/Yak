require('./interpreter.js')(require('fs').readFileSync('./server.yak').toString())
//require('coffee-script');
//require('./interpreter.coffee')(require('fs'.readFileSync('./server.yak')));