require('coffee-script');
require('./interpreter.coffee')(require('fs'.readFileSync('./server.yak')));