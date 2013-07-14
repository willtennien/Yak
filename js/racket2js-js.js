//        SCHEME FUNJECTS HAND-WRITTEN IN JAVASCRIPT

var Symbol = (function () {
    var prototypeOfSymbol = {};
    
    prototypeOfSymbol.toString = function () {
        return "'|" + String(this._val) + "|";

    };

    prototypeOfSymbol.isSymbol = function () {
        return true;

    }

    var internalSymbol = function (str) {
        result = Object.create(prototypeOfSymbol);
        result._val = str;
        return result;

    };

    var symbols = {};

    return function (str) {
        return symbols[str] || (symbols[str] = internalSymbol(str));

    };

}());

string_to_symbol = Symbol
symbol_to_string = function (sym) { 
    return sym._val; 
    
};



var not = function (x) {
    return !x;

}
