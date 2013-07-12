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




var not = function (x) {
    return !x;

}
