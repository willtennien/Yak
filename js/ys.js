var Yak = (function () {
    /*! jQuery v1.10.2 | (c) 2005, 2013 jQuery Foundation, Inc. | jquery.org/license
    */
    var MutationObserver = window.MutationObserver || window.WebKitMutationObserver;

    /*! Yak v0.1 | yak-lang.org */
    var interpret = function (str) {
    	console.log(str);
    };

    var onInserted = function (callback) {
    	(new MutationObserver(function (muts) {
    		callback(muts.filter(function (mut) {
    			return true;
    		}).map(function (mut) {
    			return Array.prototype.slice.call(mut.addedNodes, 0);
    		}).reduce(function (a, b) { 
    			return a.concat(b); 
    		}));
    	})).observe(document, {
    		childList: true,
    		subtree: true
    	});
    };

    onInserted(function (elems) {
    	elems.filter(function (elem) {
    		return $(elem).is('script[type="text/yak"]');
    	}).map(function (elem) {
    		if ($(elem).is('[src]')) {
    			if ($(elem).attr('src').match(/^http:\/\//)) {
    				$.get('http://www.corsproxy.com/' + $(elem).attr('src').match(/^http:\/\/(.+)/)[1], function (data) {
    					console.debug('local src interpret:');
    					return interpret(data);
    				});
    			} else {
    				$.get($(elem).attr('src'), function (data) {
    					console.debug('local src interpret:');
    					return interpret(data);
    				});
    			}
    		} else {
    			console.debug('inline interpret:');
    			interpret($(elem).text());
    		}
    	});
    });

    return {
        '$': $,
        'eval': interpret
    }
}());