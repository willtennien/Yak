interpreted = []

debug = -> #(str) -> console.debug(str)
interpret = (str) ->
    interpreted.push(str)
    debug($('body')[0])
    debug(str)
    Yak.interpreter.evalSync(str)

MutationObserver = window.MutationObserver or window.WebKitMutationObserver

onInserted = (callback) ->
    (new MutationObserver((muts) ->
        callback muts.filter((mut) ->
            true
        ).map((mut) ->
            Array::slice.call mut.addedNodes, 0
        ).reduce((a, b) ->
            a.concat b
        )
    )).observe document,
        childList: true
        subtree: true           

onInserted (elems) ->
    elems.filter((elem) ->
        $(elem).is "script[type=\"text/yak\"]"
    ).map (elem) ->
        if $(elem).is("[src]")
            if $(elem).attr("src").match(/^http:\/\//)
                $.get "http://www.corsproxy.com/" + $(elem).attr("src").match(/^http:\/\/(.+)/)[1], (data) ->
                debug "local src interpret:"
                interpret data

            else
                $.get $(elem).attr("src"), (data) ->
                    debug "local src interpret:"
                    interpret data

        else
            debug "inline interpret:"
            interpret $(elem).text().replace(/^[ \t]*\n/, '').replace(/[\n\ \t]*$/, '')
