interpreted = []

interpret = (str) ->
    interpreted.push(str)
    # if interpreted.length is 2
    #     for c, i in interpreted[0]
    #         if interpreted[1][i] is c
    #             console.log('-')
    #         else
    #             console.log("'#{c}' isnt '#{interpreted[1][i]}'")
    #     console.log(interpreted[0] is interpreted[1])
    console.log($('body')[0])
    console.debug(str)
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
                console.debug "local src interpret:"
                interpret data

            else
                $.get $(elem).attr("src"), (data) ->
                    console.debug "local src interpret:"
                    interpret data

        else
            console.debug "inline interpret:"
            interpret $(elem).text().replace(/^[ \t]*\n/, '').replace(/[\n\ \t]*$/, '')
