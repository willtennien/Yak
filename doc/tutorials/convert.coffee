_ = require('customscore')
{exec} = require('child_process')

escapeWhite = (str) -> str.replace(/(\n\r|\n|\r)/g, '<br>').replace(/\ /g, '&nbsp;')

escapeWhiteTrim = (str) -> escapeWhite(str.replace(/^\ +|\ +$/gm, ''))

_.streamToString(process.stdin, (str) ->
    result  = (if process.argv[2].match(/t/)? then escapeWhiteTrim else escapeWhite)(if process.argv[2].match(/w/)? then str else _.escape(str))
    exec("echo '#{result.replace("'", "\\'")}' | pbcopy"))