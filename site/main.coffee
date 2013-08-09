header = document.querySelector("header")
space = document.querySelector(".header-fill")

h1s = document.querySelectorAll("h1")

nav = header.firstElementChild
links = document.querySelectorAll("nav a")
tryYak = document.querySelector(".try-yak")

interpreter = document.querySelector(".interpreter")
prompt = document.querySelector ".prompt"
textarea = document.querySelector ".prompt textarea"
measure = document.querySelector ".prompt .measure"

interpreter.style.display = "block"
header.style.position = "fixed"
header.style.left = "24px"
header.style.right = "24px"

fixedInterpret = false
activeLink = undefined

ANIMATE = {}
scroll = (animate) ->
    min = nav.lastElementChild.offsetHeight - nav.offsetHeight
    interpreterHeight = interpreter.offsetHeight
    minInterpreter = min + interpreterHeight
    min = minInterpreter  if fixedInterpret
    top = Math.min(0, Math.max(min, -window.scrollY))
    if animate is ANIMATE
        header.style.transition = "top .3s"
        setTimeout (->
            header.style.transition = "none"
        ), 300
    header.style.top = top + "px"
    activeLink.className = ""  if activeLink
    tryYak.style.display = (if top <= minInterpreter then "block" else "none")
    i = 0
    h1 = h1s[0]
    while i < h1s.length
        bb = h1s[i].getBoundingClientRect()
        break  if bb.top > 96 + (if fixedInterpret then interpreterHeight else 0)
        h1 = h1s[i]
        ++i
    id = h1.nextElementSibling.id
    i = 0
    while i < links.length
        if links[i].href.split("#").pop() is id
            activeLink = links[i]
            break
        ++i
    activeLink.className = "active"

window.addEventListener "scroll", scroll
scroll()

resize = ->
    space.style.height = header.offsetHeight + "px"
    scroll()

window.addEventListener "resize", resize
resize()

tryYak.addEventListener "click", ->
    unless fixedInterpret
        fixedInterpret = true
        setTimeout (->
            focusInterpreter()
        ), 300
        scroll ANIMATE
        tryYak.textContent = "Close"
    else
        fixedInterpret = false
        scroll ANIMATE
        tryYak.textContent = "Try Yak"

focusInterpreter = ->
    textarea.focus()
    textarea.selectionStart = textarea.selectionEnd = textarea.value.length

focusInterpreter() if textarea.getBoundingClientRect().top >= 0

autoSize = ->
    measure.textContent = textarea.value + "X"
    textarea.style.height = measure.offsetHeight + "px"

textarea.addEventListener "input", autoSize
autoSize()

log = (level, s) ->
    div = document.createElement("div")
    div.className = level
    div.textContent = s
    interpreter.insertBefore div, prompt
    interpreter.scrollTop = interpreter.scrollHeight

keyboard = (e) ->
    insert = (s) ->
        b = before + s
        textarea.value = b + after
        textarea.selectionStart = textarea.selectionEnd = b.length

    before = textarea.value.slice(0, textarea.selectionStart)
    after = textarea.value.slice(textarea.selectionStart)
    line = before.split("\n").pop()
    indent = /^\s*/.exec(line)[0]

    switch e.keyCode
        when 13
            e.preventDefault()
            shouldIndent = /class|module|if|else|try|catch|finally|\[[^\]]*$|\([^\)]*$|\{[^\}]*$/.test line
            if e.shiftKey or textarea.selectionStart isnt textarea.selectionEnd or textarea.selectionStart isnt textarea.value.length or indent.length isnt 0 or shouldIndent
                indent += "    "  if shouldIndent
                insert "\n" + indent
            else
                log "prompt", textarea.value
                if textarea.value
                    Yak.interpreter.eval textarea.value, "input", 1, (error, result) ->
                        if error
                            log "error", error.trace ? error.message
                        else if result
                            log "output", result.toSource()

                    textarea.value = ""
            autoSize()

        when 9
            e.preventDefault()
            if e.shiftKey
                n = Math.min line.length - Math.ceil(line.length / 4 - 1) * 4, /\s*$/.exec(before)[0].length
                if n
                    before = before.slice 0, -n
                    insert ""
            else
                insert Array(Math.floor(line.length / 4 + 1) * 4 - line.length + 1).join(" ")

textarea.addEventListener "keydown", keyboard

Yak.interpreter.print = (string) ->
    log "print", string
