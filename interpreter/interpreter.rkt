#lang racket
(define-syntax try
  (syntax-rules (catch)
    ((_ body (catch catcher))
     (call-with-current-continuation
      (lambda (exit)
        (with-exception-handler
         (lambda (condition)
           catcher
           (exit condition))
         (lambda () body)))))))

(require compatibility/mlist)

;;;                Personal Library
(define (is a)
  (lambda (b)
    (equal? a b)))

(define (is-gt a)
  (lambda (b)
    (< a b)))

(define (is-gte a)
  (lambda (b)
    (<= a b)))


;;;;begin not translating
(define (matches a b)
  (or (and (equal? a "")
           (equal? b ""))
      (ormap (lambda (result)
               (not (equal? result b)))
             (string-split b a))))
;;;;end not translating





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                Syntax Parser

(define odsl 'doesnt-matter)
(define parse ((lambda ()
                 (define (string-capitalize-word str)
                   (string-append (string-titlecase (substring str 0 1))
                                  (substring str 1)))
                 
                 (define (is-in-string str)
                   (lambda (matched)
                     (matches matched str)))
                 
                 (define (is-in-list xs)
                   (lambda (c)
                     (< 0 (count (lambda (e) (equal? e c)) xs))))
                                  
                 (define (partial-flatten xs)
                   (if (empty? xs)
                       xs
                       (append (car xs)
                               (partial-flatten (cdr xs)))))
                 
                 (define (substring-no-max str . indexes)
                   (cond 
                     [(< (string-length str) 
                         (car indexes))
                      ""]
                     [(and (not (empty? (cdr indexes)))
                           (< (string-length str)
                              (cadr indexes)))
                      (substring-no-max str (car indexes))]
                     [else
                      (apply substring (cons str indexes))]))
                 
                 (define (assert check message)
                   (if check
                       'ok
                       (raise message)))
                 
                 ;;;;begin not translating
                 (define (p . args)
                   (apply display args)
                   (newline))
                 
                 (define (deep-stream->list s)
                   (if (not (stream? s))
                       s
                       (map deep-stream->list (stream->list s))))
                 
                 (define dsl deep-stream->list)
                 ;;;;end not translating
                 
                 (define (parens->spaces str)
                   (string-replace (string-replace str
                                                   ")"
                                                   " ")
                                   "(" 
                                   " "
                                   ))
                 
                 ;;;                Parsing
                 
                 ;;Test
                 #|
(dsl (parse-number "3.0e8" no-indent))
(dsl (parse-boolean "true" no-indent))
(dsl (parse-string "\"f\"" no-indent))
(dsl (parse-nil "nil foo" no-indent))
(dsl (parse-dot "dot " no-indent))
;Check multiline strings later.
(dsl (parse-identifier "fo-o" no-indent))
(dsl (parse-list-literal "[]" no-indent))
(dsl (parse-list-literal "[1, 2, [9]]" no-indent))
(dsl (parse-funject-literal "{}" no-indent))
(dsl (parse-funject-literal "{ x: 3 }" no-indent))
(dsl (parse-exp "{ \nx: 3\ny: 5 \n}" no-indent))
(dsl (parse-exp "x :=   \n    y = 3\n    y |=   { foo: 'bar' }   \n    y" base-indent))
(dsl (parse-exp "foo-bar-baz |:=
    y = 3
    
    y |= {
        foo: 'bar'
        baz: 'qux'
    }
    y
" base-indent))
|#
                 
                 ;;;    Possibility Procedures
                 
                 ;;;;begin not translating
                 (define-syntax possibility
                   (syntax-rules ()
                     [(possibility) (stream '())]
                     [(possibility a ...) (stream (list a ...))]))
                 
                 (define (impossibility) (stream))
                 (define (possible? s) (not (stream-empty? s)))
                 (define (impossible? s) (stream-empty? s))
                 
                 (define (given possibilities proc)
                   ;(p (stream->list possibilities))
                   (stream-shallow-flatten (stream-map (lambda (xs) 
                                                         (proc (car xs)
                                                               (cadr xs)))
                                                       possibilities)))
                 
                 (define (stream-shallow-flatten s)
                   (cond 
                     [(stream-empty? s)
                      (stream)]
                     [(stream-empty? (stream-first s))
                      (stream-shallow-flatten (stream-rest s))]
                     [else
                      (stream-cons (stream-first (stream-first s))
                                   (stream-shallow-flatten (stream-cons (stream-rest (stream-first s))
                                                                        (stream-rest s))))]))
                 
                 (define-syntax also
                   (syntax-rules ()
                     [(also) (stream)]
                     [(also a ...) (stream-shallow-flatten (stream a ...))]))
                 
                 (define (given-seq possibilities . procs)
                   (define (iter possibilities procs)
                     (if (empty? procs)
                         possibilities
                         (given possibilities
                                (lambda (_ str)
                                  (iter ((car procs) str)
                                        (cdr procs))))))
                   (iter possibilities procs))
                 ;;;;end not translating
                 
                 
                 (define (no-indent x) (= x 0))
                 (define (base-indent x) (<= 0 x))
                 (define (any-indent _) true) 
                 (define (tokenize . args)
                   (apply list (cons (string->symbol (string-append (string-append "Token-")
                                                                    (string-downcase (symbol->string (car args)))))
                                     (cdr args))))
                 (define (token-contents token f)
                   (apply f (cdr token)))
                 
                 
                 
                 ;;;    Basic Syntax
                 
                 ;;;least-acceptable-indent
                 
                 ;;;;begin not translating
                 (define (least-acceptable-indent indent max)
                   (define candidates (filter indent (range 0 (+ 1 max))))
                   (if (empty? candidates)
                       #f
                       (make-string (car candidates) #\ )))
                 ;;;;end not translating
                 
                 ;;;parse-white (force all; do not entertain the possibility of fewer than all possible spaces)
                 
                 (define (parse-white str _)
                   (parse-all-like (is " ") str no-indent))
                 
                 ;;;parse-white-line
                 ;;;Use this if masochistic users want to retain indentation on blank lines:
                 #|(given parse-n-like (least-acceptable-indent indent (string-length str))
                      (is " ")
                      str
                      no-indent)|#
                 
                 (define (parse-white-line str indent)
                   (given (parse-all-like (is " ") str no-indent)
                          (lambda (spaces str)
                            (given (parse-characters "\n" str no-indent)
                                   (lambda (newline str)
                                     (possibility (string-append spaces newline) str))))))
                 
                 ;;;parse-characters
                 
                 (define (parse-characters to-parse str _)
                   (if (equal? to-parse (substring-no-max str 0 (string-length to-parse)))
                       (possibility to-parse (substring str (string-length to-parse)))
                       (impossibility)))
                 
                 ;;;parse-n-like
                 
                 (define (parse-n-like n check str _)
                   (cond [(= n 0)
                          (possibility "" str)]
                         [(or (equal? str "") 
                              (not (check (substring str 0 1))))
                          (impossibility)]
                         [else
                          (given (parse-n-like (- n 1) check (substring str 1) no-indent)
                                 (lambda (rest unparsed-str)
                                   (possibility (string-append (substring str 0 1) 
                                                               rest)
                                                unparsed-str)))]))
                 
                 ;;;parse-all-like (force all; do not entertain the possibility of fewer than all possible matches)
                 
                 (define (parse-all-like check str _)
                   (if (or (equal? str "")
                           (not (check (substring str 0 1))))
                       (possibility "" str)
                       (given (parse-all-like check (substring-no-max str 1) no-indent)
                              (lambda (rest unparsed-str)
                                (possibility (string-append (substring str 0 1)
                                                            rest)
                                             unparsed-str)))))
                 
                 ;;;parse-all-at-least-one-like
                 
                 (define (parse-all-at-least-one-like check str _)
                   (given (parse-all-like check str no-indent)
                          (lambda (maybe-result str)
                            (if (< 0 (string-length maybe-result))
                                (possibility maybe-result str)
                                (impossibility)))))
                 
                 ;;;parse-separated
                 
                 (define (parse-separated separator separated str indent)
                   (define (parse-separated result str)
                     (if (possible? (separated str indent))
                         (given (separated str indent)
                                (lambda (e str)
                                  (parse-separator (append result (list e)) str)))
                         (possibility result str)))
                   (define (parse-separator result str)
                     (if (possible? (separator str indent))
                         (given (separator str indent)
                                (lambda (_ str)
                                  (parse-separated result str)))
                         (possibility result str)))
                   (parse-separated '() str))
                 
                 ;;;parse-each proc
                 
                 (define (parse-each proc str indent)
                   (define possibilities (proc str indent))
                   (if (not (possible? possibilities))
                       (possibility '() str)
                       (given possibilities
                              (lambda (exp str)
                                (given (parse-each proc str indent)
                                       (lambda (exps str)
                                         (possibility (cons exp exps) str)))))))
                 
                 
                 
                 
                 ;;;    User Syntax
                 
                 
                 
                 ;;;parse-exps
                 
                 (define (parse-exps str indent)
                   (given (parse-all-like (is-in-string " \n") 
                                          str 
                                          no-indent)
                          (lambda (_ str)
                            (given (parse-separated (lambda (str indent)
                                                      (given-seq (parse-white str no-indent)
                                                                 (lambda (str)
                                                                   (parse-characters "\n" str no-indent))
                                                                 (lambda (str)
                                                                   (parse-all-like (lambda (str)
                                                                                     (possible? (parse-white-line str indent)))
                                                                                   str 
                                                                                   no-indent))))
                                                    parse-exp
                                                    str 
                                                    indent)
                                   (lambda (exps str)
                                     (given (parse-all-like (is-in-string " \n") str no-indent)
                                            (lambda (_ str)
                                              (if (equal? str "")
                                                  (possibility exps "")
                                                  (impossibility)))))))))
                 
                 ;;;parse-exp (provide for parentheses)
                 
                 (define (parse-exp str indent)
                   (given (parse-n-like 1 (lambda (c) true) str no-indent)
                          (lambda (_ __)
                            (also (given-seq (parse-characters "(" str no-indent)
                                             (lambda (str) 
                                               (given (parse-exp str indent)
                                                      (lambda (exp str)
                                                        (given-seq (parse-characters ")" str no-indent)
                                                                   (lambda (str)
                                                                     (possibility exp str)))))))
                                  
                                  (parse-not-beginning-with-exp str indent)
                                  (parse-beginning-with-exp str indent)))))
                 
                 
                 ;;;parse-exp-not-naked-compound
                 
                 (define (parse-exp-not-naked-compound str indent)
                   (also (given-seq (parse-characters "(" str no-indent)
                                    (lambda (str) 
                                      (given (parse-exp str indent)
                                             (lambda (exp str)
                                               (given-seq (parse-characters ")" str no-indent)
                                                          (lambda (str)
                                                            (possibility exp str)))))))
                         (parse-not-beginning-with-exp str indent)))
                 
                 
                 ;;;also-within-naked-compound
                 
                 (define (also-within-naked-compound possi str indent)
                     (also (parse-associating-left-beginning-with-exp-with (stream-first (stream-first possi)) str indent)
                           possi))
                 
                 ;;;;begin not translating
                 (define pdsl (compose dsl parse-exp))
                 ;;;;end not translating
                 
                 
                 ;;;parse-not-beginning-with-exp
                 
                 (define (parse-not-beginning-with-exp str indent)
                   (also (parse-number str indent)
                         (parse-string str indent)
                         (parse-boolean str indent)
                         (parse-nil str indent)
                         (parse-dot str indent)
                         (parse-unknown str indent)
                         (parse-identifier str indent)
                         (parse-parameter str indent)
                         (parse-list-literal str indent)
                         (parse-funject-literal str indent)
                         (parse-strict-assignment str indent)
                         (parse-lazy-assignment str indent)
                         (parse-reset-strict-assignment str indent)
                         (parse-reset-lazy-assignment str indent)))
                 
                 
                 ;;;parse-beginning-with-exp
                 
                 (define (parse-beginning-with-exp str indent) 
                   (also (parse-funject-strict-assignment str indent)
                         (parse-funject-lazy-assignment str indent)
                         (parse-invocation str indent)
                         (parse-funject-inheritance str indent)
                         (parse-inverse-definition str indent)))
                 
                 
                 ;;;parse-associating-left-beginning-with-exp-with
                 (define (parse-associating-left-beginning-with-exp-with first str indent) 
                   (also (parse-invocation-with first str indent))) ;for now, the only expression that associates left and begins with another expression is an invocation.
                 
                 
                 ;;;parse-number
                 
                 (define (parse-number str indent)
                   (define (is-digit c)
                     (or (equal? c "0")
                         (equal? c "1")
                         (equal? c "2")
                         (equal? c "3")
                         (equal? c "4")
                         (equal? c "5")
                         (equal? c "6")
                         (equal? c "7")
                         (equal? c "8")
                         (equal? c "9")))
                   
                   (define (possibility-if-number nstr str)
                     (if (string->number nstr)
                         (possibility (tokenize 'Number (string->number nstr)) str)
                         empty))
                   
                   (define (with-e-notation nstr str)
                     (define (digits-after-e nstr str)
                       (given (parse-all-at-least-one-like is-digit str no-indent)
                              (lambda (digits str)
                                (possibility-if-number (string-append nstr digits)
                                                       str))))
                     (given (parse-n-like 1 
                                          (lambda (c) 
                                            (or (equal? c "e") 
                                                (equal? c "E")))
                                          str
                                          no-indent)
                            (lambda (e str)
                              (also (digits-after-e (string-append nstr e) str)
                                    (given (parse-n-like 1 
                                                         (lambda (c)
                                                           (or (equal? c "+")
                                                               (equal? c "-")))
                                                         str
                                                         no-indent)
                                           (lambda (e-sign str) 
                                             (digits-after-e (string-append nstr e e-sign)
                                                             str)))))))
                   
                   (define (after-initial-sign sign str)
                     (given (parse-all-at-least-one-like is-digit str no-indent)
                            (lambda (first-digits str)
                              (also (possibility-if-number (string-append sign first-digits) str)
                                    (with-e-notation (string-append sign first-digits) str)
                                    (given (parse-characters "." str no-indent)
                                           (lambda (spot str)
                                             (given (parse-all-at-least-one-like is-digit str no-indent)
                                                    (lambda (rest-digits str)
                                                      (also (possibility-if-number (string-append sign
                                                                                                  first-digits
                                                                                                  spot
                                                                                                  rest-digits)
                                                                                   str)
                                                            (with-e-notation (string-append sign 
                                                                                            first-digits 
                                                                                            spot 
                                                                                            rest-digits)
                                                                             str))))))))))
                   
                   (also (after-initial-sign "" str)
                         (given (parse-characters "-" str no-indent) 
                                after-initial-sign)))
                 
                 
                 
                 ;;;parse-string
                 
                 (define (parse-string str indent)
                   (define (unescape str)
                     (string-replace "\\n" "\n" 
                                     (string-replace "\\\"" "\""
                                                     (string-replace "\\'" "'"
                                                                     str))))
                   (define (parse-string-contents delim str)
                     (define (iter str)
                       (also (given (parse-characters delim str no-indent)
                                    (lambda (_ __)
                                      (possibility "" str)))
                             (given (parse-all-at-least-one-like (lambda (c) (not (or (equal? c delim)
                                                                                      (equal? c "\\")
                                                                                      (equal? c "\n"))))
                                                                 str
                                                                 no-indent)
                                    (lambda (first str)
                                      (given (iter str)
                                             (lambda (second str)
                                               (possibility (string-append first second)
                                                            str)))))
                             (given (parse-characters "\\" str no-indent)
                                    (lambda (_ str)
                                      (given (parse-n-like 1 
                                                           (lambda (c)
                                                             (or (equal? c "n")
                                                                 (equal? c "\\")
                                                                 (equal? c "\"")
                                                                 (equal? c "'")))
                                                           str
                                                           no-indent)
                                             (lambda (c str)
                                               (given (iter str)
                                                      (lambda (contents str)
                                                        (possibility (string-append "\\" c contents) str)))))))
                             (given (parse-characters "\n" str no-indent)
                                    (lambda (_ str)
                                      (given (parse-all-like (is-in-string " ") str no-indent)
                                             (lambda (spaces str)
                                               (define indentation (least-acceptable-indent indent (string-length spaces)))
                                               (if indentation
                                                   (iter (substring str (string-length indentation)))
                                                   (impossibility))))))))
                     (iter str))
                   
                   
                   (define (with-delimiter delim)
                     (given (parse-characters delim str no-indent)
                            (lambda (_ str)
                              (given (parse-string-contents delim str)
                                     (lambda (contents str)
                                       (given (parse-characters delim str no-indent)
                                              (lambda (_ str)
                                                (possibility (tokenize 'String contents) str))))))))
                   
                   (also (with-delimiter "\"")
                         (with-delimiter "'")))
                 
                 ;;;parse-boolean
                 
                 (define (parse-boolean str indent)
                   (given (also (parse-characters "true" str no-indent)
                                (parse-characters "false" str no-indent))
                          (lambda (bool str)
                            (cond [(equal? bool "true") (possibility (tokenize 'Boolean true) str)]
                                  [(equal? bool "false") (possibility (tokenize 'Boolean false) str)]
                                  [else (raise "parse-boolean: Syntax Error")]))))
                 
                 ;;;parse-nil
                 
                 (define (parse-nil str indent)
                   (given (parse-characters "nil" str no-indent)
                          (lambda (_ str)
                            (possibility (tokenize 'Nil) str))))
                 
                 ;;;parse-dot
                 
                 (define (parse-dot str indent) 
                   (given (parse-characters "dot" str no-indent)
                          (lambda (_ str)
                            (possibility (tokenize 'Dot) str))))
                 
                 ;;;parse-unknown 
                 
                 (define (parse-unknown str indent)
                   (given (parse-characters "unknown" str no-indent)
                          (lambda (_ str)
                            (possibility (tokenize 'Unknown) str))))
                 
                 ;;;parse-identifier
                 
                 ;You may need these depending on how you define legal identifiers: "1" "2" "3" "4" "5" "6" "7" "8" "9"
                 (define numbers "1234567890")
                 (define legal-variable-characters "-+*/%_$<>0QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm")
                 (define numbers-and-legal-variable-characters (string-append numbers legal-variable-characters))
                 
                 (define (parse-identifier str indent)
                   (given (also (given (parse-n-like 1 (is-in-string legal-variable-characters) str no-indent)
                                       (lambda (name str)
                                         (if (impossible? (parse-n-like 1 (is-in-string numbers-and-legal-variable-characters) str no-indent))
                                             (possibility name str)
                                             (impossibility))))
                                (given (parse-n-like 1 (is-in-string numbers-and-legal-variable-characters) str no-indent)
                                       (lambda (first str)
                                         (given (parse-all-at-least-one-like (is-in-string numbers-and-legal-variable-characters) str no-indent)
                                                (lambda (rest str)
                                                  (possibility (string-append first rest) str))))))
                          (lambda (name str)
                            (possibility (tokenize 'Identifier name) str))))
                 
                 ;;;parse-parameter
                 
                 (define (parse-parameter str indent)
                   (given-seq (parse-characters "@" str no-indent)
                              (lambda (str)
                                (given (parse-identifier str no-indent)
                                       (lambda (name str)
                                         (possibility (tokenize 'Parameter (string-append "@" (cadr name))) str))))))
                 
                 ;;;parse-funject-literal
                 
                 (define (parse-funject-literal str indent)
                   (given (parse-characters "{" str no-indent)
                          (lambda (_ str)
                            (also (given (parse-white str no-indent)
                                         (lambda (_ str)
                                           (given (parse-characters "}" str no-indent)
                                                  (lambda (_ str)
                                                    (possibility (tokenize 'Funject-literal '()) str)))))
                                  ;Single-line funject literal:
                                  (given 
                                   (parse-white str no-indent)
                                   (lambda (_ str)
                                     (given (parse-exp str indent)
                                            (lambda (key str)
                                              (given-seq (parse-white str no-indent)
                                                         (lambda (str) 
                                                           (parse-characters ":" str no-indent))
                                                         (lambda (str)
                                                           (parse-white str no-indent))
                                                         (lambda (str)
                                                           (given (parse-sequence str indent)
                                                                  (lambda (value str)
                                                                    (given (parse-all-like (lambda (c) (or (equal? c " ") 
                                                                                                           (equal? c "\n")))
                                                                                           str 
                                                                                           no-indent)
                                                                           (lambda (_ str)
                                                                             (given (parse-characters "}" str no-indent)
                                                                                    (lambda (_ str)
                                                                                      (possibility (tokenize 'Funject-literal
                                                                                                             (list (list key value)))
                                                                                                   str)))))))))))))
                                  ;Multiline funject literal:
                                  (given-seq 
                                   (parse-white str no-indent)
                                   (lambda (str) 
                                     (parse-characters "\n" str no-indent))
                                   (lambda (str) 
                                     (parse-each parse-white-line str no-indent))
                                   (lambda (str)
                                     (also (given-seq (parse-white str indent)
                                                      (lambda (str) 
                                                        (parse-characters "}" str no-indent))
                                                      (lambda (str)
                                                        (possibility (tokenize 'Funject empty) str)))
                                           (given (parse-white str no-indent)
                                                  (lambda (spaces _)
                                                    (define indentation (string-length spaces))
                                                    #|Check|# (if (indent indentation) '() (raise "parse-funject-literal: Synatx Error: you must indent the arguments of a multiline funject literal more than the normal indentation level!"))
                                                    ;;Parse each key-value pair:
                                                    (given (parse-each (lambda (str indent)
                                                                         (given-seq (parse-each parse-white-line str indent)
                                                                                    (lambda (str)
                                                                                      (parse-characters spaces str no-indent))
                                                                                    (lambda (str)
                                                                                      (given (parse-exp str indent)
                                                                                             (lambda (key str)
                                                                                               (given-seq (parse-white str no-indent)
                                                                                                          (lambda (str)
                                                                                                            (parse-characters ":" str no-indent))
                                                                                                          (lambda (str)
                                                                                                            (parse-white str no-indent))
                                                                                                          (lambda (str)
                                                                                                            (given (parse-sequence str (lambda (ind) (< indentation ind)))
                                                                                                                   (lambda (value str)
                                                                                                                     (given-seq (parse-white str no-indent)
                                                                                                                                (lambda (str)
                                                                                                                                  (parse-characters "\n" str no-indent))
                                                                                                                                (lambda (str)
                                                                                                                                  (possibility (list key value) str))))))))))))
                                                                       str
                                                                       indent)
                                                           (lambda (pairs str)
                                                             (given-seq (parse-each parse-white-line str indent)
                                                                        (lambda (str) 
                                                                          (given (parse-all-like (is " ") str no-indent)
                                                                                 (lambda (spaces str)
                                                                                   (if (not (indent (string-length spaces)))
                                                                                       (impossibility)
                                                                                       (given-seq (parse-characters "}" str no-indent)
                                                                                                  (lambda (str)
                                                                                                    (possibility (tokenize 'Funject-literal pairs) str)))))))))))))))))))
                 
                 
                 
                 
                 
                 
                 ;;;parse-list-literal
                 
                 (define (parse-list-literal str indent)
                   (given (parse-characters "[" str no-indent)
                          (lambda (_ str)
                            (given (parse-separated (lambda (str _)
                                                      (given-seq (parse-white str no-indent)
                                                                 (lambda (str) (parse-characters "," str no-indent))
                                                                 (lambda (str) (parse-white str no-indent))))
                                                    parse-exp
                                                    str
                                                    indent)
                                   (lambda (elems str)
                                     (given (parse-characters "]" str no-indent)
                                            (lambda (_ str)
                                              (possibility (tokenize 'List-literal elems) str))))))))
                 
                 
                 ;;;parse-sequence
                 
                 (define (parse-sequence str indent)
                   (also (given (parse-exp str indent)
                                (lambda (exp str)
                                  (possibility (tokenize 'Sequence (list exp)) str)))
                         (given-seq (parse-white str no-indent)
                                    (lambda (measure-str) 
                                      (parse-characters "\n" str no-indent))
                                    (lambda (measure-str)
                                      (given (parse-all-like (is " ") measure-str no-indent)
                                             (lambda (spaces measure-str)
                                               (define indentation (string-length spaces))
                                               #|ensure indent indentation|# (if (indent indentation) '() (raise "parse-sequence: you must indent a series of statements farther than its enclosing syntactic block!"))
                                               (define (parse-white-newline-indent str indent)
                                                 (given-seq (parse-white str no-indent)
                                                            (lambda (str)
                                                              (parse-characters "\n" str no-indent))
                                                            (lambda (str)
                                                              (parse-each parse-white-line str base-indent))
                                                            (lambda (str)
                                                              (parse-characters spaces str no-indent))))
                                               (given-seq (parse-white-newline-indent str indent)
                                                          (lambda (str)
                                                            (given (parse-separated parse-white-newline-indent
                                                                                    parse-exp
                                                                                    str 
                                                                                    (is-gte indentation))
                                                                   (lambda (exps str)
                                                                     (possibility (tokenize 'Sequence exps) str)))))))))))
                 
                 ;;;parse-strict-assignment
                 
                 (define (parse-strict-assignment str indent)
                   (given (parse-identifier str no-indent)
                          (lambda (left str)
                            (given-seq (parse-white str no-indent)
                                       (lambda (str)
                                         (parse-characters "=" str no-indent))
                                       (lambda (str) 
                                         (parse-white str no-indent))
                                       (lambda (str)
                                         (given (parse-exp str indent)
                                                (lambda (right str)
                                                  (possibility (tokenize 'Strict-assignment left right) str))))))))
                 
                 ;;;parse-lazy-assignment
                 
                 (define (parse-lazy-assignment str indent)
                   (given (parse-identifier str no-indent)
                          (lambda (left str)
                            (given-seq (parse-white str no-indent)
                                       (lambda (str)
                                         (parse-characters ":=" str no-indent))
                                       (lambda (str) 
                                         (parse-white str no-indent))
                                       (lambda (str)
                                         (given (parse-sequence str indent)
                                                (lambda (right str)
                                                  (possibility (tokenize 'Lazy-assignment left right) str))))))))
                 
                 ;;;parse-funject-strict-assignment
                 
                 (define (parse-funject-strict-assignment str indent)
                   (given (parse-invocation str indent)
                          (lambda (left str)
                            (given-seq (parse-white str no-indent)
                                       (lambda (str)
                                         (parse-characters "=" str no-indent))
                                       (lambda (str)
                                         (parse-white str no-indent))
                                       (lambda (str)
                                         (given (parse-exp str indent)
                                                (lambda (right str)
                                                  (possibility (tokenize 'Funject-strict-assignment left right) str))))))))
                 
                 ;;;parse-funject-lazy-assignment
                 
                 (define (parse-funject-lazy-assignment str indent)
                   (given (parse-invocation str indent)
                          (lambda (left str)
                            (given-seq (parse-white str no-indent)
                                       (lambda (str)
                                         (parse-characters ":=" str no-indent))
                                       (lambda (str)
                                         (parse-white str no-indent))
                                       (lambda (str)
                                         (given (parse-sequence str indent)
                                                (lambda (right str)
                                                  (possibility (tokenize 'Funject-lazy-assignment left right) str))))))))
                 
                 ;;;parse-reset-strict-assignment
                 
                 (define (parse-reset-strict-assignment str indent)
                   (given (parse-identifier str no-indent)
                          (lambda (left str)
                            (given-seq (parse-white str no-indent)
                                       (lambda (str)
                                         (parse-characters "|=" str no-indent))
                                       (lambda (str) 
                                         (parse-white str no-indent))
                                       (lambda (str)
                                         (given (parse-exp str indent)
                                                (lambda (right str)
                                                  (possibility (tokenize 'Reset-strict-assignment left right) str))))))))
                 
                 ;;;parse-reset-lazy-assignment
                 
                 (define (parse-reset-lazy-assignment str indent)
                   (given (parse-identifier str no-indent)
                          (lambda (left str)
                            (given-seq (parse-white str no-indent)
                                       (lambda (str)
                                         (parse-characters "|:=" str no-indent))
                                       (lambda (str) 
                                         (parse-white str no-indent))
                                       (lambda (str)
                                         (given (parse-sequence str indent)
                                                (lambda (right str)
                                                  (possibility (tokenize 'Reset-lazy-assignment left right) str))))))))
                 
                 ;;;parse-invocation
                 
                 (define (parse-invocation str indent)
                   (given (parse-exp-not-naked-compound str indent)
                          (lambda (receiver str)
                            (parse-invocation-with receiver str indent))))
                 (define (parse-invocation-with receiver str indent)
                   (also (given (parse-white str no-indent)
                                (lambda (_ str)
                                  (given (parse-exp str indent)
                                         (lambda (args str)
                                           (also-within-naked-compound (possibility (tokenize 'Invocation receiver args) str) str indent)))))
                         (given (parse-characters "." str no-indent)
                                (lambda (_ str)
                                  (given (parse-identifier str no-indent)
                                         (lambda (property-token str)
                                           (token-contents property-token 
                                                           (lambda (property)
                                                             (also-within-naked-compound (possibility (tokenize 'Invocation 
                                                                                                                receiver 
                                                                                                                (tokenize 'List-literal
                                                                                                                          (list (tokenize 'Dot)
                                                                                                                                (tokenize 'String
                                                                                                                                          property))))
                                                                                                      str)
                                                                                         str
                                                                                         indent)))))))))
                 
                 ;;;parse-funject-inheritance
                 
                 (define (parse-funject-inheritance str indent)
                   (given (parse-exp-not-naked-compound str indent)
                          (lambda (heir str)
                            (given-seq (parse-white str no-indent)
                                       (lambda (str)
                                         (parse-characters "<<" str no-indent))
                                       (lambda (str)
                                         (parse-white str no-indent))
                                       (lambda (str)
                                         (given (parse-exp str indent)
                                                (lambda (inherited str)
                                                  (possibility (tokenize 'Funject-inheritance 
                                                                         heir
                                                                         inherited)
                                                               str))))))))
                 
                 ;;;parse-inverse-definition
                 
                 (define (parse-inverse-definition str indent)
                   (given (parse-exp-not-naked-compound str indent)
                          (lambda (f str)
                            (given-seq (parse-white str no-indent)
                                       (lambda (str)
                                         (parse-characters "<-" str no-indent))
                                       (lambda (str)
                                         (parse-white str no-indent))
                                       (lambda (str)
                                         (given (parse-exp str indent)
                                                (lambda (f-inverse str)
                                                  (possibility (tokenize 'Inverse-definition 
                                                                         f 
                                                                         f-inverse) 
                                                               str))))))))
                 
                 ;;;;begin not translating
                 (set! odsl dsl)
                 ;;;;end not translating
                 
                 (define (parse str)
                   (parse-exps str base-indent))
                 parse)))
































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                Evaluator






;;;;    Personal Library


(define mcadr (compose mcar mcdr))
(define mcaddr (compose mcar mcdr mcdr))
(define mcadddr (compose mcar mcdr mcdr mcdr))
(define mcaddddr (compose mcar mcdr mcdr mcdr mcdr))
(define mcadddddr (compose mcar mcdr mcdr mcdr mcdr mcdr))
(define mcaddddddr (compose mcar mcdr mcdr mcdr mcdr mcdr mcdr))
(define mcadddddddr (compose mcar mcdr mcdr mcdr mcdr mcdr mcdr mcdr))
(define mcaddddddddr (compose mcar mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcdr))
(define mcaadr (compose mcar mcar mcdr))
(define mcaaddr (compose mcar mcar mcdr mcdr))
(define mcaadddr (compose mcar mcar mcdr mcdr mcdr))
(define mcaaddddr (compose mcar mcar mcdr mcdr mcdr mcdr))
(define mcaadddddr (compose mcar mcar mcdr mcdr mcdr mcdr mcdr))
(define mcaaddddddr (compose mcar mcar mcdr mcdr mcdr mcdr mcdr mcdr))
(define mcaadddddddr (compose mcar mcar mcdr mcdr mcdr mcdr mcdr mcdr mcdr))
(define mcaaddddddddr (compose mcar mcar mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcdr))
(define mcaar (compose mcar mcar))
(define mcadar (compose mcar mcdr mcar))
(define mcaddar (compose mcar mcdr mcdr mcar))
(define mcadddar (compose mcar mcdr mcdr mcdr mcar))
(define mcaddddar (compose mcar mcdr mcdr mcdr mcdr mcar))
(define mcadddddar (compose mcar mcdr mcdr mcdr mcdr mcdr mcar))
(define mcaddddddar (compose mcar mcdr mcdr mcdr mcdr mcdr mcdr mcar))
(define mcadddddddar (compose mcar mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcar))
(define mcaddddddddar (compose mcar mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcar))
(define mcaadar (compose mcar mcar mcdr mcar))
(define mcaaddar (compose mcar mcar mcdr mcdr mcar))
(define mcaadddar (compose mcar mcar mcdr mcdr mcdr mcar))
(define mcaaddddar (compose mcar mcar mcdr mcdr mcdr mcdr mcar))
(define mcaadddddar (compose mcar mcar mcdr mcdr mcdr mcdr mcdr mcar))
(define mcaaddddddar (compose mcar mcar mcdr mcdr mcdr mcdr mcdr mcdr mcar))
(define mcaadddddddar (compose mcar mcar mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcar))
(define mcaaddddddddar (compose mcar mcar mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcar))
(define mcddr (compose mcdr mcdr))
(define mcdddr (compose mcdr mcdr mcdr))
(define mcddddr (compose mcdr mcdr mcdr mcdr))
(define mcdddddr (compose mcdr mcdr mcdr mcdr mcdr))
(define mcddddddr (compose mcdr mcdr mcdr mcdr mcdr mcdr))
(define mcdddddddr (compose mcdr mcdr mcdr mcdr mcdr mcdr mcdr))
(define mcddddddddr (compose mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcdr))
(define mcdar (compose mcdr mcar))
(define mcddar (compose mcdr mcdr mcar))
(define mcdddar (compose mcdr mcdr mcdr mcar))
(define mcddddar (compose mcdr mcdr mcdr mcdr mcar))
(define mcdddddar (compose mcdr mcdr mcdr mcdr mcdr mcar))
(define mcddddddar (compose mcdr mcdr mcdr mcdr mcdr mcdr mcar))
(define mcdddddddar (compose mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcar))
(define mcddddddddar (compose mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcar))
(define mcadaadr (compose mcar mcdr mcar mcar mcdr))
(define (set-mcadr! xs x) (set-mcar! (mcdr xs) x))
(define (set-mcaddr! xs x) (set-mcar! (mcddr xs) x))
(define (set-mcadddr! xs x) (set-mcar! (mcdddr xs) x))
(define (set-mcaddddr! xs x) (set-mcar! (mcddddr xs) x))
(define (set-mcadddddr! xs x) (set-mcar! (mcdddddr xs) x))
(define (set-mcaddddddr! xs x) (set-mcar! (mcddddddr xs) x))
(define (set-mcadddddddr! xs x) (set-mcar! (mcdddddddr xs) x))
(define (set-mcaddddddddr! xs x) (set-mcar! (mcddddddddr xs) x))
(define (set-mcaadr! xs x) (set-mcar! (mcadr xs) x))
(define (set-mcaaddr! xs x) (set-mcar! (mcaddr xs) x))
(define (set-mcaadddr! xs x) (set-mcar! (mcadddr xs) x))
(define (set-mcaaddddr! xs x) (set-mcar! (mcaddddr xs) x))
(define (set-mcaadddddr! xs x) (set-mcar! (mcadddddr xs) x))
(define (set-mcaaddddddr! xs x) (set-mcar! (mcaddddddr xs) x))
(define (set-mcaadddddddr! xs x) (set-mcar! (mcadddddddr xs) x))
(define (set-mcaaddddddddr! xs x) (set-mcar! (mcaddddddddr xs) x))
(define (set-mcadar! xs x) (set-mcar! (mcdar xs) x))
(define (set-mcaddar! xs x) (set-mcar! (mcddar xs) x))
(define (set-mcadddar! xs x) (set-mcar! (mcdddar xs) x))
(define (set-mcaddddar! xs x) (set-mcar! (mcddddar xs) x))
(define (set-mcadddddar! xs x) (set-mcar! (mcdddddar xs) x))
(define (set-mcaddddddar! xs x) (set-mcar! (mcddddddar xs) x))
(define (set-mcadddddddar! xs x) (set-mcar! (mcdddddddar xs) x))
(define (set-mcaddddddddar! xs x) (set-mcar! (mcddddddddar xs) x))
(define (set-mcaadar! xs x) (set-mcar! (mcadar xs) x))
(define (set-mcaaddar! xs x) (set-mcar! (mcaddar xs) x))
(define (set-mcaadddar! xs x) (set-mcar! (mcadddar xs) x))
(define (set-mcaaddddar! xs x) (set-mcar! (mcaddddar xs) x))
(define (set-mcaadddddar! xs x) (set-mcar! (mcadddddar xs) x))
(define (set-mcaaddddddar! xs x) (set-mcar! (mcaddddddar xs) x))
(define (set-mcaadddddddar! xs x) (set-mcar! (mcadddddddar xs) x))
(define (set-mcaaddddddddar! xs x) (set-mcar! (mcaddddddddar xs) x))
(define (set-mcdar! xs x) (set-mcdr! (mcar xs) x)) 
(define (mlast xs)
  (if (empty? (mcdr xs))
      (mcar xs)
      (mlast (mcdr xs))))
(define (mcopy xs)
  (if (empty? xs)
      '()
      (mcons (mcar xs) (mcopy (mcdr xs)))))

;;;;begin not translating
(define (mfoldl f init xs) 
  (foldl f init (mlist->list xs)))

(define (mapply f mxs)
  (apply f (mlist->list mxs)))

(define-syntax-rule (unless a b c) (if (not a) b c))

(define (assert . args)
  (unless (or (empty? args)
              (car args))
          (if (empty? (cdr args))
              (error "assert: something failed. I think you need to write a better error message.")
              (apply error (cdr args)))
          (void)))

(define (display-all . args)
  (for-each display args))

(define (deep-stream->list s)
                   (if (not (stream? s))
                       s
                       (map deep-stream->list (stream->list s))))

(define (deep-list->mlist xs)
                   (if (not (list? xs))
                       xs
                       (mmap deep-list->mlist (list->mlist xs))))

(define (deep-mlist->list xs)
                   (if (not (mlist? xs))
                       xs
                       (map deep-mlist->list (mlist->list xs))))


(define deep-stream->mlist (compose deep-list->mlist deep-stream->list))
;;;;end not translating

(define (partial f . args)
  (define (result . more-args)
    (apply f (append args more-args)))
  result)


(define (contents xs f)
  (apply f xs))

(define (mlist-contents xs f)
  (apply f (mlist->list xs)))

(define (tagged-list? sym exp)
  (equal? sym (mcar exp)))




;;;;    Environment
;;;;    env-<specifier>

(define create-env-pair mlist)

(define env-pair-count mlength)

(define env-pair-key-of mcar)

(define env-pair-value-of mcadr)

(define env-pairs mlist)

(define (create-env-pair-strict name val)
  (create-env-pair name (bind-as-though-sequence val)))

(define empty-env 'empty-env)


(define env-empty? (partial eq? 'empty-env))


(define (env-create pairs . maybe-parent)
  (mlist 'env
         pairs
         empty-env))


(define (env-extend pairs env)
  (mlist 'env
         pairs
         env))


(define (env-extend-one key-value env)
  (mlist 'env
         (env-pairs key-value)
         env))


(define env-pairs-of mcadr)


(define env-parent-of mcaddr)


(define (env-has? name env)
  (define (iter ps)
    (cond 
      [(empty? ps)
       (env-has? name (env-parent-of env))]
      [(equal? name (env-pair-key-of (mcar ps)))
       true]
      [else (iter (mcdr ps))]))
  (if (env-empty? env)
      false
      (iter (mcadr env))))


(define (env-get name env)
  (define (iter ps)
    (if (empty? ps)
        (if (env-empty? (env-parent-of env))
            (user-error-cannot-find-variable name)
            (env-get name (env-parent-of env)))
        (if (equal? name (env-pair-key-of (mcar ps)))
            (env-pair-value-of (mcar ps))
            (iter (mcdr ps)))))
  (iter (mcadr env)))


(define (env-set! key value env) 
  (define (iter ps)
    (cond 
      [(empty? ps)
       (set-mcadr! env
                   (mcons (create-env-pair key value) 
                          (mcadr env)))
        env]
      [(equal? key (mcaar ps))
       (set-mcdar! ps value)
       env]
      [else (iter (mcdr ps))]))
  (iter (mcadr env)))


(define (env-reset! key value env)
  (define (iter ps)
    (cond
      [(empty? ps)
       (if (env-empty? (env-parent-of env))
           (user-error-cannot-reset-unset-variable key)
           (env-reset! key value (env-parent-of env)))]
      [(equal? key (mcaar ps))
       (set-mcdar! ps value)
       env]
      [else (iter (mcdr ps))]))
  (iter (mcadr env)))


(define env-merge ;env-merge does NOT copy the environments; it merely mappends their pairs.
  ((lambda () 
     (define (env-merge-2 e1 e2)
       (cond 
         [(and (env-empty? e1)
               (env-empty? e2))
          empty-env]
         [(env-empty? e1) 
          e2]
         [(env-empty? e2) 
          e1]
         [else 
          (env-extend (mappend (env-pairs-of e1)
                               (env-pairs-of e2))
                      (env-merge-2 (env-parent-of e1)
                                   (env-parent-of e2)))]))
     
     (define (env-merge first-env . rest-env)
       (if (empty? rest-env)
           first-env
           (env-merge-2 first-env (apply env-merge rest-env))))
     env-merge)))


(define (env-all-pairs-of env)
  (if (env-empty? env)
      empty
      (mappend (env-pairs-of env) (env-all-pairs-of (env-parent-of env)))))

(define (env-copy-youngest-scope env)
  (env-extend (mcopy (env-pairs-of env)) (env-parent-of env)))


;;;;    Token types
;;;;    token-<type>?

(define token-number? (partial tagged-list? 'Token-number))
(define token-string? (partial tagged-list? 'Token-string))
(define token-boolean? (partial tagged-list? 'Token-boolean))
(define token-nil? (partial tagged-list? 'Token-nil))
(define token-dot? (partial tagged-list? 'Token-dot))
(define token-unknown? (partial tagged-list? 'Token-unknown ))
(define token-identifier? (partial tagged-list? 'Token-identifier))
(define token-parameter? (partial tagged-list? 'Token-parameter))
(define token-funject-literal? (partial tagged-list? 'Token-funject-literal))
(define token-list-literal? (partial tagged-list? 'Token-list-literal))
(define token-sequence? (partial tagged-list? 'Token-sequence))
(define token-strict-assignment? (partial tagged-list? 'Token-strict-assignment))
(define token-lazy-assignment? (partial tagged-list? 'Token-lazy-assignment))
(define token-funject-strict-assignment? (partial tagged-list? 'Token-funject-strict-assignment))
(define token-funject-lazy-assignment? (partial tagged-list? 'Token-funject-lazy-assignment))
(define token-reset-strict-assignment? (partial tagged-list? 'Token-reset-strict-assignment))
(define token-reset-lazy-assignment? (partial tagged-list? 'Token-reset-lazy-assignment))
(define token-invocation? (partial tagged-list? 'Token-invocation))
(define token-funject-inheritance? (partial tagged-list? 'Token-funject-inheritance))
(define token-inverse-definition? (partial tagged-list? 'Token-inverse-definition))

(define (token-any? exp)
  (or (token-number? exp)
      (token-string? exp)
      (token-boolean? exp)
      (token-nil? exp)
      (token-dot? exp)
      (token-unknown? exp)
      (token-identifier? exp)
      (token-parameter? exp)
      (token-funject-literal? exp)
      (token-list-literal? exp)
      (token-sequence? exp)
      (token-strict-assignment? exp)
      (token-lazy-assignment? exp)
      (token-funject-strict-assignment? exp)
      (token-funject-lazy-assignment? exp)
      (token-reset-strict-assignment? exp)
      (token-reset-lazy-assignment? exp)
      (token-invocation? exp)
      (token-funject-inheritance? exp)
      (token-inverse-definition? exp)))

(define (token-contents tokens f)
  (assert (token-any? tokens)) ;to optimize, remove this line.
  (apply f (mlist->list (mcdr tokens))))




;;;;    Language Blobs

;to optimize, change this to:
;(define create-lang mlist)
(define (create-lang . args)
  (begin (cond
           [(eq? 'Number (car args))
            (assert (= 2 (length args)) "I tried to create a Number but was passed " args)
            (assert (number? (cadr args)) "I tried to create a Number but was passed " args)]
           [(eq? 'String (car args))
            (assert (= 2 (length args)) "I tried to create a String but was passed " args)
            (assert (string? (cadr args)) "I tried to create a String but was passed " args)]
           [(eq? 'Boolean (car args))
            (assert (= 2 (length args)) "I tried to create a Boolean but was passed " args)
            (assert (boolean? (cadr args)) "I tried to create a Boolean but was passed " args)]
           [(or (eq? 'Nil (car args))
                (eq? 'Dot (car args))
                (eq? 'Unknown (car args)))
            (assert (= 1 (length args)) "I tried to create a Nil, Dot, or Unknown but was passed " args)]
           [(eq? 'List (car args))
            (assert (= 2 (length args)) "I tried to create a List but was passed " args)
            (assert (mlist? (cadr args)) "I tried to create a List but was passed " args)]
           [(eq? 'Funject (car args))
            (assert (= 5 (length args)) "I tried to create a Funject but was passed " args)
            (assert (mlist? (caddr args)) "I tried to create a Funject but was passed " args)]
           [else (display-all "Warning: I fail to recognize the type of " args "!\n")])
         (apply mlist args)))

(define (lang? sym exp)
  (eq? sym (mcar exp)))

(define (lang-any? exp)
  (and (mlist? exp)
       (not (empty? exp))
       (or (eq? (mcar exp) 'Number)
           (eq? (mcar exp) 'String)
           (eq? (mcar exp) 'Boolean)
           (eq? (mcar exp) 'Nil)
           (eq? (mcar exp) 'Dot)
           (eq? (mcar exp) 'Unknown)
           (eq? (mcar exp) 'List)
           (eq? (mcar exp) 'Funject))))

(define (lang-contents exp f)
  (assert (lang-any? exp) "I tried to take the lang-contents of a non-lang: " exp) ;to optimize, remove this line.
  (apply f (mlist->list (mcdr exp))))
         
         
;;;;    Evaluation
;;;;    eval(-<each>)

(define (eval exp env) 
  (exp env))


(define (eval-each exps env)
  (mmap (lambda (exp)
          (eval exp env))
        exps))

(define (each-evaled exps env f)
  (apply f (map (lambda (exp) 
                  (eval exp env)) 
                exps)))




;;;;    Analysis

;Tokens types to analyze:
    ;number
    ;string
    ;boolean
    ;nil
    ;dot
    ;unknown 
    ;identifier
    ;parameter
    ;funject-literal
    ;list-literal
    ;sequence
    ;strict-assignment
    ;lazy-assignment
    ;funject-strict-assignment
    ;funject-lazy-assignment
    ;reset-strict-assignment
    ;reset-lazy-assignment
    ;invocation
    ;funject-inheritance
    ;inverse-definition

(define (analyze tokens)
  (cond 
    [(token-number? tokens) (analyze-number tokens)]
    [(token-string? tokens) (analyze-string tokens)]
    [(token-boolean? tokens) (analyze-boolean tokens)]
    [(token-nil? tokens) (analyze-nil tokens)]
    [(token-dot? tokens) (analyze-dot tokens)]
    [(token-unknown? tokens) (analyze-unknown  tokens)]
    [(token-identifier? tokens) (analyze-identifier tokens)]
    [(token-parameter? tokens) (analyze-parameter tokens)]
    [(token-funject-literal? tokens) (analyze-funject-literal tokens)]
    [(token-list-literal? tokens) (analyze-list-literal tokens)]
    [(token-sequence? tokens) (analyze-sequence tokens)]
    [(token-strict-assignment? tokens) (analyze-strict-assignment tokens)]
    [(token-lazy-assignment? tokens) (analyze-lazy-assignment tokens)]
    [(token-funject-strict-assignment? tokens) (analyze-funject-strict-assignment tokens)]
    [(token-funject-lazy-assignment? tokens) (analyze-funject-lazy-assignment tokens)]
    [(token-reset-strict-assignment? tokens) (analyze-reset-strict-assignment tokens)]
    [(token-reset-lazy-assignment? tokens) (analyze-reset-lazy-assignment tokens)]
    [(token-invocation? tokens) (analyze-invocation tokens)]
    [(token-funject-inheritance? tokens) (analyze-funject-inheritance tokens)]
    [(token-inverse-definition? tokens) (analyze-inverse-definition tokens)]
    [else (error "analyze: I fail to recognize the token " tokens)]))


(define (each-analyzed tokenses f)
  (apply f (map analyze tokenses)))



;analyze-<type>
(define (analyze-number tokens)
  (lambda (env) (create-lang 'Number (mcadr tokens))))


(define (analyze-string tokens)
    (lambda (env) (create-lang 'String (mcadr tokens))))


(define (analyze-boolean tokens)
    (lambda (env) (create-lang 'Boolean (mcadr tokens))))

  
(define (analyze-nil tokens)
    (lambda (env) (create-lang 'Nil)))
  

(define (analyze-dot tokens)
    (lambda (env) (create-lang 'Dot)))


(define (analyze-unknown tokens)
    (lambda (env) (create-lang 'Unknown)))
  

(define (analyze-identifier tokens)
  (let ((name (mcadr tokens)))
    (lambda (env)
      (lookup-identifier name env))))


(define analyze-parameter analyze-identifier)


(define analyze-funject-literal ((lambda ()
                                   (define (analyze-pairs ps)
                                     (mmap (lambda (p)
                                             (set-mcadr! p 
                                                         (analyze-sequence (mcadr p)))
                                             p)
                                           ps))
                                   (define (bind-pairs ps env)
                                     (mmap (lambda (p) 
                                             (bind-funject-pair p env))
                                           ps))
                                   (define (analyze-funject-literal tokens)
                                     (let* ((apairs (analyze-pairs (mcadr tokens))))
                                       (lambda (env)
                                         (let ((bpairs (bind-pairs apairs env)))
                                           (create-lang 'Funject 
                                                        (random 100000000)
                                                        bpairs
                                                        primitive-funject-god
                                                        primitive-funject-inverse-god)))))
                                   analyze-funject-literal)))


(define (analyze-list-literal tokens)
  (let ((aelems (mmap analyze (mcadr tokens))))
    (lambda (env)
      (create-lang 'List (eval-each aelems env)))))


(define (analyze-sequence tokens) (mlist 'Analyzed-sequence (mmap analyze (mcadr tokens))))


(define (analyze-strict-assignment tokens) 
  (let ((left-name (mcadr (mcadr tokens)))
        (aright (analyze (mcaddr tokens))))
    (lambda (env)
      (let ((eright (eval aright env)))
        (assign-strict-identifier! left-name
                                   eright
                                   env)
        eright))))


(define (analyze-lazy-assignment tokens)
  (let ((left-name (mcadr (mcadr tokens)))
        (aright (analyze (mcaddr tokens))))
    (lambda (env)
      (assign-lazy-identifier! left-name 
                              aright 
                              env)
      lang-nil)))


(define (analyze-funject-strict-assignment tokens)
  (token-contents tokens
                  (lambda (invocation right)
                    (token-contents invocation
                                    (lambda (receiver pattern)
                                      (each-analyzed (list receiver right)
                                                     (lambda (areceiver aright)
                                                       (lambda (env)
                                                         (each-evaled (list areceiver aright) env
                                                                      (lambda (ereceiver eright)
                                                                        (push-funject-pair! ereceiver (bind-funject-pair (create-funject-pair pattern eright) global-env))
                                                                        eright))))))))))


(define (analyze-funject-lazy-assignment tokens)
  (token-contents tokens
                  (lambda (invocation right)
                    (token-contents invocation
                                    (lambda (receiver pattern)
                                      (each-analyzed (list receiver right)
                                                     (lambda (areceiver aright)
                                                       (lambda (env)
                                                         (each-evaled (list areceiver) env
                                                                      (lambda (ereceiver)
                                                                        (push-funject-pair! ereceiver (bind-funject-pair (create-funject-pair pattern aright) env))
                                                                        lang-nil))))))))))


(define (analyze-reset-strict-assignment tokens) 
  (token-contents tokens 
                  (lambda (left right)
                    (let ((left-name (mcadr left))
                          (aright (analyze right)))
                      (lambda (env)
                        (if (not (equal? env global-env))
                            (let ((eright (eval aright env)))
                              (reset-strict-identifier! left-name 
                                                        eright 
                                                        env)
                              eright)
                            (user-error "analyze-reset-strict-assignment" 
                                        "I cannot reset the variable \"" left-name "\" in the global scope!")))))))
                        

(define (analyze-reset-lazy-assignment tokens) 
  (token-contents tokens 
                  (lambda (left right)
                    (let ((left-name (mcadr left))
                          (aright (analyze right)))
                      (lambda (env)
                        (if (not (equal? env global-env))
                            (reset-lazy-identifier! left-name 
                                                    aright 
                                                    env)
                            (user-error "analyze-reset-lazy-assignment" 
                                        "I cannot reset the variable \"" left-name "\" in the global scope!")))))))


(define (analyze-invocation tokens)
  (token-contents tokens
                  (lambda (receiver arg)
                    (let ((areceiver (analyze receiver))
                          (aarg (analyze arg)))
                      (lambda (env)
                        (let ((ereceiver (eval areceiver env))
                              (earg (eval aarg env)))
                          (invoke ereceiver earg)))))))


(define (analyze-funject-inheritance tokens) 
  (token-contents tokens
                  (lambda (heir inherited)
                    (each-analyzed (list heir inherited)
                                   (lambda (aheir ainherited)
                                     (lambda (env)
                                       (let ((eheir (eval aheir env))
                                             (einherited (eval ainherited env)))
                                         (set-funject-parent! eheir einherited)
                                         eheir)))))))


(define (analyze-inverse-definition tokens) 
  (token-contents tokens
                  (lambda (left right)
                    (each-analyzed (list left right)
                                   (lambda (aleft aright)
                                     (lambda (env)
                                       (let ((eleft (eval aleft env))
                                             (eright (eval aright env)))
                                         (set-funject-inverse! eleft eright)
                                         eleft)))))))




;;;;    invoke

(define (invoke receiver arg . maybe-own)
  (define own (if (empty? maybe-own)
                  receiver
                  (car maybe-own)))
  (cond 
    [(primitive? receiver) (invoke-primitive receiver arg own)]
    [(lang? 'Number receiver) (invoke-number receiver arg own)]
    [(lang? 'Funject receiver) (invoke-funject receiver arg own)]
    [else (error "invoke: I know not how to invoke " receiver "!")]))

(define (invoke-primitive receiver arg own)
  (primitive-contents receiver
                      (lambda (funject _)
                        (funject arg own))))

(define (invoke-number receiver arg own)
  (unless (lang? 'String arg)
          (invoke primitive-funject-god arg own)
          (lang-contents arg
                         (lambda (str)
                           (cond 
                             [(equal? str "+") (create-primitive-number-plus (mcadr receiver))]
                             [(equal? str "-") (create-primitive-number-minus (mcadr receiver))]
                             [(equal? str "*") (create-primitive-number-times (mcadr receiver))]
                             [(equal? str "/") (create-primitive-number-div (mcadr receiver))]
                             [else (invoke primitive-funject-god arg own)])))))

                 

(define (invoke-funject receiver arg own)
  (let ((apairs (funject-pairs-of receiver))
        (parent (funject-parent-of receiver))
        (inverse (funject-inverse-of receiver)))
    (define (iter apairs)
      (if (empty? apairs)
          (if parent
              (invoke parent arg receiver)
              (user-error-no-matching-pattern receiver arg))
          (let* ((pair (mcar apairs))
                 (pattern (mcar pair))
                 (consequent (mcadr pair))
                 (env (mcaddr pair))
                 (bindings (choice-bindings-from-matching pattern arg env)))
            (if bindings
                (if (lang? 'Analyzed-sequence consequent)
                    (force-sequence (bind-sequence consequent 
                                                   (env-extend (env-pairs (create-env-pair "own" 
                                                                                           (bind-as-though-sequence own)))
                                                               bindings)))
                    consequent)
                (iter (mcdr apairs))))))
    (iter apairs)))




;(set-)funject-<prop>(-of)

(define (funject-pairs-of funject)
  (unless (lang? 'Funject funject) ;to optimize, remove this condition.
          (error "I cannot find the pairs of a non-funject: " funject "!")
          (mcaddr funject)))

(define (set-funject-pairs! funject pairs)
  (unless (lang? 'Funject funject) ;to optimize, remove this condition.
          (error "I cannot set the pairs of a non-funject: " funject "!")
          (set-mcaddr! funject pairs)))

(define (push-funject-pair! funject p)
  (unless (lang? 'Funject funject) ;to optimize, remove this condition.
          (user-error-cannot-push-pair-to-non-funject)
          (set-mcaddr! funject (mcons p (mcaddr funject)))))

(define (funject-parent-of funject)
  (unless (lang? 'Funject funject) ;to optimize, remove this condition.
          (error "I cannot find the parent of a non-funject: " funject "!")
          (mcadddr funject)))

(define (set-funject-parent! funject parent)
  (unless (lang? 'Funject funject) ;to optimize, remove this condition.
          (user-error-cannot-set-parent-of-non-funject)
          (set-mcadddr! funject parent)))

(define (funject-inverse-of funject)
  (cond 
    [(lang? 'Funject funject)
     (mcaddddr funject)]
    [(primitive? funject)
     (primitive-contents funject
                         (lambda (itself inverse)
                           (create-primitive inverse itself)))]
    [else
     (error "I cannot find the inverse of a non-funject or primitive: " funject "!")]))

(define (set-funject-inverse! funject inverse)
  (unless (lang? 'Funject funject)
          (user-error-set-inverse-of-non-funject)
          (set-mcaddddr! funject inverse)))

(define create-funject-pair mlist)

(define create-funject-bound-pair mlist)




;<operation>(-<type>)-identifier
(define (lookup-identifier name env)
  ((env-get name env))) ;note the extra parenthases

(define (assign-strict-identifier! name right env)
  (env-set! name (bind-as-though-sequence right) env))

(define (assign-lazy-identifier! name right env)
  (env-set! name (bind-sequence right env) env))

(define (reset-strict-identifier! name right env)
  (env-reset! name (bind-as-though-sequence right) (env-parent-of env)))

(define (reset-lazy-identifier! name right env) 
  (env-reset! name (bind-sequence right env) (env-parent-of env)))

;bind-{sequence/funject-pair}
;force-sequence
(define (bind-sequence exp env)
  (let ((statements (mcadr exp)))
    (lambda () 
      (mlast (eval-each statements env)))))

(define (bind-funject-pair p env)
  (mlist-contents p
                  (lambda (pattern consequent)
                    (create-funject-bound-pair pattern consequent env))))

(define (force-sequence stmts) (stmts))

(define (bind-as-though-sequence exp) 
  (lambda () exp))






;;;;    Bindings-from-matching

(define choice-bindings-from-matching 
  ((lambda () 
     
     
     
     
     ;;;;    Possibility procedures
     
     (define possibility list)
     (define possible? (compose not empty?))
     (define impossibility list)
     (define impossible? empty?)
     (define possibility-first car)
     (define (given possibilities iterator)
       (flatten (map iterator possibilities)))
     (define also append)
     (define lang-list->possibilities (compose mlist->list mcadr))
     
     
     
     
     ;;;;    Find unknown parameters
     
     (define find-unknown-parameters
       ((lambda ()
          (define (find-unknown-parameters pattern bindings env found)
            (cond [(token-parameter? pattern)
                   (if (or (env-has? (mcadr pattern) bindings)
                           (mmember (mcadr pattern) found))
                       found
                       (mcons (mcadr pattern) found))]
                  [(token-invocation? pattern)
                   (find-unknown-parameters (mcadr pattern)  ;I search the mcadr (receiver) only for the day when the programmer can include parameters there.
                                            bindings
                                            env
                                            (find-unknown-parameters (mcaddr pattern)
                                                                     bindings
                                                                     env
                                                                     found))]
                  [(token-list-literal? pattern)
                   (mfoldl (lambda (e found)
                            (find-unknown-parameters e bindings env found))
                           found
                           (mcadr pattern))]
                  [(or (token-number? pattern)
                       (token-string? pattern)
                       (token-boolean? pattern)
                       (token-nil? pattern)
                       (token-dot? pattern)
                       (token-unknown? pattern)
                       (not (token-any? pattern))
                       (token-identifier? pattern))
                   found]
                  [(or (token-strict-assignment? pattern)
                       (token-lazy-assignment? pattern)
                       (token-funject-strict-assignment? pattern)
                       (token-funject-lazy-assignment? pattern)
                       (token-inverse-definition? pattern)
                       (token-funject-inheritance? pattern)
                       (token-funject-literal? pattern))
                   (user-error-funject-pattern-cannot-contain pattern)]
                  [else (error "find-unknown-parameters: I fail to account for the type of " pattern "!")]))
          
          (lambda (pattern bindings env)
            (find-unknown-parameters pattern bindings env '())))))
     
     
     
     
     ;;;;    Bindings-from-matching-once: a mega proc
     
     (define (bindings-from-matching-once pattern arg bindings env)
       (cond 
         [(or (token-number? pattern)
              (token-string? pattern)
              (token-boolean? pattern)
              (token-nil? pattern)
              (token-dot? pattern)
              (token-unknown? pattern))
          (bindings-from-matching-flat pattern arg bindings env)]
         [(not (token-any? pattern))
          (if (lang-equal? pattern arg)
              (possibility bindings)
              (impossibility))]
         [(or (token-strict-assignment? pattern)
              (token-lazy-assignment? pattern)
              (token-funject-strict-assignment? pattern)
              (token-funject-lazy-assignment? pattern)
              (token-inverse-definition? pattern)
              (token-funject-inheritance? pattern)
              (token-funject-literal? pattern))
          (user-error-funject-pattern-cannot-contain pattern)]
         [(token-list-literal? pattern)
          (bindings-from-matching-list-literal pattern arg bindings env)]
         [(token-parameter? pattern)
          (bindings-from-matching-parameter pattern arg bindings env)]
         [(token-identifier? pattern)
          (bindings-from-matching-identifier pattern arg bindings env)]
         [(token-invocation? pattern)
          (bindings-from-matching-invocation pattern arg bindings env)]
         [else (error "bindings-from-matching: I fail to account for the type of " pattern "!")]))
     
     
     (define (bindings-from-matching-flat pattern arg bindings env)
       (if (lang-equal? arg (eval (analyze pattern) env))
           (possibility bindings)
           (impossibility)))
     
     
     (define bindings-from-matching-list-literal
       ((lambda () 
          (define (iter elems arg-elems bindings env)
            (if (empty? elems)
                (if (empty? arg-elems)
                    (possibility bindings)
                    (impossibility))
                (given (bindings-from-matching-once (mcar elems)
                                                    (mcar arg-elems)
                                                    bindings
                                                    env)
                       (lambda (bindings)
                         (iter (mcdr elems)
                               (mcdr arg-elems)
                               bindings
                               env)))))
          (define (bindings-from-matching-list-literal pattern arg bindings env) 
            (if (not (lang? 'List arg))
                (impossibility)
                (token-contents pattern
                                (lambda (elems)
                                  (lang-contents arg
                                                 (lambda (arg-elems)
                                                   (iter elems arg-elems bindings env)))))))
          bindings-from-matching-list-literal)))
     
     
     (define (bindings-from-matching-parameter pattern arg bindings env)
       (token-contents pattern
                       (lambda (name)
                         (cond 
                           [(not (env-has? name bindings)) 
                            (let ((new-bindings (env-copy-youngest-scope bindings)))
                              (assign-strict-identifier! name arg new-bindings)
                              (possibility new-bindings))]
                           [(lang-equal? arg (lookup-identifier name bindings))
                            (possibility bindings)]
                           [else (impossibility)]))))
     
     
     (define (bindings-from-matching-identifier pattern arg bindings env)
       (if (lang-equal? (eval (analyze pattern) env) 
                        arg)
           (possibility bindings)
           (impossibility)))
     
     
     (define (bindings-from-matching-invocation pattern arg bindings env)
       (if (< 1 (mlength (find-unknown-parameters pattern bindings env)))
           (possibility bindings)
           (token-contents pattern
                           (lambda (receiver pattern-arg)
                             (let* ((ereceiver (eval (analyze receiver) env))
                                    (unknowns-epattern-arg (eval-pattern-arg pattern-arg bindings env))
                                    (unknowns (car unknowns-epattern-arg))
                                    (epattern-arg (cadr unknowns-epattern-arg))
                                    (inverse (invoke (funject-inverse-of ereceiver) (create-lang 'List (mlist arg epattern-arg))))
                                    (possibilities (lang-list->possibilities inverse)))
                               (if (not (= 1 (length unknowns)))
                                   (possibility bindings)
                                   (let ((to-match (car unknowns)))
                                     (given possibilities
                                            (lambda (value)
                                              (let ((new-bindings (env-copy-youngest-scope bindings)))
                                                (assign-strict-identifier! to-match value new-bindings)
                                                new-bindings))))))))))
     
     
     
     
     ;;;;    Eval-pattern-arg :: tokens -> env -> env -> (list (list) lang)
     
     (define eval-pattern-arg
       ((lambda ()
          (define (eval-pattern-arg pattern-arg bindings env)
            (cond
              [(token-number? pattern-arg) (list empty (create-lang 'Number (mcadr pattern-arg)))]
              [(token-string? pattern-arg) (list empty (create-lang 'String (mcadr pattern-arg)))]
              [(token-boolean? pattern-arg) (list empty (create-lang 'Boolean (mcadr pattern-arg)))]
              [(token-nil? pattern-arg) (list empty (create-lang 'Nil))]
              [(token-dot? pattern-arg) (list empty (create-lang 'Dot))]
              [(token-unknown? pattern-arg) (list empty (create-lang 'Unknown))]
              [(token-parameter? pattern-arg) 
               (if (env-has? (mcadr pattern-arg) bindings)
                   (list empty (lookup-identifier (mcadr pattern-arg) bindings))
                   (list (list (mcadr pattern-arg)) lang-unknown))]
              [(token-identifier? pattern-arg)
               (list empty (lookup-identifier (mcadr pattern-arg) env))]
              [(token-list-literal? pattern-arg) 
               (eval-pattern-arg-list-literal pattern-arg bindings env)]            
              [else (error "eval-pattern-arg: I fail to recognize the token " (deep-stream->list pattern-arg))]))
          
          (define (eval-pattern-arg-list-literal pattern-arg bindings env)
            (define (iter elems)
              (if (empty? elems)
                  (list empty empty)
                  (let* ((first-unknowns--first-evaled (eval-pattern-arg (mcar elems) bindings env))
                         (first-unknowns (car first-unknowns--first-evaled))
                         (first-evaled (cadr first-unknowns--first-evaled))
                         (rest-unknowns--rest-evaled (iter (mcdr elems)))
                         (rest-unknowns (car rest-unknowns--rest-evaled))
                         (rest-evaled (cadr rest-unknowns--rest-evaled)))
                    (list (append first-unknowns rest-unknowns) 
                          (mcons first-evaled rest-evaled)))))
            (let* ((unknowns--elems-evaled (iter (mcadr pattern-arg)))
                   (unknowns (car unknowns--elems-evaled))
                   (elems-evaled (cadr unknowns--elems-evaled)))
              (list unknowns (create-lang 'List elems-evaled))))
          eval-pattern-arg)))
     
     
     
     
     ;;;;    Bindings-from-matching
     
     (define (bindings-from-matching pattern arg bindings env)
       (define n (mlength (find-unknown-parameters pattern bindings env)))
       (define (iter last-found bindings)
         (given (bindings-from-matching-once pattern arg bindings env)
                (lambda (bindings)
                  (let ((now-found (env-pair-count (env-pairs-of bindings))))
                    (cond
                      [(= n now-found)
                       (possibility bindings)]
                      [(= last-found now-found)
                       (impossibility)]
                      [else
                       (iter now-found bindings)])))))
       (iter 0 bindings))
     
     
     
     
     ;;;; Choice-bindings-from-matching
     
     (define (choice-bindings-from-matching pattern arg env)
       (let ((possibilities (bindings-from-matching pattern 
                                                    arg 
                                                    (env-create (env-pairs))
                                                    env)))
         (if (impossible? possibilities)
             false
             (env-extend (env-all-pairs-of (possibility-first possibilities)) env))))
     choice-bindings-from-matching)))






;;;;    Primitives

;Primitive funjects rely on these:
(define lang-nil (create-lang 'Nil))
(define lang-dot (create-lang 'Dot))
(define lang-unknown (create-lang 'Unknown))
(define lang-equal? equal?)

(define (create-primitive funject inverse)
  (mlist 'Primitive funject inverse))

(define (primitive? exp)
  (and (mlist? exp)
       (not (empty? exp))
       (eq? 'Primitive (mcar exp))))

(define (primitive-contents prim f)
  (assert (primitive? prim)) ;to optimize, remove this line.
  (apply f (mlist->list (mcdr prim))))

(define (create-primitive-infix-operator right-type? result-type? op op-inv)
  (define self (lambda (left)
                 (create-primitive (lambda (other own)
                                     (unless (right-type? other)
                                       (invoke primitive-funject-god other)
                                       (op left 
                                           other)))
                                   (lambda (result--arg own)
                                     (lang-contents result--arg
                                                    (lambda (elems)
                                                      (mlist-contents elems
                                                                      (lambda (result arg)
                                                                        (unless (and (result-type? result)
                                                                                     (lang? 'Unknown arg))
                                                                          (invoke primitive-funject-inverse-god result--arg self)
                                                                          (op-inv result left))))))))))
  self)

(define (create-primitive-unoverloaded-infix-operator right-type result-type op op-inv)
  (create-primitive-infix-operator (partial lang? right-type)
                                   (partial lang? result-type)
                                   (lambda (left right)
                                     (lang-contents right
                                                    (lambda (right-contents)
                                                      (create-lang result-type (op left right-contents)))))
                                   (lambda (result left)
                                     (lang-contents result
                                                    (lambda (result-contents)
                                                      (create-lang 'List (mlist (create-lang right-type (op-inv result-contents left)))))))))

(define create-primitive-funject-is (create-primitive-infix-operator lang-any? ;to optimize, change this to sycophant
                                                                     lang-any? ;to optimize, change this to sycophant
                                                                     (lambda (a b)
                                                                       (create-lang 'Boolean (lang-equal? a b)))
                                                                     (lambda (result left)
                                                                       (if (equal? result (mlist 'Boolean true))
                                                                           (create-lang 'List left)
                                                                           (user-error-no-matching-pattern (list "is of " left) (create-lang 'List result (create-lang 'Unknown)))))))

(define create-primitive-number-plus (create-primitive-unoverloaded-infix-operator 'Number 'Number + -))

(define create-primitive-number-minus (create-primitive-unoverloaded-infix-operator 'Number 'Number - +))

(define create-primitive-number-times (create-primitive-unoverloaded-infix-operator 'Number 'Number * /))

(define create-primitive-number-div (create-primitive-unoverloaded-infix-operator 'Number 'Number / *))




(define primitive-funject-god
  (create-primitive (lambda (arg own)
                      (cond 
                        [(lang? 'String arg)
                         (lang-contents arg
                                        (lambda (str)
                                          (cond 
                                            [(equal? str "is") (create-primitive-funject-is own)]
                                            [else (user-error-no-matching-pattern own arg)])))]
                        [else (user-error-no-matching-pattern "The primitive funject god inversted" arg)]))
                    (lambda (arg own)
                      (user-error-no-matching-pattern "The primitive funject god inversted" arg))))


(define primitive-funject-inverse-god (create-primitive (mcaddr primitive-funject-god) (mcadr primitive-funject-god)))



(define global-env (env-create (env-pairs (create-env-pair "Yin" primitive-funject-god)
                                          (create-env-pair "Yang" primitive-funject-inverse-god)
                                          (create-env-pair-strict "+" (create-lang 'String "+"))
                                          (create-env-pair-strict "-" (create-lang 'String "-"))
                                          (create-env-pair-strict "*" (create-lang 'String "*"))
                                          (create-env-pair-strict "/" (create-lang 'String "/"))
                                          (create-env-pair-strict "is" (create-lang 'String "is")))))




;;;;    user-error

(define (user-error . args)
  (apply error (cons "User Error: " args)))

(define (user-error-cannot-find-variable var)
  (user-error "I cannot find the variable " var "!"))

(define (user-error-no-matching-pattern receiver arg) (user-error "I find no pattern matching " arg " in " receiver))

(define (user-error-cannot-reset-unset-variable var) (user-error "You tried to reset the variable " var ", but you haven't even assigned it yet!"))

(define (user-error-cannot-push-pair-to-non-funject) (user-error "I cannot alter the patterns of a non-funject!"))

(define (user-error-cannot-set-parent-of-non-funject) (user-error "I cannot set the parent of a non-funject!"))

(define (user-error-set-inverse-of-non-funject) (user-error "I cannot set the inverse of a non-funject!"))

(define (user-error-funject-pattern-cannot-contain pattern) (user-error "A funject pattern cannot contain a funject of the type of " pattern))

(define (user-error-multiple-unknowns-in-pattern-arg) (user-error "A funject pattern cannot contain multiple unknown matching variables!")) 



 
































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                Testing

;;;begin not translating
(define v deep-mlist->list)
(define p1 (compose car stream-first parse))
(define (t str) (deep-list->mlist (p1 str)))
(define (interpret str)
  (mmap (lambda (exp)
         (eval exp global-env))
       (mmap analyze (deep-list->mlist (p1 str)))))
(define (interpret-verbose str)
  (display "I parse the syntax...\n")
  (let ((parsed (p1 str)))
    (display "Then I analyze the expressions...\n")
    (let ((analyzed (mmap analyze (deep-list->mlist parsed))))
      (display "And finally I evaluate them:\n")
      (mmap (lambda (exp)
              (eval exp global-env))
            analyzed))))
(define i interpret-verbose)
(define j (compose deep-mlist->list i))
;;;;end not translating