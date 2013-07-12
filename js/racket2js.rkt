#lang racket
(define (zip . xss) 
  (apply map (cons list xss)))

(define (->string x)
  (cond ((string? x) x)
        ((number? x) (number->string x))
        ((symbol? x) (symbol->string x))
        ((boolean? x) (if x "true" "false"))
        (else (error " ! ->string: I cannot convert " x " to a string!"))))

(define (to-upper c)
  (cond
    [(eq? c "q") "Q"]
    [(eq? c "w") "W"]
    [(eq? c "e") "E"]
    [(eq? c "r") "R"]
    [(eq? c "t") "T"]
    [(eq? c "y") "Y"]
    [(eq? c "u") "U"]
    [(eq? c "i") "I"]
    [(eq? c "o") "O"]
    [(eq? c "p") "P"]
    [(eq? c "a") "A"]
    [(eq? c "s") "S"]
    [(eq? c "d") "D"]
    [(eq? c "f") "F"]
    [(eq? c "g") "G"]
    [(eq? c "h") "H"]
    [(eq? c "j") "J"]
    [(eq? c "k") "K"]
    [(eq? c "l") "L"]
    [(eq? c "z") "Z"]
    [(eq? c "x") "X"]
    [(eq? c "c") "C"]
    [(eq? c "v") "V"]
    [(eq? c "b") "B"]
    [(eq? c "n") "N"]
    [(eq? c "m") "M"]
    [else c]))

(define (to-lower c)
  (cond 
    [(eq? c "Q") "q"]
    [(eq? c "W") "w"]
    [(eq? c "E") "e"]
    [(eq? c "R") "r"]
    [(eq? c "T") "t"]
    [(eq? c "Y") "y"]
    [(eq? c "U") "u"]
    [(eq? c "I") "i"]
    [(eq? c "O") "o"]
    [(eq? c "P") "p"]
    [(eq? c "A") "a"]
    [(eq? c "S") "s"]
    [(eq? c "D") "d"]
    [(eq? c "F") "f"]
    [(eq? c "G") "g"]
    [(eq? c "H") "h"]
    [(eq? c "J") "j"]
    [(eq? c "K") "k"]
    [(eq? c "L") "l"]
    [(eq? c "Z") "z"]
    [(eq? c "X") "x"]
    [(eq? c "C") "c"]
    [(eq? c "V") "v"]
    [(eq? c "B") "b"]
    [(eq? c "N") "n"]
    [(eq? c "M") "m"]
    [else c]))

(define (js-exp . args)
  (apply string-append (append '("(") (map ->string args) '(")"))))

(define (js-stmt . args)
  (apply string-append (append '("(") (map ->string args) '(");"))))

(define (join str exps)
  (let ((exps (map ->string exps)))
    (if (empty? exps)
        ""
        (foldl (lambda (e acc)
                 (string-append acc str e))
               (car exps)
               (cdr exps)))))
  
(define (tagged-list? exp tag)
  (eq? tag (car exp)))

(define (unempty-list? exp)
  (and (list? exp)
       (not (empty? exp))))

(define (quoted? exp)
  (and (unempty-list? exp)
       (eq? 'quote (car exp))))
(define quoted-contents cadr)

(define (string->js str)
  (string-append "\""
                 (string-replace (string-replace (string-replace str "\\"  "\\\\") "\"" "\\\"") "\n" "\\n")
                 "\""))

(define (definately-quote x)
  (list 'quote x))

(define (ununquote x)
  (if (or (number? x)
          (string? x)
          (boolean? x))
      x
      (definately-quote x)))
  

;;  racket-symbol
(define (racket-symbol? exp) 
  (and (quoted? exp)
       (symbol? (cadr exp))))

(define (translate-racket-symbol exp)
  (let ((sym (cadr exp)))
    (js-exp "Symbol(\""
            (string-replace (->string sym) "\"" "\\\"")
            "\")")))



;;  racket-number
(define (racket-number? exp)
  (or (number? exp)
      (and (quoted? exp)
           (number? (quoted-contents exp)))))

(define (translate-racket-number exp)
  (if (number? exp)
      (->string exp)
      (->string (quoted-contents exp))))
      
      


;;  racket-string
(define (racket-string? exp)
  (or (string? exp)
      (and (quoted? exp)
           (string? (quoted-contents exp)))))

(define (translate-racket-string exp)
  (string->js (if (string? exp)
                  exp
                  (quoted-contents exp))))



;;  racket-list
(define (racket-list? exp)
  (and (quoted? exp)
       (list? (quoted-contents exp))))

(define (translate-racket-list exp) 
  (let ((xs (quoted-contents exp)))
    (js-exp "["
            (join ", " (map (compose translate ununquote) xs))
            "]")))



;;  racket-boolean
(define (racket-boolean? exp) 
  (or (boolean? exp)
      (eq? exp 'true)
      (eq? exp 'false)))

(define (translate-racket-boolean exp)
  (->string (if (boolean? exp)
                exp
                (cond [(eq? 'true exp) true]
                      [(eq? 'false exp) false]
                      [else (error "translate-racket-boolean: I fail to recognize the boolean expression " exp "!")]))))



;;  identifier
(define (identifier? exp)
  (symbol? exp))

(define (remove-if-final-? str)
  (let ((split (string-split str "?")))
    (if (equal? str (string-append (car split) "?"))
        (string-append "is_" (car split))
        str)))

(define (replace-if-equal str . substs)
  (foldl (lambda (subst str)
           (if (equal? str (car subst))
               (cdr subst)
               str))
         str
         substs))

(define (translate-identifier exp)
  (remove-if-final-? (replace-if-equal (string-replace (string-replace (string-replace (string-replace (string-replace (string-replace (string-replace (->string exp) 
                                                                                                                                                       "->" "_to_") 
                                                                                                                                       "-" "_") 
                                                                                                                       "!" "_racket_exclamation_point")
                                                                                                       "+" "_racket_plus_symbol")
                                                                                       "-" "_racket_minus_symbol")
                                                                       "*" "_racket_mult_symbol")
                                                       "/" "_racket_division_symbol")
                                       (cons "var" "_var_escaped_due_to_significance_in_js"))))
                                     



;; assignment
(define (assignment? exp)
  (eq? 'set! (car exp)))

(define (translate-assignment exp)
  (js-exp (translate (cadr exp))
          " = "
          (translate (caddr exp))))



;; definition
(define (definition? exp) 
  (eq? 'define (car exp)))

(define (translate-definition exp)
  (if (symbol? (cadr exp))
      (string-append "var "
                     (translate-identifier (cadr exp))
                     " = "
                     (translate (caddr exp))
                     ";")
      (string-append "var "
                     (translate-identifier (caadr exp))
                     " = "
                     (translate (append (list 'lambda)
                                        (list (cdadr exp))
                                        (cddr exp)))
                     ";")))



;; if
(define (if? exp) 
  (eq? 'if (car exp)))

(define (translate-if exp) 
  (let ((cond (cadr exp))
        (conseq (caddr exp))
        (altern (cadddr exp)))
    (js-exp "(false !== ("
            (translate cond)
            ")) ? ("
            (translate conseq)
            ") : ("
            (translate altern)
            ")")))



;; lambda
(define (lambda? exp)
  (eq? 'lambda (car exp)))

(define (separate-named-args args)
  (if (pair? args)
      (cons (car args)
            (separate-named-args (cdr args)))
      '()))

(define (separate-args-collector args) 
  (if (pair? args)
      (separate-args-collector (cdr args))
      args))

(define (translate-lambda exp)
  (let ((arguments (cadr exp))
        (body-exps (take (cddr exp) (- (length (cddr exp)) 1)))
        (last-exp  (last (cddr exp))))
    (js-exp (if (list? arguments)
                (string-append "function ("
                               (join ", " (map translate arguments))
                               ") {")
                (let ((named-args (separate-named-args arguments))
                      (args-collector (separate-args-collector arguments)))
                  (string-append "function ("
                                 (join ", " (map translate named-args))
                                 ") { \n    var "
                                 (translate args-collector)
                                 " = array2nested_pairs(Array.prototype.slice.call(arguments, " ; if I revert to Racket list -> JS Array, change this line to " = Array.prototype.slice.call(arguments, "
                                 (number->string (length named-args))                           ; and change the line two down to have only one paren.
                                 ")); \n    ")))
            (join "; \n    " (map translate body-exps))
            (if (empty? body-exps) "\n" ";\n")
            "    return ("
            (translate last-exp)
            "); \n}")))
            


;; begin
(define (begin? exp) 
  (eq? 'begin (car exp)))

(define (translate-begin exp) 
  (let ((exps (cdr exp)))
    (join ", " (map translate exps))))



;; cond
(define (cond? exp) 
  (eq? 'cond (car exp)))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (> 2 (length first))
            (error "ELSE clause contains fewer than two expressions!")
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF"
                           clauses))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(define (translate-cond exp) 
  (translate (cond->if exp)))



;; let
(define (make-application proc args)
  (cons proc args))

(define (make-lambda args body)
  (append (list 'lambda args)
          body))

(define (make-unnamed-let assignments body)
  (list 'let assignments body))
(define (make-named-let name assignments body)
  (list 'let name assignments body))
(define (make-let . args)
  (cond 
    [(= 2 (length args)) (apply make-unnamed-let args)]
    [(= 3 (length args)) (apply make-named-let args)]
    [else (error "make-let: I can create a let expression only with two or three arguments!")]))
(define (let? exp)
  (tagged-list? exp 'let))
(define (unnamed-let? exp)
  (and (let? exp)
       (not (named-let? exp))))
(define (named-let? exp)
  (and (let? exp)
       (symbol? (cadr exp))))
(define (let-name exp)
  (if (named-let? exp)
      (cadr exp)
      (error "let-name: I cannot identify the name of an expression other than named-let.")))
(define (let-assignments exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))
(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))
  
(define (let->combination exp)
  (cond
    [(unnamed-let? exp)
     (make-application (make-lambda (map car (let-assignments exp))
                                    (let-body exp))
                       (map cadr (let-assignments exp)))]
    #|[(named-let? exp)
     (make-let (list (list 'self
                           (make-lambda '(self)
                                        (make-lambda (map car (let-assignments exp))
                                                     (make-let (list (list (let-name exp)
                                                                           (make-application 'self '(self))))
                                                               (let-body exp))))))
               (make-application (make-application 'self '(self)) (map cadr (let-assignments exp))))]|#))

(define (translate-let exp)
  (translate (let->combination exp)))


;;let*
(define (let*? exp)
  (tagged-list? exp 'let*))

(define (make-let* var-vals body)
  (append (list 'let* var-vals)
          body))
         

(define (translate-let* exp)
  (translate (let*->combination exp)))

(define (let*->combination exp)
  (let ((vars (map car (let-assignments exp)))
        (vals (map cadr (let-assignments exp)))
        (body (let-body exp)))
    (make-application (make-lambda (list (car vars))
                                   (if (empty? (cdr vars))
                                       body
                                       (list (let*->combination (make-let* (zip (cdr vars) (cdr vals))
                                                                           body)))))
                      (list (car vals)))))
                    



;; application
(define (application? exp) 
  (pair? exp))

(define (is-js-infix operator)
  (or (eq? operator '+)
      (eq? operator '-)
      (eq? operator '*)
      (eq? operator '/)
      (eq? operator '<)
      (eq? operator '>)
      (eq? operator '<=)
      (eq? operator '>=)
      (eq? operator '=)
      (eq? operator 'or)
      (eq? operator 'and)))

(define (racket-operator->js-infix operator)
  (cond [(eq? operator '=) "==="]
        [(eq? operator 'or) "||"]
        [(eq? operator 'and) "&&"]
        [else (symbol->string operator)]))

(define (translate-application exp)
  (cond [(is-js-infix (car exp))
         (js-exp (join (string-append " " (racket-operator->js-infix (car exp)) " ")
                       (map translate (cdr exp))))]
        [else 
         (let ((receiver (car exp))
               (args (cdr exp)))
           (js-exp (translate receiver)
                   "("
                   (join ", " (map translate args))
                   ")"))]))
   

   
(define (translate exp)
  (cond ((racket-symbol? exp) (translate-racket-symbol exp))
        ((racket-number? exp) (translate-racket-number exp))
        ((racket-string? exp) (translate-racket-string exp))
        ((racket-boolean? exp) (translate-racket-boolean exp))
        ((identifier? exp) (translate-identifier exp))
        ((racket-list? exp) (translate-racket-list exp))
        ((assignment? exp) (translate-assignment exp))
        ((definition? exp) (translate-definition exp))
        ((if? exp) (translate-if exp))
        ((lambda? exp) (translate-lambda exp))
        ((begin? exp) (translate-begin exp))
        ((cond? exp) (translate-cond exp))
        ((let? exp) (translate-let exp))
        ((let*? exp) (translate-let* exp))
        ((application? exp) (translate-application exp))))



(define (racket->js exps)
  (join ";\n\n" (map translate exps)))

(define dt (compose display racket->js))


;;;Examples as tests:
#|
((lambda () (define x "doesnt matter") (if (begin (if true (set! x 0) (set! x 1)) x) 1 0)))
(cond (false 0) (else 1))
(cond (false 0) (true ((lambda () ((lambda () 3))))) (else "oh no!"))

|#




































(dt '(
      


;;;                Personal Library







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
                 
                 (define (is a)
                   (lambda (b)
                     (equal? a b)))
                 
                 (define (is-gt a)
                   (lambda (b)
                     (< a b)))
                 
                 (define (is-gte a)
                   (lambda (b)
                     (<= a b)))
                 
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
                 
                 
                 
                 ;;;parse-not-beginning-with-exp
                 
                 (define (parse-not-beginning-with-exp str indent)
                   (also (parse-number str indent)
                         (parse-string str indent)
                         (parse-boolean str indent)
                         (parse-nil str indent)
                         (parse-dot str indent)
                         (parse-unknown str indent)
                         (parse-identifier str indent)
                         (parse-matching-identifier str indent)
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
                 (define legal-variable-characters "-+=_0QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm")
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
                 
                 ;;;parse-matching-identifier
                 
                 (define (parse-matching-identifier str indent)
                   (given-seq (parse-characters "@" str no-indent)
                              (lambda (str)
                                (given (parse-identifier str no-indent)
                                       (lambda (name str)
                                         (possibility (tokenize 'Matching-identifier (string-append "@" (cadr name))) str))))))
                 
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
                                                           (given (parse-lazy-expressions str indent)
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
                                                                                                            (given (parse-lazy-expressions str (lambda (ind) (< indentation ind)))
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
                 
                 
                 ;;;parse-lazy-expressions
                 
                 (define (parse-lazy-expressions str indent)
                   (also (given (parse-exp str indent)
                                (lambda (exp str)
                                  (possibility (tokenize 'Lazy-expressions (list exp)) str)))
                         (given-seq (parse-white str no-indent)
                                    (lambda (measure-str) 
                                      (parse-characters "\n" str no-indent))
                                    (lambda (measure-str)
                                      (given (parse-all-like (is " ") measure-str no-indent)
                                             (lambda (spaces measure-str)
                                               (define indentation (string-length spaces))
                                               #|ensure indent indentation|# (if (indent indentation) '() (raise "parse-lazy-expressions: you must indent a series of statements farther than its enclosing syntactic block!"))
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
                                                                     (possibility (tokenize 'Lazy-expressions exps) str)))))))))))
                 
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
                                         (given (parse-lazy-expressions str indent)
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
                                         (given (parse-lazy-expressions str indent)
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
                                         (given (parse-lazy-expressions str indent)
                                                (lambda (right str)
                                                  (possibility (tokenize 'Reset-lazy-assignment left right) str))))))))
                 
                 ;;;parse-invocation
                 
                 (define (parse-invocation str indent)
                   (given (parse-exp-not-naked-compound str indent)
                          (lambda (receiver str)
                            (parse-invocation-with receiver str indent))))
                 (define (parse-invocation-with receiver str indent)
                   (also (given (parse-exp str indent)
                                (lambda (args str)
                                  (also-within-naked-compound (possibility (tokenize 'Invocation receiver args) str) str indent)))
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
                                         (parse-exp str indent)
                                         (lambda (f-inverse str)
                                           (possibility (tokenize 'Inverse-definition 
                                                                  f 
                                                                  f-inverse) 
                                                        str)))))))
                 
                 
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

(define env-pair mcons)


(define env-pairs mlist)


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
      [(equal? name (mcaar ps))
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
        (if (equal? name (mcaar ps))
            (mcdar ps)
            (iter (mcdr ps)))))
  (iter (mcadr env)))


(define (env-set! key value env) 
  (define (iter ps)
    (cond 
      [(empty? ps)
       (set-mcadr! env
                   (mcons (env-pair key value) 
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




;;;;    Token types
;;;;    token-<type>?

(define token-number? (partial tagged-list? 'Token-number))
(define token-string? (partial tagged-list? 'Token-string))
(define token-boolean? (partial tagged-list? 'Token-boolean))
(define token-nil? (partial tagged-list? 'Token-nil))
(define token-dot? (partial tagged-list? 'Token-dot))
(define token-unknown? (partial tagged-list? 'Token-unknown ))
(define token-identifier? (partial tagged-list? 'Token-identifier))
(define token-matching-identifier? (partial tagged-list? 'Token-matching-identifier))
(define token-funject-literal? (partial tagged-list? 'Token-funject-literal))
(define token-list-literal? (partial tagged-list? 'Token-list-literal))
(define token-lazy-expressions? (partial tagged-list? 'Token-lazy-expressions))
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
      (token-matching-identifier? exp)
      (token-funject-literal? exp)
      (token-list-literal? exp)
      (token-lazy-expressions? exp)
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
            (assert (= 4 (length args)) "I tried to create a Funject but was passed " args)
            (assert (mlist? (cadr args)) "I tried to create a Funject but was passed " args)]
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
    ;matching-identifier
    ;funject-literal
    ;list-literal
    ;lazy-expressions
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
    [(token-matching-identifier? tokens) (analyze-matching-identifier tokens)]
    [(token-funject-literal? tokens) (analyze-funject-literal tokens)]
    [(token-list-literal? tokens) (analyze-list-literal tokens)]
    [(token-lazy-expressions? tokens) (analyze-lazy-expressions tokens)]
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


(define analyze-matching-identifier analyze-identifier)


(define analyze-funject-literal ((lambda ()
                                   (define (analyze-pairs ps)
                                     (mmap (lambda (p)
                                             (set-mcadr! p 
                                                         (analyze-lazy-expressions (mcadr p)))
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
                                                        bpairs
                                                        lang-funject-god
                                                        lang-funject-inverse-god)))))
                                   analyze-funject-literal)))


(define (analyze-list-literal tokens)
  (let ((aelems (mmap analyze (mcadr tokens))))
    (lambda (env)
      (create-lang 'List (eval-each aelems env)))))


(define (analyze-lazy-expressions tokens) (mlist 'Analyzed-lazy-expressions (mmap analyze (mcadr tokens))))


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

(define (invoke receiver arg)
  (cond 
    [(primitive? receiver) (invoke-primitive receiver arg)]
    [(lang? 'Number receiver) (invoke-number receiver arg)]
    [(lang? 'Funject receiver) (invoke-funject receiver arg)]
    [else (error "invoke: I know not how to invoke " receiver "!")]))

(define (invoke-primitive receiver arg)
  (primitive-contents receiver
                      (lambda (funject _)
                        (funject arg))))

(define (invoke-number receiver arg)
  (unless (lang? 'List arg)
          (user-error-no-matching-pattern receiver arg)
          (lang-contents arg
                         (lambda (elems)
                           (unless (and (lang? 'Dot (mcar elems))
                                        (lang? 'String (mcadr elems)))
                                   (user-error-no-matching-pattern receiver arg)
                                   (lang-contents (mcadr elems)
                                                  (lambda (str)
                                                    (cond 
                                                      [(equal? str "plus") (create-primitive-number-plus (mcadr receiver))]
                                                      [(equal? str "minus") (create-primitive-number-minus (mcadr receiver))]
                                                      [(equal? str "times") (create-primitive-number-times (mcadr receiver))]
                                                      [(equal? str "div") (create-primitive-number-div (mcadr receiver))]
                                                      [else (user-error-no-matching-pattern receiver arg)]))))))))
                                          
                 

(define (invoke-funject receiver arg)
  (let ((apairs (funject-pairs-of receiver))
        (parent (funject-parent-of receiver))
        (inverse (funject-inverse-of receiver)))
    (define (iter apairs)
      (if (empty? apairs)
          (if parent
              (invoke parent arg)
              (user-error-no-matching-pattern receiver arg))
          (let* ((pair (mcar apairs))
                 (pattern (mcar pair))
                 (consequent (mcadr pair))
                 (env (mcaddr pair))
                 (bindings (choice-bindings-from-matching pattern arg env)))
            (if bindings
                (if (lang? 'Analyzed-lazy-expressions consequent)
                    (force-lazy-expressions (bind-lazy-expressions consequent 
                                                                   (env-extend (env-pairs (env-pair "own" (bind-as-though-lazy-expressions receiver)))
                                                                               bindings)))
                    consequent)
                (iter (mcdr apairs))))))
    (iter apairs)))




;(set-)funject-<prop>(-of)

(define (funject-pairs-of funject)
  (unless (lang? 'Funject funject)
          (error "I cannot find the pairs of a non-funject: " funject "!")
          (mcadr funject)))

(define (set-funject-pairs! funject pairs)
  (unless (lang? 'Funject funject)
          (error "I cannot set the pairs of a non-funject: " funject "!")
          (set-mcadr! funject pairs)))

(define (push-funject-pair! funject p)
  (unless (lang? 'Funject funject)
          (user-error-cannot-push-pair-to-non-funject)
          (set-mcadr! funject (mcons p (mcadr funject)))))

(define (funject-parent-of funject)
  (unless (lang? 'Funject funject)
          (error "I cannot find the parent of a non-funject: " funject "!")
          (mcaddr funject)))

(define (set-funject-parent! funject parent)
  (unless (lang? 'Funject funject)
          (user-error-cannot-set-parent-of-non-funject)
          (set-mcaddr! funject parent)))

(define (funject-inverse-of funject)
  (cond 
    [(lang? 'Funject funject)
     (mcadddr funject)]
    [(primitive? funject)
     (primitive-contents funject
                         (lambda (itself inverse)
                           (create-primitive inverse itself)))]
    [else
     (error "I cannot find the inverse of a non-funject or primitive: " funject "!")]))

(define (set-funject-inverse! funject inverse)
  (unless (lang? 'Funject funject)
          (user-error-set-inverse-of-non-funject)
          (set-mcadddr! funject inverse)))

(define create-funject-pair mlist)

(define create-funject-bound-pair mlist)




;<operation>(-<type>)-identifier
(define (lookup-identifier name env)
  ((env-get name env))) ;note the extra parenthases

(define (assign-strict-identifier! name right env)
  (env-set! name (bind-as-though-lazy-expressions right) env))

(define (assign-lazy-identifier! name right env)
  (env-set! name (bind-lazy-expressions right env) env))

(define (reset-strict-identifier! name right env)
  (env-reset! name (bind-as-though-lazy-expressions right) (env-parent-of env)))

(define (reset-lazy-identifier! name right env) 
  (env-reset! name (bind-lazy-expressions right env) (env-parent-of env)))

;bind-{lazy-expressions/funject-pair}
;force-lazy-expressions
(define (bind-lazy-expressions exp env)
  (let ((statements (mcadr exp)))
    (lambda () 
      (mlast (eval-each statements env)))))

(define (bind-funject-pair p env)
  (mlist-contents p
                  (lambda (pattern consequent)
                    (create-funject-bound-pair pattern consequent env))))

(define (force-lazy-expressions stmts) (stmts))

(define (bind-as-though-lazy-expressions exp) 
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
      
     
     
     
     ;;;;    Bindings-from-matching itself
     
     (define (bindings-from-matching pattern arg bindings env)
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
         [(token-matching-identifier? pattern)
          (bindings-from-matching-matching-identifier pattern arg bindings env)]
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
                (given (bindings-from-matching (mcar elems)
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
     
     
     (define (bindings-from-matching-matching-identifier pattern arg bindings env)
       (token-contents pattern
                       (lambda (name)
                         (cond 
                           [(not (env-has? name bindings)) 
                            (let ((new-bindings (env-extend (env-pairs) bindings)))
                              (assign-strict-identifier! name arg new-bindings)
                              (possibility new-bindings))]
                           [(lang-equal? arg (env-get name bindings))
                            (possibility bindings)]
                           [else (impossibility)]))))
                           
     
     (define (bindings-from-matching-identifier pattern arg bindings env)
       (if (lang-equal? (eval (analyze pattern) env) 
                        arg)
           (possibility bindings)
           (impossibility)))
     
     
     (define (bindings-from-matching-invocation pattern arg bindings env)
       (token-contents pattern
                       (lambda (receiver pattern-arg)
                         (let* ((ereceiver (eval (analyze receiver) env))
                                (unknowns-epattern-arg (eval-pattern-arg pattern-arg bindings env))
                                (unknowns (car unknowns-epattern-arg))
                                (epattern-arg (cadr unknowns-epattern-arg))
                                (inverse (invoke (funject-inverse-of ereceiver) (create-lang 'List (mlist arg epattern-arg))))
                                ;(e (error "" inverse))
                                (possibilities (lang-list->possibilities inverse))
                                (to-match (if (= 1 (mlength unknowns))
                                              (mcar unknowns)
                                              (user-error-multiple-unknowns-in-pattern-arg))))
                           (given possibilities
                                  (lambda (value)
                                    (let ((new-bindings (env-extend (env-pairs) bindings)))
                                      (assign-strict-identifier! to-match value new-bindings)
                                      new-bindings)))))))

     
     
     
     ;;;;    Eval-pattern-arg
     
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
              [(token-matching-identifier? pattern-arg) 
               (if (env-has? (mcadr pattern-arg) bindings)
                   (list empty (env-get (mcadr pattern-arg) bindings))
                   (list (mcadr pattern-arg) lang-unknown))]
              [(token-identifier? pattern-arg)
               (list empty (env-get (mcadr pattern-arg) env))]
              [(token-list-literal? pattern-arg) 
               (eval-pattern-arg-list-literal pattern-arg bindings env)]            
              [else (error "analyze: I fail to recognize the token " (deep-stream->list pattern-arg))]))
          
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
                    (list (mcons first-unknowns rest-unknowns) 
                          (mcons first-evaled rest-evaled)))))
            (let* ((unknowns--elems-evaled (iter (mcadr pattern-arg)))
                   (unknowns (car unknowns--elems-evaled))
                   (elems-evaled (cadr unknowns--elems-evaled)))
              (list unknowns (create-lang 'List elems-evaled))))
          eval-pattern-arg)))
     
     
     
     
     (define (choice-bindings-from-matching pattern arg env)
       (let ((possibilities (bindings-from-matching pattern arg (env-create (env-pairs)) env)))
         (if (impossible? possibilities)
             false
             (env-extend (env-all-pairs-of (possibility-first possibilities)) env))))
     choice-bindings-from-matching)))




;;;;    Primitives

(define (create-primitive funject inverse)
  (mlist 'Primitive funject inverse))

(define (primitive? exp)
  (and (mlist? exp)
       (not (empty? exp))
       (eq? 'Primitive (mcar exp))))

(define (primitive-contents prim f)
  (assert (primitive? prim)) ;to optimize, remove this line.
  (apply f (mlist->list (mcdr prim))))

(define (create-primitive-infix-operation type1 type2 result-type op op-inv)
  (lambda (left)
    (create-primitive (lambda (other)
                        (unless (lang? 'List other)
                          (invoke primitive-funject-god other)
                          (lang-contents other
                                         (lambda (elems)
                                           (unless (and (= 1 (mlength elems))
                                                        (lang? type2 (mcar elems)))
                                             (invoke primitive-funject-god other)
                                             (lang-contents (mcar elems)
                                                            (lambda (right) 
                                                              (create-lang result-type (op left 
                                                                                           right)))))))))
                      (lambda (result--arg)
                        (unless (lang? 'List result--arg) (invoke primitive-funject-inverse-god result--arg)
                          (lang-contents result--arg
                                         (lambda (elems)
                                           (mlist-contents elems
                                                           (lambda (result arg)
                                                             (unless (and (lang? 'List result)
                                                                          (= 1 (mlength (mcadr result)))
                                                                          (lang? result-type (mcaadr result))
                                                                          (number? (mcadaadr result))
                                                                          (lang? 'List arg)
                                                                          (= 1 (mlength (mcadr arg)))
                                                                          (lang? 'Unknown (mcaadr arg)))
                                                               (invoke primitive-funject-inverse-god result--arg)
                                                               (create-lang 'List (mlist (create-lang type2 (op-inv (mcadaadr result) left))))))))))))))

(define create-primitive-number-plus (create-primitive-infix-operation 'Number 'Number 'Number + -))

(define create-primitive-number-minus (create-primitive-infix-operation 'Number 'Number 'Number - +))

(define create-primitive-number-times (create-primitive-infix-operation 'Number 'Number 'Number * /))

(define create-primitive-number-div (create-primitive-infix-operation 'Number 'Number 'Number / *))

                    


(define primitive-funject-god
  (create-primitive (lambda (arg)
                      (user-error-no-matching-pattern "The primitive funject god" arg))
                    (lambda (arg)
                      (user-error-no-matching-pattern "The primitive funject god inversted" arg))))


(define primitive-funject-inverse-god
  (create-primitive (lambda (arg)
                      (user-error-no-matching-pattern "The primitive funject inverse god" arg))
                    (lambda (arg)
                      (user-error-no-matching-pattern "The primitive funject inverse god himself inversted" arg))))


(define lang-funject-god (create-primitive (lambda (arg)
                                             (error "The funject god is called upon to serve " arg ", but he serves only the enlightened."))
                                           (lambda (arg)
                                             (error "The funject god was called upon in esrver to serve " arg ", yet he has no inverse!"))))

(define lang-funject-inverse-god (create-primitive (lambda (arg)
                                                     (error ("The funject inverse god was called upon to serve " arg ", but the inverse god serves no one!")))
                                                   (lambda (arg)
                                                     (error ("The funject was himself called in reverse to serve " arg ". He is amused.")))))
                                                     


(define lang-nil (create-lang 'Nil))
(define lang-dot (create-lang 'Dot))
(define lang-unknown (create-lang 'Unknown))
(define lang-equal? equal?)

(define global-env (env-create (env-pairs (env-pair "Yin" lang-funject-god)
                                          (env-pair "Yang" lang-funject-inverse-god))))




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


))
