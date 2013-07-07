#lang racket
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

(define (translate-identifier exp)
  (remove-if-final-? (string-replace (string-replace (->string exp) "->" "_to_") "-" "_")))



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
                                 " = Array.prototype.slice.call(arguments, " 
                                 (number->string (length named-args))
                                 "); \n    ")))
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
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (translate-cond exp) 
  (translate (cond->if exp)))



;; let
(define (make-application proc args)
  (cons proc args))

(define (make-lambda args body)
  (list 'lambda args body))

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
       (= (length exp) 3)))
(define (named-let? exp)
  (and (let? exp)
       (= (length exp) 4)))
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
      (cadddr exp)
      (caddr exp)))
  
(define (let->combination exp)
  (cond
    [(unnamed-let? exp)
     (make-application (make-lambda (map car (let-assignments exp))
                                    (let-body exp))
                       (map cadr (let-assignments exp)))]
    [(named-let? exp)
     (make-let (list (list 'self
                           (make-lambda '(self)
                                        (make-lambda (map car (let-assignments exp))
                                                     (make-let (list (list (let-name exp)
                                                                           (make-application 'self '(self))))
                                                               (let-body exp))))))
               (make-application (make-application 'self '(self)) (map cadr (let-assignments exp))))]))

(define (translate-let exp)
  (translate (let->combination exp)))



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




































(dt '((define (is-in-string str)
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
(define tokenize list)



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


(define (parse-exp-not-naked-compound str indent)
  (also (given-seq (parse-characters "(" str no-indent)
                   (lambda (str) 
                     (given (parse-exp str indent)
                            (lambda (exp str)
                              (given-seq (parse-characters ")" str no-indent)
                                         (lambda (str)
                                           (possibility exp str)))))))
        (parse-not-beginning-with-exp str indent)))

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
(define legal-variable-characters "-+=0QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm")
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
  (also (parse-exp str indent)
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
           (also (given (parse-exp str indent)
                        (lambda (args str)
                          (possibility (tokenize 'Application receiver args) str)))
                 (given (parse-characters "." str no-indent)
                        (lambda (_ str)
                          (given (parse-identifier str no-indent)
                                 (lambda (property str)
                                   (possibility (tokenize 'Application 
                                                          receiver 
                                                          (tokenize 'List-literal
                                                                    (list (tokenize 'Dot)
                                                                          (tokenize 'String
                                                                                    property))))
                                                str)))))))))

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



;;;parse-list-literal

 




;;;                Analysis


;;;analyze-self-evaluating

;;;analyze-list-literal

;;;analyze-identifier

;;;analyze-strict-assignment

;;analyze-lazy-assignment

;;;analyze-reset-assignment

;;;analyze-funject-inheritance



;;;create-funject

;;;create-number

;;;create-boolean

;;;create-string

;;;create-nil

;;;create-assignment

;;;create-lazy-assignment

;;;create-reset-assignment


#|(display (dsl (parse-exps "

ajax{
    'url': 'http://www.google.com'
    'expected-encoding': 'utf-8'
    'success': { [data]: print['hooray! data'] }
    'failure': { [data]: print['oh no! it failed.'] }
}

" base-indent)))|#))