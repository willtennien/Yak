#lang racket
;;;;begin not translating
(require compatibility/mlist)

(define-syntax try
  (syntax-rules (catch finally)
    ((_ try-body ... (catch catch-proc))
     (with-handlers (((lambda (ex) #t)
                      (lambda (ex) 
                        (catch-proc ex))))
       (begin
         try-body ...)))
    ((_ try-body ... (catch catch-proc) (finally fin-body ...))
     (dynamic-wind
      (lambda () ())
      
      (lambda ()
        (with-handlers (((lambda (ex) #t)
                         (lambda (ex) 
                           (catch-proc ex))))
          (begin
            try-body ...)))
      
      (lambda () fin-body ...)))
    ((_ try-body ... (finally fin-body ...))
     (dynamic-wind
      (lambda () ())
      
      (lambda () try-body ...)

      (lambda () fin-body ...)))))
 
;;;;end not translating

;;;                Personal Library
(define (->string exp)
  (let ((o (open-output-string)))
    (write exp o)
    (get-output-string o)))

(define ->boolean (compose not not))

(define-syntax-rule (on args ... f) (f args ...))

(define (hash-if-match h key conseq altern)
  (define hash-has-key? true)
  (let ((result (hash-ref h 
                          key 
                          (lambda () 
                            (set! hash-has-key? false)))))
    (if hash-has-key?
        (conseq key result)
        (altern key))))

(define (hash-if-val-match h val conseq altern)
  (define found? false)
  (define answer (void))
  (hash-for-each h
                 (lambda (k v)
                   (cond
                     [(equal? v val)
                      (set! found? true)
                      (set! answer k)]
                     [else 'ok])))
  (if found?
      (conseq answer val)
      (altern val)))

(define (is a)
  (lambda (b)
    (equal? a b)))

(define (is-gt a)
  (lambda (b)
    (< a b)))

(define (is-gte a)
  (lambda (b)
    (<= a b)))

(define (partial f . args)
  (define (result . more-args)
    (apply f (append args more-args)))
  result)

(define (apply-n n f arg)
  (if (= 0 n)
      arg
      (apply-n (sub1 n) f (f arg))))

(define (zip xs ys)
  (if (or (empty? xs)
          (empty? ys))
      empty
      (cons (cons (car xs)
                  (car ys))
            (zip (cdr xs)
                 (cdr ys)))))
          

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
(define mcadadr (compose mcar mcdr mcar mcdr))
(define mcadaddr (compose mcar mcdr mcar mcdr mcdr))
(define mcadadddr (compose mcar mcdr mcar mcdr mcdr mcdr))
(define mcadaddddr (compose mcar mcdr mcar mcdr mcdr mcdr mcdr))
(define mcadadddddr (compose mcar mcdr mcar mcdr mcdr mcdr mcdr mcdr))
(define mcadaddddddr (compose mcar mcdr mcar mcdr mcdr mcdr mcdr mcdr mcdr))
(define mcadadddddddr (compose mcar mcdr mcar mcdr mcdr mcdr mcdr mcdr mcdr mcdr))
(define mcadaddddddddr (compose mcar mcdr mcar mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcdr))
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
(define mcdadr (compose mcdr mcar mcdr))
(define mcddadr (compose mcdr mcdr mcar mcdr))
(define mcdddadr (compose mcdr mcdr mcdr mcar mcdr))
(define mcddddadr (compose mcdr mcdr mcdr mcdr mcar mcdr))
(define mcdddddadr (compose mcdr mcdr mcdr mcdr mcdr mcar mcdr))
(define mcddddddadr (compose mcdr mcdr mcdr mcdr mcdr mcdr mcar mcdr))
(define mcdddddddadr (compose mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcar mcdr))
(define mcddddddddadr (compose mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcar mcdr))
(define mcdaddr (compose mcdr mcdr mcar mcdr mcdr))
(define mcdadddr (compose mcdr mcdr mcdr mcar mcdr mcdr))
(define mcdaddddr (compose mcdr mcdr mcdr mcdr mcar mcdr mcdr))
(define mcdadddddr (compose mcdr mcdr mcdr mcdr mcdr mcar mcdr mcdr))
(define mcdaddddddr (compose mcdr mcdr mcdr mcdr mcdr mcdr mcar mcdr mcdr))
(define mcdadddddddr (compose mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcar mcdr mcdr))
(define mcdaddddddddr (compose mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcdr mcar mcdr mcdr))
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
(define (set-mcadadr! xs x) (set-mcar! (mcdadr xs) x))
(define (set-mcadaddr! xs x) (set-mcar! (mcdaddr xs) x))
(define (set-mcadadddr! xs x) (set-mcar! (mcdadddr xs) x))
(define (set-mcadaddddr! xs x) (set-mcar! (mcdaddddr xs) x))
(define (set-mcadadddddr! xs x) (set-mcar! (mcdadddddr xs) x))
(define (set-mcadaddddddr! xs x) (set-mcar! (mcdaddddddr xs) x))
(define (set-mcadadddddddr! xs x) (set-mcar! (mcdadddddddr xs) x))
(define (set-mcadaddddddddr! xs x) (set-mcar! (mcdaddddddddr xs) x))
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

(define (mmap-indexes proc xs)
  (mmap proc
        xs
        (mup-to 0 (mlength xs))))

(define (mup-to start end)
  (if (= end start)
      empty
      (mcons start
             (mup-to (+ 1 start) end))))

(define (mlist-begins-with? xs start)
  (cond 
    [(empty? start)
     true]
    [(empty? xs)
     false]
    [else
     (and (equal? (mcar xs)
                  (mcar start))
          (mlist-begins-with? (mcdr xs)
                              (mcdr start)))]))

(define (msplit-at xs n)
  (let-values ([(before after) (split-at (mlist->list xs) n)])
    (values (list->mlist before)
            (list->mlist after))))
          
(define (repeat n elem)
  (build-list n (lambda (_) elem)))

(define mrepeat (compose list->mlist repeat))

(define (mtake xs n)
  (list->mlist (take (mlist->list xs) n)))

(define (mdrop xs n)
  (list->mlist (drop (mlist->list xs) n)))
          

;;;;begin not translating
(define (just a)
  (list 'Maybe (list 'Just a)))

(define nothing
  (list 'Maybe 'Nothing))

(define (maybe? a)
  (equal? 'Maybe (car a)))

(define (nothing? a)
  (assert (maybe? a) "nothing?: you passed me something other than a maybe!")
  (equal? 'Nothing (cadr a)))

(define something? (compose not nothing?))

(define (just-thing a)
  (assert (something? a) "just-thing: you failed to pass me a just something!")
  (cadadr a))

(define (matches a b)
  (or (and (equal? a "")
           (equal? b ""))
      (ormap (lambda (result)
               (not (equal? result b)))
             (string-split b a))))

(define (string-begins-with? str start)
  (and (<= (string-length start) (string-length str))
       (equal? start (substring str 0 (string-length start)))))
;;;;end not translating




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                Syntax Parser

(define odsl 'doesnt-matter)
(define parse ((lambda ()   
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
(dsl (parse-symbol ".foo " no-indent))
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
                   (cons (string->symbol (string-append "Token-"
                                                        (string-downcase (symbol->string (car args)))))
                         (cdr args)))
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
                         (parse-unknown str indent)
                         (parse-identifier str indent)
                         (parse-parameter str indent)
                         (parse-symbol str indent)
                         (parse-list-literal str indent)
                         (parse-funject-literal str indent)
                         (parse-strict-assignment str indent)
                         (parse-lazy-assignment str indent)
                         (parse-reset-strict-assignment str indent)
                         (parse-reset-lazy-assignment str indent)
                         (parse-module str indent)
                         (parse-class str indent)
                         (parse-conditional str indent)))
                 
                 
                 ;;;parse-beginning-with-exp
                 
                 (define (parse-beginning-with-exp str indent) 
                   (also (parse-funject-strict-assignment str indent)
                         (parse-funject-lazy-assignment str indent)
                         (parse-funject-inheritance str indent)
                         (parse-inverse-definition str indent)
                         (parse-invocation str indent)))
                 
                 
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
                 
                 ;;;parse-unknown 
                 
                 (define (parse-unknown str indent)
                   (given (parse-characters "unknown" str no-indent)
                          (lambda (_ str)
                            (possibility (tokenize 'Unknown) str))))
                 
                 ;;;parse-identifier
                 
                 ;You may need these depending on how you define legal identifiers: "1" "2" "3" "4" "5" "6" "7" "8" "9"
                 (define numbers "1234567890")
                 (define legal-variable-characters "-+*/%_$<>?!=0QWERTYUIOPASDFGHJKLZXCVBNMqwertyuiopasdfghjklzxcvbnm")
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
                                (given (also (parse-identifier str no-indent)
                                             (possibility (tokenize 'Identifier "") str))
                                       (lambda (name str)
                                         (possibility (tokenize 'Parameter (string-append "@" (cadr name))) str))))))
                 
                 ;;;parse-symbol
                 
                 (define (parse-symbol str indent) 
                   (given (parse-characters "." str no-indent)
                          (lambda (_ str)
                            (given (parse-identifier str no-indent)
                                   (lambda (ident str)
                                     (possibility (tokenize 'Symbol (cadr ident)) str))))))
                 
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
                                                    (given (parse-separated (lambda (str _)
                                                                              (given-seq (parse-white str no-indent)
                                                                                         (lambda (str)
                                                                                           (parse-characters "\n" str no-indent))))
                                                                            (lambda (str _)
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
                                                                                                                          (possibility (list key value) str))))))))))
                                                                            str
                                                                            indent)
                                                           (lambda (pairs str)
                                                             (given-seq (given-seq (parse-white str no-indent)
                                                                                   (lambda (str)
                                                                                     (also (possibility 'doesnt-matter str)
                                                                                           (parse-characters "\n" str no-indent))))
                                                                        (lambda (str)
                                                                          (parse-each parse-white-line str indent))
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
                   (given (parse-white str no-indent)
                                (lambda (_ str)
                                  (given (parse-exp str indent)
                                         (lambda (args str)
                                           (also-within-naked-compound (possibility (tokenize 'Invocation receiver args) str) str indent))))))
                 
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
                 
                 ;;;parse-module and parse-class
                 
                 (define parse-module--parse-class
                   ((lambda ()
                      (define (parse-module-or-class-after-declaration type maybe-parent str indent)
                        (given (parse-sequence str indent)
                               (lambda (seq str)
                                 (possibility (if (something? maybe-parent)
                                                  (tokenize type (just-thing maybe-parent) seq)
                                                  (tokenize type seq))
                                              str))))
                      
                      (define (parse-module-or-class-after-type type str indent)
                        (given-seq (parse-white str no-indent)
                                   (lambda (str)
                                     (also (given (parse-identifier str no-indent)
                                                  (lambda (name str)
                                                    (if (equal? "<" (cadr name))
                                                        (impossibility)
                                                        (given (parse-module-or-class-after-name type str indent)
                                                               (lambda (module-or-class str)
                                                                 (possibility (tokenize 'Strict-assignment
                                                                                        name
                                                                                        module-or-class)
                                                                              str))))))
                                           (parse-module-or-class-after-name type str indent)))))
                      
                      (define (parse-module-or-class-after-name type str indent)
                        (also (given-seq (parse-white str no-indent)
                                         (lambda (str)
                                           (parse-characters "<" str no-indent))
                                         (lambda (str)
                                           (parse-white str no-indent))
                                         (lambda (str)
                                           (given (parse-exp str indent)
                                                  (lambda (parent str)
                                                    (given-seq (parse-white str indent)
                                                               (lambda (str)
                                                                 (parse-module-or-class-after-declaration type (just parent) str indent)))))))
                              (given-seq (parse-white str indent)
                                         (lambda (str)
                                           (parse-module-or-class-after-declaration type nothing str indent)))))                                                                                 
                     
                      (define (parse-module str indent)
                        (given (parse-characters "module" str indent)
                               (lambda (_ str)
                                 (parse-module-or-class-after-type 'Module str indent))))
                      
                      (define (parse-class str indent)
                        (given (parse-characters "class" str indent)
                               (lambda (_ str)
                                 (parse-module-or-class-after-type 'Class str indent))))
                      
                      (mlist parse-module parse-class))))
                        
                 (define parse-module (mcar parse-module--parse-class))
                 
                 (define parse-class (mcadr parse-module--parse-class))
                 
                 (define (parse-conditional str indent)
                   (given-seq (parse-characters "if" str no-indent)
                              (lambda (str)
                                (parse-white str no-indent))
                              (lambda (str)
                                (given (parse-exp str indent)
                                       (lambda (condition str)
                                         (given-seq (parse-white str no-indent)
                                                    (lambda (str)
                                                      (given (also (given-seq (parse-characters "\n" str no-indent)
                                                                              (lambda (_) (possibility true str)))
                                                                   (given-seq (parse-characters "then" str no-indent)
                                                                              (lambda (str)
                                                                                (parse-white str no-indent))
                                                                              (lambda (str)
                                                                                (possibility false str))))
                                                             (lambda (multiline str)
                                                               (given (parse-sequence str indent)
                                                                      (lambda (consequent str)
                                                                        (also (given-seq (if multiline
                                                                                             (given-seq (parse-characters "\n" str no-indent)
                                                                                                        (lambda (str)
                                                                                                          (given (parse-white str no-indent)
                                                                                                                 (lambda (white str)
                                                                                                                   (unless (indent (string-length white))
                                                                                                                     (impossibility)
                                                                                                                     (possibility 'doesnt-matter str))))))
                                                                                             (parse-white str no-indent))
                                                                                         (lambda (str)
                                                                                           (given-seq (parse-characters "else" str no-indent)
                                                                                                      (lambda (str)
                                                                                                        (parse-white str no-indent))
                                                                                                      (lambda (str)
                                                                                                        (given (parse-sequence str indent)
                                                                                                               (lambda (alternative str)
                                                                                                                 (possibility (tokenize 'Conditional
                                                                                                                                        condition
                                                                                                                                        consequent
                                                                                                                                        alternative)
                                                                                                                              str)))))))
                                                                              (possibility (tokenize 'Conditional 
                                                                                                     condition 
                                                                                                     consequent 
                                                                                                     (tokenize 'Nil))
                                                                                           str)))))))))))))

                                                                                       
                                                                                       
                 ;;;;begin not translating
                 (set! odsl dsl)
                 ;;;;end not translating
                 
                 (define (parse str)
                   (parse-exps str base-indent))
                 parse)))
































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                Evaluator


;;;;begin not translating
(define (mfoldl f init xs) 
  (foldl f init (mlist->list xs)))

(define (mormap f xs)
  (ormap f (mlist->list xs)))

(define (mandmap f xs)
  (andmap f (mlist->list xs)))

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

(define (contents xs f)
  (apply f xs))

(define (mlist-contents xs f)
  (apply f (mlist->list xs)))

(define (tagged-list? sym exp)
  (equal? sym (mcar exp)))




;;;;    Environment
;;;;    env-<specifier>
(struct env (pairs-of parent-of) #:transparent #:constructor-name env-extend #:mutable)

(define create-env-pair mlist)

(define env-pair-count mlength)

(define env-pair-key-of mcar)

(define env-pair-value-of mcadr)

(define env-pairs mlist)

(define env-pairs-append mappend)

(define env-pairs-cons mcons)

(define env-pairs-cdr mcdr)

(define (create-env-pair-strict name val)
  (create-env-pair name (bind-as-though-unevaled val)))

(define empty-env 'empty-env)


(define env-empty? (partial eq? 'empty-env))


(define (env-create pairs)
  (env-extend pairs
              empty-env))

(define (env-extend-one key-value env)
  (env-create (env-pairs key-value)
              env))

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
      (iter (env-pairs-of env))))


(define (env-get name env)
  (define (iter ps)
    (if (empty? ps)
        (if (env-empty? (env-parent-of env))
            (user-error-cannot-find-variable name)
            (env-get name (env-parent-of env)))
        (if (equal? name (env-pair-key-of (mcar ps)))
            (env-pair-value-of (mcar ps))
            (iter (mcdr ps)))))
  (iter (env-pairs-of env)))


(define (env-set! key value env) 
  (define (iter ps)
    (cond 
      [(empty? ps)
       (set-env-pairs-of! env
                          (mcons (create-env-pair key value) 
                                 (env-pairs-of env)))
        env]
      [(equal? key (mcaar ps))
       (set-mcadar! ps value)
       env]
      [else (iter (mcdr ps))]))
  (iter (env-pairs-of env)))


(define (env-reset! key value env)
  (define (iter ps)
    (cond
      [(empty? ps)
       (if (env-empty? (env-parent-of env))
           (user-error-cannot-reset-unset-variable key)
           (env-reset! key value (env-parent-of env)))]
      [(equal? key (mcaar ps))
       (set-env-pairs-of! env (env-pairs-cons (create-env-pair key value)
                                              (env-pairs-cdr ps)))]
      [else (iter (mcdr ps))]))
  (iter (env-pairs-of env)))


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
(define token-symbol? (partial tagged-list? 'Token-symbol))
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
(define token-module? (partial tagged-list? 'Token-module))
(define token-class? (partial tagged-list? 'Token-class))
(define token-conditional? (partial tagged-list? 'Token-conditional))

(define (token-any? exp) ;to optimize, define this simply as (equal? "Token" (substring (symbol->string (cadr exp)) 0 5))
  (or (token-number? exp)
      (token-string? exp)
      (token-boolean? exp)
      (token-nil? exp)
      (token-symbol? exp)
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
      (token-inverse-definition? exp)
      (token-module? exp)
      (token-class? exp)
      (token-conditional? exp)))

(define (tokenize . args)
  (apply mlist (cons (string->symbol (string-append "Token-"
                                                    (string-downcase (symbol->string (car args)))))
                     (cdr args))))
(define (token-contents tokens f)
  (assert (token-any? tokens) "token-contents: I can only take a token, but you passed me:" tokens) ;to optimize, remove this line.
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
           [(eq? 'Symbol (car args))
            (assert (= 2 (length args)) "I tried to create a Symbol but was passed " args)
            (assert (string? (cadr args)) "I tried to create a Symbol but was passed " args)]
           [(eq? 'Future (car args))
            (assert (= 2 (length args)) "I tried to create a Future but was passed " args)
            (assert (future? (cadr args)) "I tried to create a Future but was passed " args)]
           [(or (eq? 'Nil (car args))
                (eq? 'Unknown (car args)))
            (assert (= 1 (length args)) "I tried to create a Nil, or Unknown but was passed " args)]
           [(eq? 'List (car args))
            (assert (= 2 (length args)) "I tried to create a List but was passed " args)
            (assert (mlist? (cadr args)) "I tried to create a List but was passed " args)]
           [(eq? 'Funject (car args))
            (assert (= 5 (length args)) "I tried to create a Funject but was passed " args)
            (assert (mlist? (caddr args)) "I tried to create a Funject but was passed " args)]
           [(eq? 'Class (car args))
            (assert (= 3 (length args)))]
           [else (display-all "Warning: I fail to recognize the type of " args "!\n")])
         (apply mlist args)))

(define (create-lang-list-mlist . elems)
  (create-lang 'List (list->mlist elems)))

(define (lang? sym exp)
  (and (mlist? exp)
       (equal? sym (mcar exp))))

(define (lang-any? exp)
  (and (mlist? exp)
       (not (empty? exp))
       (or (eq? (mcar exp) 'Class)
           (eq? (mcar exp) 'Number)
           (eq? (mcar exp) 'String)
           (eq? (mcar exp) 'Boolean)
           (eq? (mcar exp) 'Nil)
           (eq? (mcar exp) 'Symbol)
           (eq? (mcar exp) 'Unknown)
           (eq? (mcar exp) 'List)
           (eq? (mcar exp) 'Funject)
           (eq? (mcar exp) 'Future))))  


(define (lang-contents exp f)
  (assert (lang-any? exp) "I tried to take the lang-contents of a non-lang: " exp) ;to optimize, remove this line.
  (apply f (mlist->list (mcdr exp))))

(define (lang-content-single lang)
  (assert (= (mlength lang)))
  (mcadr lang))

(define (lang-list-n-contents lang-list 
                              n 
                              on-failure
                              on-success)
  (lang-list-contents lang-list
                      on-failure
                      (lambda elems
                        (unless (= n (length elems))
                                (on-failure)
                                (apply on-success elems)))))

(define (each-lang-contents-typed-helper vals types on-failure on-success . collected)
  (let ((collected (if (empty? collected)
                       empty
                       (car collected))))
        (if (or (empty? vals)
                (empty? types))
            (apply on-success collected)
            (lang-contents-typed (car vals)
                                 (car types)
                                 on-failure
                                 (lambda internals
                                   (each-lang-contents-typed-helper (cdr vals) (cdr types) on-failure on-success (append collected internals)))))))
  
(define-syntax-rule (each-lang-contents-typed (vals ...) (types ...) on-failure on-success)
  (each-lang-contents-typed-helper (list vals ...) (list types ...) on-failure on-success))


   
;lang-list-contents
(define (lang-list-contents result--arg 
                            handle-failure
                            handle-success)
  (lang-contents-typed result--arg
                       'List
                       handle-failure
                       (lambda (elems)
                         (apply handle-success (mlist->list elems)))))

;inverse-arg-contents-typed
(define (inverse-arg-contents-typed result--arg
                                    result-type 
                                    arg-type 
                                    callback)
  (lang-contents-typed result--arg
                       'List
                       (partial
                       (lambda (elems)
                         (assert (= 2 (mlength elems)))
                         (mlist-contents elems 
                                         (lambda (result arg)
                                           (lang-contents-typed result
                                                                result-type
                                                                (partial default-primitive-parent arg)
                                                                (lambda (result)
                                                                  (lang-contents-typed arg
                                                                                       arg-type
                                                                                       (partial default-primitive-parent arg)
                                                                                       (lambda (arg)
                                                                                         (callback result arg)))))))))))
                                                                                         
                                                                                       
                                         

(define (equal-lang? type yak racket)
  (and (lang? type yak)
       (lang-contents yak
                      (lambda (internal)
                        (equal? internal racket)))))

;lang-contents-typed
(define (lang-contents-typed yak
                             type
                             handle-failure
                             handle-success)
  (user-assert (lang? type yak) handle-failure)
  (lang-contents yak
                 (lambda contents
                   (apply handle-success contents))))




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
    ;symbol
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
    [(token-symbol? tokens) (analyze-symbol tokens)]
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
    [(token-module? tokens) (analyze-module tokens)]
    [(token-class? tokens) (analyze-class tokens)]
    [(token-conditional? tokens) (analyze-conditional tokens)]
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
  

(define (analyze-symbol tokens)
    (lambda (env) (create-lang 'Symbol (mcadr tokens))))


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
                                                         (mlist 'Unevaled (analyze-sequence (mcadr p))))
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
                                                        (gen-funject-id)
                                                        bpairs
                                                        primitive-funject-god
                                                        primitive-funject-inverse-god)))))
                                   analyze-funject-literal)))


(define (analyze-list-literal tokens)
  (let ((aelems (mmap analyze (mcadr tokens))))
    (lambda (env)
      (create-lang 'List (eval-each aelems env)))))


(define (analyze-sequence-without-own-scope tokens)
  (assert (< 0 (mlength (mcadr tokens))) "analyze-sequence: A sequence must have at least one expression, but this one has none!") ;to optimize, remove this assertion
  (let ((aexps (mmap analyze (mcadr tokens))))
    (lambda (env)
      (mlast (mmap (lambda (aexp)
                     (eval aexp env))
                   aexps)))))

(define (analyze-sequence tokens)
  (let ((analyzed (analyze-sequence-without-own-scope tokens)))
    (lambda (env)
      (eval analyzed (env-extend (env-pairs) env)))))


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
                                                                        (push-funject-pair! ereceiver (bind-funject-pair (create-funject-pair pattern (mlist 'Evaled eright)) env))
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
                                                                        (push-funject-pair! ereceiver (bind-funject-pair (create-funject-pair pattern (mlist 'Unevaled aright)) env))
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
                            (user-error-cannot-find-variable left-name)))))))
                        

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
                            (user-error-cannot-find-variable left-name)))))))


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


(define analyze-module--analyze-class 
  ((lambda ()
     (define (general-with-new-parent! thing parent)
       (cond 
               [(lang? 'Funject thing)
                (set-funject-parent! thing parent)
                thing]
               [(lang? 'List thing)
                (create-lang 'Funject 
                             (gen-funject-id)
                             (mmap-indexes (lambda (elem i)
                                             (bind-funject-pair (create-funject-pair (create-lang 'Number i) (mlist 'Evaled elem)) (env-create (env-pairs))))
                                           (mcadr thing))
                             parent
                             primitive-funject-inverse-god)]
               [else
                thing]))
     
     (define (after-analyze-module aparent aseq)
       (lambda (env)
         (let* ((parent (eval aparent env))
                (module-env (env-extend (env-pairs)
                                        env)))
           (assign-strict-identifier! "exports" 
                                      (create-lang 'Funject 
                                                   (gen-funject-id)
                                                   '() 
                                                   parent
                                                   primitive-funject-inverse-god)
                                      module-env)
           (eval aseq module-env)
           (let ((exports (lookup-identifier "exports" module-env)))
             (general-with-new-parent! exports parent)))))
     
     (define (analyze-header callback)
       (lambda (tokens)
         (cond 
           [(= 2 (mlength tokens))
            (callback (lambda (env) 
                        primitive-funject-class)
                      (if (token-sequence? (mcadr tokens))
                          (analyze-sequence-without-own-scope (mcadr tokens))
                          (analyze (mcadr tokens))))]
           [(= 3 (mlength tokens))
            (callback (analyze (mcadr tokens))
                      (if (token-sequence? (mcaddr tokens))
                          (analyze-sequence-without-own-scope (mcaddr tokens))
                          (analyze (mcaddr tokens))))])))

     (define (wrap-instance-conseqs-in-private! instance)
       (set-funject-pairs! instance
                           (mmap (lambda (p)
                                   (let ((pattern (funject-pair-pattern-of p))
                                         (original (funject-pair-conseq-of p))
                                         (original-env (funject-pair-env-of p)))
                                     (create-funject-bound-pair pattern
                                                                (mlist 'Unevaled 
                                                                       (lambda (env)
                                                                         (let* ((meth (if (eq? 'Unevaled (mcar original))
                                                                                          ((mcadr original) env)
                                                                                          (mcadr original))))
                                                                           (set-funject-pairs! meth
                                                                                               (mmap (lambda (p)
                                                                                                       (let ((pattern (funject-pair-pattern-of p))
                                                                                                             (original-unevaled (mcadr (funject-pair-conseq-of p)))
                                                                                                             (original-env (funject-pair-env-of p)))
                                                                                                         (create-funject-bound-pair pattern
                                                                                                                                    (mlist 'Unevaled 
                                                                                                                                           (lambda (env)
                                                                                                                                             (let ((self (lookup-identifier "@self" env)))
                                                                                                                                               (original-unevaled (env-extend (env-pairs-for-private-of self) env)))))
                                                                                                                                    original-env)))
                                                                                                     (funject-pairs-of meth)))
                                                                           meth)))
                                                                original-env)))
                                 (funject-pairs-of instance))))
     
     (define (after-analyze-class aparent aseq)
       (lambda (env)
         (let* ((parent (eval aparent env))
                (parent-instance (if (has?-match parent (create-lang 'Symbol "instance"))
                                                       (invoke parent (create-lang 'Symbol "instance"))
                                                       primitive-funject-class-instance))
                (module-env (env-extend (env-pairs)
                                        env)))
           (assign-strict-identifier! "exports" 
                                      (create-lang 'Funject 
                                                   (gen-funject-id)
                                                   '() 
                                                   parent
                                                   primitive-funject-inverse-god)
                                      module-env)
           (assign-strict-identifier! "instance" 
                                      (create-lang 'Funject 
                                                   (gen-funject-id)
                                                   '() 
                                                   parent-instance
                                                   primitive-funject-inverse-god)
                                      module-env)
           (eval aseq module-env)
           (let ((exports (general-with-new-parent! (lookup-identifier "exports" module-env) parent))
                 (instance (general-with-new-parent! (lookup-identifier "instance" module-env) parent-instance)))
             (if (has?-match instance (create-lang 'Symbol "initialize"))
                 'ok
                 (push-funject-pair! instance (bind-funject-pair (create-funject-pair (create-lang 'Symbol "initialize")
                                                                                      (mlist 'Evaled 
                                                                                             default-class-initialize))
                                                                 module-env)))
             (wrap-instance-conseqs-in-private! instance)
             (push-funject-pair! exports 
                                 (bind-funject-pair (create-funject-pair (create-lang 'Symbol 
                                                                                      "instance")
                                                                         (mlist 'Evaled 
                                                                                instance))
                                                    module-env))
             (push-funject-pair! exports (bind-funject-pair (create-funject-pair (create-lang 'Symbol "new")
                                                                                 (mlist 'Evaled (create-primitive-class-new module-env)))
                                                            module-env))
             (create-lang 'Class exports parent)))))
             
         
     (mlist (analyze-header after-analyze-module)
            (analyze-header after-analyze-class)))))

(define analyze-module (mcar analyze-module--analyze-class))
(define analyze-class (mcadr analyze-module--analyze-class))

(define (analyze-conditional tokens)
  (token-contents tokens
                  (lambda (condition consequent alternative)
                    (let ((acond (analyze condition))
                          (aconseq (analyze consequent))
                          (aaltern (analyze alternative)))
                      (lambda (env)
                        (let ((econd (eval acond env)))
                          (cond 
                            [(equal? econd (create-lang 'Boolean true))
                             (eval aconseq env)]
                            [(equal? econd (create-lang 'Boolean false))
                             (eval aaltern env)]
                            [else
                             (user-error-condition-not-boolean)])))))))
     

;;;;    invoke

(define (invoke receiver arg . maybe-own-and-extra-bindings)
  (define own (if (empty? maybe-own-and-extra-bindings)
                  receiver
                  (car maybe-own-and-extra-bindings)))
  (define extra-bindings (if (= 2 (length maybe-own-and-extra-bindings))
                             (cadr maybe-own-and-extra-bindings)
                             (env-pairs)))
  (cond 
    [(primitive? receiver) 
     (invoke-primitive receiver arg own)]
    [(lang? 'Number receiver) 
     (invoke-number receiver arg own)]
    [(lang? 'String receiver)
     (invoke-string receiver arg own)]
    [(lang? 'Boolean receiver)
     (invoke-boolean receiver arg own)]
    [(lang? 'Symbol receiver)
     (invoke-symbol receiver arg own)]
    [(lang? 'Nil receiver)
     (invoke-nil receiver arg own)]
    [(lang? 'Unknown receiver)
     (invoke-unknown receiver arg own)]
    [(lang? 'List receiver)
     (invoke-list receiver arg own)]
    [(lang? 'Future receiver)
     (invoke-future receiver arg own)]
    [(lang? 'Class receiver)
     (invoke-class receiver arg own)] 
    [(and (lang? 'Funject receiver)
          (lang? 'Symbol arg) 
          (equal? "has?" (mcadr arg))) 
     (create-primitive-has?-match own)]
    [(lang? 'Funject receiver)
     (invoke-funject receiver arg own extra-bindings)]
    [else 
     (error "invoke: I know not how to invoke " receiver "!")]))

(define (invoke-funject-itself-with-bindings receiver arg extra-bindings)
  (invoke-funject receiver arg receiver extra-bindings))
(define (invoke-funject receiver arg own . maybe-extra-bindings)
  (if (void? receiver) (error "I cannot invoke a void receiver!") 'ok)
  (let ((extra-bindings (env-pairs-cons (create-env-pair "own" 
                                                         (bind-as-though-unevaled own))
                                        (if (empty? maybe-extra-bindings)
                                            (env-pairs)
                                            (car maybe-extra-bindings))))
        (apairs (funject-pairs-of receiver))
        (parent (funject-parent-of receiver))
        (inverse (funject-inverse-of receiver)))
    (define (iter apairs)
      (if (empty? apairs)
          (if parent
              (invoke parent arg receiver)
              (error "Every lang Funject should have a parent, but the following one didn't!" receiver))
          (let* ((pair (mcar apairs))
                 (pattern (mcar pair))
                 (consequent (mcadr pair))
                 (env (mcaddr pair))
                 (bindings (choice-bindings-from-matching pattern arg (env-extend extra-bindings env))))
            (if bindings
                (cond
                  [(eq? 'Unevaled (mcar consequent))
                   (force-bound (bind-analyzed (mcadr consequent) (env-extend extra-bindings bindings)))]
                  [(eq? 'Evaled (mcar consequent))
                   (mcadr consequent)]
                  [else
                   (error "In the consequent of a pattern, I find neither 'Evaled nor 'Unevaled, but " consequent)])
                (iter (mcdr apairs))))))
    (iter apairs)))

(define (invoke-class klass arg own)
  (lang-contents klass
                 (lambda (exports super)
                   (cond
                     [(has?-match exports arg own)
                      (invoke exports arg klass)]
                     [(has?-match super arg own)
                      (invoke super arg own)]
                     [else
                      (invoke primitive-class-class-instance arg own)]))))

(define (invoke-inverse receiver arg . maybe-own-and-extra-bindings)
  (define own (if (empty? maybe-own-and-extra-bindings)
                  receiver
                  (car maybe-own-and-extra-bindings)))
  (define extra-bindings (if (= 2 (length maybe-own-and-extra-bindings))
                             (cadr maybe-own-and-extra-bindings)
                             (env-pairs)))
  (cond 
    [(primitive? receiver) 
     (invoke-inverse-primitive arg own)]
    [(lang? 'Number receiver) 
     (invoke-inverse-number receiver arg own)]
    [(lang? 'String receiver)
     (invoke-inverse-string receiver arg own)]
    [(lang? 'Boolean receiver)
     (invoke-inverse-boolean receiver arg own)]
    [(lang? 'Symbol receiver)
     (invoke-inverse-symbol receiver arg own)]
    [(lang? 'Nil receiver)
     (invoke-inverse-nil receiver arg own)]
    [(lang? 'Unknown receiver)
     (invoke-inverse-unknown receiver arg own)]
    [(lang? 'List receiver)
     (invoke-inverse-list receiver arg own)]
    [(lang? 'Funject receiver)
     (invoke-inverse-funject receiver arg own extra-bindings)]
    [(lang? 'Class receiver)
     (invoke-inverse-class receiver arg own)]
    [(lang? 'Future receiver)
     (invoke-inverse-future receiver arg own)]
    [else 
     (error "invoke: I know not how to invoke " receiver "!")]))

(define (invoke-inverse-funject funject receiver arg own extra-bindings) 
  (invoke-funject (funject-inverse-of funject) receiver arg own extra-bindings))

(define (invoke-inverse-class klass arg own)
  (lang-contents klass
                 (lambda (exports super)
                   (try
                    (invoke-inverse exports arg own)
                    (catch 
                        (lambda (ex)
                          (if (and (lang-error? ex)
                                   (equal? 'cannot-find-match (lang-error-contents ex)))
                              (try (invoke-inverse super arg own)
                                   (catch 
                                       (lambda (ex)
                                         (if (lang-error-cannot-find-match? ex)
                                             (invoke-inverse primitive-class-class-instance arg own)
                                             (raise ex)))))
                              (raise ex))))))))

;(set-)list-<prop>(-of)
(define (list-id-of l)
  (unless (lang? 'List l) ;to optimize, remove this condition.
          (error "I cannot find the id of a non-list: " l "!")
          (mcadr l)))

(define (set-list-id! l val)
  (unless (lang? 'List l) ;to optimize, remove this condition.
          (error "I cannot set the id of a non-list: " l "!")
          (set-mcadr! l val)))

(define (list-elems-of l)
  (unless (lang? 'List l) ;to optimize, remove this condition.
          (error "I cannot find the elements of a non-list: " l "!")
          (mcaddr l)))

(define (set-list-elems! l val)
  (unless (lang? 'List l) ;to optimize, remove this condition.
          (error "I !cannot set the elements of a non-list: " l "!")
          (set-mcaddr! l val)))

;(set-)funject-<prop>(-of)
(define (funject-id-of funject)
  (unless (lang? 'Funject funject) ;to optimize, remove this condition.
          (error "I cannot find the pairs of a non-funject: " funject "!")
          (mcadr funject)))

(define (set-funject-id! funject id)
  (unless (lang? 'Funject funject) ;to optimize, remove this condition.
          (error "I cannot set the pairs of a non-funject: " funject "!")
          (set-mcadr! funject id)))

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

(define funject-pair-pattern-of mcar)

(define funject-pair-conseq-of mcadr)

(define funject-pair-env-of mcaddr)

(define create-funject-bound-pair mlist)

(define gen-funject-id
  ((lambda ()
     (define id 0)
     (lambda ()
       (set! id (add1 id))
       id))))




;<operation>(-<type>)-identifier
(define (lookup-identifier name env)
  (force-bound (env-get name env))) ;note the extra parenthases

(define (assign-strict-identifier! name eright env)
  (env-set! name (bind-as-though-unevaled eright) env))

(define (assign-lazy-identifier! name aright env)
  (env-set! name (bind-analyzed aright env) env))

(define (reset-strict-identifier! name eright env)
  (env-reset! name (bind-as-though-unevaled eright) (env-parent-of env)))

(define (reset-lazy-identifier! name aright env) 
  (env-reset! name (bind-analyzed aright env) (env-parent-of env)))

;bind-{analyzed/as-though-unevaled/funject-pair}
;force-bound
(define (bind-analyzed exp env)
  (mlist exp env))

(define (bind-as-though-unevaled exp) 
  (mlist (lambda (_) exp) '()))

(define (bind-delayed-primitive prim)
  (mlist prim '()))

(define (force-bound exp--env) 
  ((mcar exp--env) (mcadr exp--env)))

(define (bind-funject-pair p env)
  (mlist-contents p
                  (lambda (pattern consequent)
                    (create-funject-bound-pair pattern consequent env))))

;funject-pairs
(define funject-pairs mlist)




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
                       (token-unknown? pattern)
                       (not (token-any? pattern))
                       (token-identifier? pattern)
                       (token-symbol? pattern))
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
              (token-symbol? pattern)
              (token-nil? pattern)
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
            (if (or (empty? elems) (empty? arg-elems))
                (if (equal? (empty? elems) (empty? arg-elems))
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
                                    (inverse (invoke-inverse ereceiver (create-lang 'List (mlist arg epattern-arg))))
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
              [(token-symbol? pattern-arg) (list empty (create-lang 'Symbol (mcadr (pattern-arg))))]
              [(token-nil? pattern-arg) (list empty (create-lang 'Nil))]
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



;;;;    Bridging

(define (bridge thing)
  (cond 

    ;Things of a simple type
    [(number? thing)
     (create-lang 'Number thing)]
    [(string? thing)
     (create-lang 'String thing)]
    [(boolean? thing)
     (create-lang 'Boolean thing)]
    [(symbol? thing)
     (create-lang 'Symbol thing)]
    [(future? thing)
     (create-lang 'Future thing)]
    [(void? thing)
     (create-lang 'Nil)]
    
    ;Listy things
    [(list? thing)
     (create-lang 'List (list->mlist (map bridge thing)))]
    [(mlist? thing)
     (create-lang 'List (mmap bridge thing))]
    [(vector? thing)
     (create-lang 'List (mmap bridge (mlist->list (vector->list thing))))]
    
    ;Procedures
    [(procedure? thing)
     (create-primitive (lambda (arg own)
                         (bridge (apply thing (unbridge arg))))
                       default-primitive-inverse)]
    
    [else (error "I know not how to bridge" thing)]))

(define (unbridge lang)
  (cond
    
    ;Things of a simple type
    [(or (lang? 'Number lang)
         (lang? 'String lang)
         (lang? 'Boolean lang)
         (lang? 'Future lang))
     (lang-contents lang identity)]
    [(lang? 'Symbol lang)
     (string->symbol (lang-contents lang identity))]
    [(lang? 'Nil lang)
     empty]
    [(lang? 'Unknown lang)
     (user-error 'lack-briding-analog "You cannot bridge unknowns; Racket has no proper analog.")]
    
    ;Lists
    [(lang? 'List lang)
     (lang-contents lang
                    (lambda (elems)
                      (mlist->list (mmap unbridge elems))))]
    
    ;Funjects
    [(or (lang? 'Funject lang)
         (primitive? lang))
     (lambda arg
       (unbridge (invoke lang (bridge arg))))]
    [else (error "I know not how to unbridge" lang)]))

















;;;;    Primitive struct and helpers

;Primitive funjects rely on these:
(define lang-nil (create-lang 'Nil))
(define lang-unknown (create-lang 'Unknown))
(define lang-equal? equal?)

(struct primitive (vanilla-of inverse-of) #:mutable #:transparent #:constructor-name create-primitive)

#|(define (create-primitive itself inverse)
  (create-primitive-bare (lambda (arg own)
                           (try 
                            (itself arg own)
                            (catch
                                (lambda (ex)
                                  (if (lang-error-cannot-find-match? ex)
                                      (default-primitive-parent arg own)
                                      (raise ex))))))
                         (lambda (arg own)
                           (try 
                            (inverse arg own)
                            (catch 
                                (lambda (ex)
                                  (if (lang-error-cannot-find-match? ex)
                                      (default-primitive-inverse arg own)
                                      (raise ex))))))))|#

(define (primitive-contents prim f)
  (assert (primitive? prim) "primitive-contents: I can only take a primitive, but you passed me:" prim) ;to optimize, remove this line.
  (apply f (list (primitive-vanilla-of prim)
                 (primitive-inverse-of prim))))

(define (create-primitive-infix-operator right-type? result-type? op op-inv)
  (define self (lambda (left)
                 (create-primitive (lambda (other own)
                                     (unless (right-type? other)
                                       (invoke primitive-funject-god other)
                                       (lang-contents other
                                                      (lambda other-contents
                                                        (apply op (cons left other-contents))))))
                                   (lambda (result--arg own)
                                     (lang-list-contents result--arg
                                                         (lambda (k) 
                                                           (default-primitive-parent result--arg own))
                                                         (lambda (result arg)
                                                           (unless (and (result-type? result)
                                                                        (lang? 'Unknown arg))
                                                                   (invoke primitive-funject-inverse-god result--arg self)
                                                                   (lang-contents result
                                                                                  (lambda result-contents
                                                                                    (apply op-inv (append result-contents (list left)))))))))))) 
  self)

(define (create-primitive-unoverloaded-infix-operator right-type result-type op op-inv)
  (create-primitive-infix-operator (partial lang? right-type)
                                   (partial lang? result-type)
                                   (lambda (left right)
                                     (create-lang result-type (op left right)))
                                   (lambda (result left)
                                     (create-lang 'List (mlist (create-lang right-type (op-inv result left)))))))

(define (invoke-primitive receiver arg own)
  ((primitive-vanilla-of receiver) arg own))

(define (invoke-inverse-primitive receiver arg own) 
  ((primitive-inverse-of receiver) arg own))


  
  
  









  
  
;;;;     Builtin classes

;class helpers (for primitive-funject-god)
(define (create-delegate-instance-method-to-class class-itself)
  (let* ((instance-prototype (invoke class-itself (create-lang 'Symbol "instance"))))
    (lambda (method-name self)
      (let* ((method (invoke instance-prototype method-name)))
        (if (has?-match method (create-lang-list-mlist self))
         (invoke method (create-lang-list-mlist self))
         (create-primitive (lambda (arg own)
                             (invoke method (create-lang-list-mlist self arg)))
                           (lambda (inverse-arg own)
                             (lang-list-contents inverse-arg
                                                 user-error-cannot-find-match
                                                 (lambda (result arg)
                                                   (invoke-inverse method (create-lang 'List (mlist result (create-lang 'List (mlist self arg))))))))))))))

(define (create-delegate-instance-method-inverse-to-class class-itself) ;Yak does not currently support (most) inverses of instances.
    (lambda (arg own) ; I need to wrap user-error-cannot-find-match to resolve a cyclic dependency.
      (user-error-cannot-find-match own arg)))

(define (delegate-to-class-class-instance arg own) ((create-delegate-instance-method-to-class primitive-class-class) arg own))
(define (delegate-to-class-class-instance-inverse arg own) ((create-delegate-instance-method-inverse-to-class primitive-class-class) arg own))
                                           

(define (create-primitive-class class-itself class-inverse instance instance-inverse . maybe-super)
  (let ((class-itself (create-primitive class-itself
                                        class-inverse))
        (instance (create-primitive instance
                                    instance-inverse)))
     (create-primitive (lambda (arg own)
                         (if (equal-lang? 'Symbol arg "instance")
                             instance
                             (invoke class-itself arg own)))
                       (lambda (arg own)
                         (if (lang-equal? arg instance)
                             (create-lang 'List (mlist (create-lang 'Symbol "instance")))
                             ((primitive-inverse-of class-itself) arg own))))))

(define (try-symbols methods
                     arg
                     own
                     on-failure)
  (lang-contents-typed arg
                       'Symbol
                       on-failure
                       (lambda (name)
                         (hash-if-match methods 
                                        name
                                        (lambda (name method)
                                          method)
                                        (lambda (k) (on-failure))))))

(define (try-symbols-inverse methods
                             arg
                             own
                             on-failure)
  (lang-list-contents arg
                      (lambda (result maybe-unknown)
                        (unless (lang? 'Unknown maybe-unknown)
                                (on-failure)
                                (hash-if-val-match methods 
                                                   result
                                                   (lambda (name v)
                                                     (let ((possible-answer (create-lang 'Symbol name)))
                                                       (unless (equal? result (invoke own possible-answer))
                                                         (on-failure)
                                                         (create-lang-list-mlist possible-answer))))
                                                   (lambda (k) 
                                                     (on-failure)))))))

(define (get-class-instance klass)
  (invoke klass (create-lang 'Symbol "instance")))

;class

(define primitive-class-class
  (create-primitive-class delegate-to-class-class-instance
                          delegate-to-class-class-instance-inverse
                          (lambda (arg own)
                            (try-symbols primitive-class-instance
                                         arg
                                         own
                                         (lambda ()
                                           (invoke primitive-funject-class-instance arg own))))
                          (lambda (arg own)
                            (try-symbols-inverse primitive-class-instance
                                                 arg
                                                 own
                                                 (lambda ()
                                                   (invoke-inverse primitive-funject-class-instance arg own))))))
(define primitive-class-class-instance (get-class-instance primitive-class-class))

; funject
(define primitive-funject-class
  (create-primitive-class delegate-to-class-class-instance
                          delegate-to-class-class-instance-inverse
                          (lambda (arg own)
                            (try-symbols primitive-funject-instance
                                         arg
                                         own
                                         (lambda ()
                                           (user-error-cannot-find-match own arg))))
                          (lambda (arg own)
                            (try-symbols-inverse primitive-funject-instance
                                                 arg 
                                                 own
                                                 (lambda ()
                                                   (user-error-cannot-find-match own arg))))))
(define primitive-funject-class-instance (get-class-instance primitive-funject-class))



; number
(define primitive-number-class 
  (create-primitive-class (lambda (arg own)
                            (cond
                              [(equal-lang? 'Symbol arg "e") 
                               (create-lang 'Number 2.71828182845904523536028747135266249775724709369995)]
                              [(equal-lang? 'Symbol arg "pi") 
                               (create-lang 'Number 3.14159265358979323846264338327950288419716939937510)]
                              [(lang? 'List arg)
                               (lang-contents arg
                                              (lambda (elems)
                                                (unless (= 1 (mlength elems))
                                                        (delegate-to-class-class-instance arg own)
                                                        (invoke (mcar elems) (create-lang 'Symbol "to-number")))))]
                              [else
                               (delegate-to-class-class-instance arg own)]))
                          (lambda (result--arg own)
                            (lang-list-contents result--arg
                                                (lambda ()
                                                  (delegate-to-class-class-instance-inverse result--arg own))
                                                (lambda (result arg)
                                                  (cond
                                                    [(equal-lang? 'Number result 2.71828182845904523536028747135266249775724709369995)
                                                     (create-lang 'List (mlist (create-lang 'Symbol "e")))]
                                                    [(equal-lang? 'Number result 3.14159265358979323846264338327950288419716939937510)
                                                     (create-lang 'List (mlist (create-lang 'Symbol "pi")))]
                                                    [(equal? result (invoke primitive-number-class (create-lang 'Symbol "instance")))
                                                     (create-lang 'List (mlist (create-lang 'Symbol "instance")))]
                                                    [else
                                                     (delegate-to-class-class-instance-inverse result--arg own)]))))
                          (lambda (arg own)
                            (try-symbols primitive-number-instance
                                         arg 
                                         own
                                         (lambda ()
                                           (invoke primitive-funject-class-instance arg own))))
                          (lambda (arg own)
                            (cond
                              (try-symbols-inverse primitive-number-instance
                                                   arg 
                                                   own
                                                   (lambda ()
                                                     (invoke-inverse primitive-funject-class-instance arg own)))))))
(define primitive-number-class-instance (get-class-instance primitive-number-class))

;string
(define primitive-string-class
  (create-primitive-class delegate-to-class-class-instance
                          delegate-to-class-class-instance-inverse
                          (lambda (arg own)
                            (try-symbols primitive-string-instance
                                         arg
                                         own
                                         (lambda ()
                                           (invoke primitive-funject-class-instance arg own))))
                          (lambda (arg own)
                            (try-symbols-inverse primitive-string-instance
                                                 arg 
                                                 own
                                                 (lambda ()
                                                   (invoke-inverse primitive-funject-class-instance arg own))))))
(define primitive-string-class-instance (get-class-instance primitive-string-class))


;boolean
(define primitive-boolean-class
  (create-primitive-class delegate-to-class-class-instance
                          delegate-to-class-class-instance-inverse
                          (lambda (arg own)
                            (try-symbols primitive-boolean-instance
                                         arg
                                         own
                                         (lambda ()
                                           (invoke primitive-funject-class-instance arg own))))
                          (lambda (arg own)
                            (try-symbols-inverse primitive-boolean-instance
                                                 arg
                                                 own
                                                 (lambda ()
                                                   (invoke-inverse primitive-funject-class-instance arg own))))))
(define primitive-boolean-class-instance (get-class-instance primitive-boolean-class))


;symbol
(define primitive-symbol-class
  (create-primitive-class delegate-to-class-class-instance
                          delegate-to-class-class-instance-inverse
                          (lambda (arg own)
                            (try-symbols primitive-symbol-instance
                                         arg
                                         own
                                         (lambda ()
                                           (invoke primitive-funject-class-instance arg own))))
                          (lambda (arg own)
                            (try-symbols-inverse primitive-symbol-instance
                                                 arg
                                                 own
                                                 (lambda ()
                                                   (invoke-inverse primitive-funject-class-instance arg own))))))
(define primitive-symbol-class-instance (get-class-instance primitive-symbol-class))


;nil
(define primitive-nil-class
  (create-primitive-class delegate-to-class-class-instance
                          delegate-to-class-class-instance-inverse
                          (lambda (arg own)
                            (try-symbols primitive-nil-instance
                                         arg
                                         own
                                         (lambda ()
                                           (invoke primitive-funject-class-instance arg own))))
                          (lambda (arg own)
                            (try-symbols-inverse primitive-nil-instance
                                                 arg
                                                 own
                                                 (lambda ()
                                                   (invoke-inverse primitive-funject-class-instance arg own))))))
(define primitive-nil-class-instance (get-class-instance primitive-nil-class))


;unknown
(define primitive-unknown-class
  (create-primitive-class delegate-to-class-class-instance
                          delegate-to-class-class-instance-inverse
                          (lambda (arg own)
                            (try-symbols primitive-unknown-instance
                                         arg
                                         own
                                         (lambda ()
                                           (invoke primitive-funject-class-instance arg own))))
                          (lambda (arg own)
                            (try-symbols-inverse primitive-unknown-instance
                                                 arg 
                                                 own
                                                 (lambda ()
                                                   (invoke-inverse primitive-funject-class-instance arg own))))))
(define primitive-unknown-class-instance (get-class-instance primitive-unknown-class))


;list
(define primitive-list-class
  (create-primitive-class delegate-to-class-class-instance
                          delegate-to-class-class-instance-inverse
                          (lambda (arg own)
                            (try-symbols primitive-list-instance
                                         arg
                                         own
                                         (lambda ()
                                           (invoke primitive-funject-class-instance arg own))))
                          (lambda (arg own)
                            (try-symbols-inverse primitive-list-instance
                                                 arg
                                                 own
                                                 (lambda ()
                                                   (invoke-inverse primitive-funject-class-instance arg own))))))
(define primitive-list-class-instance (get-class-instance primitive-list-class))


;future
(define primitive-future-class
  (create-primitive-class delegate-to-class-class-instance
                          delegate-to-class-class-instance-inverse
                          (lambda (arg own)
                            (try-symbols primitive-future-instance
                                         arg
                                         own
                                         (lambda ()
                                           (invoke primitive-funject-class-instance arg own))))
                          (lambda (arg own)
                            (try-symbols-inverse primitive-future-instance
                                                 arg 
                                                 own
                                                 (lambda ()
                                                   (invoke-inverse primitive-funject-class-instance arg own))))))
(define primitive-future-class-instance (get-class-instance primitive-future-class))

















;;;;    Primitives

(define primitive-funject-god
  (let ((delegate (create-delegate-instance-method-to-class primitive-funject-class))
        (delegate-inverse (create-delegate-instance-method-inverse-to-class primitive-funject-class)))
    (create-primitive (lambda (arg own)
                        (cond 
                          [(lang? 'Symbol arg)
                           (lang-contents arg
                                          (lambda (sym)
                                            (cond 
                                              [(equal? sym "is") 
                                               (create-primitive-funject-is own)]
                                              [else 
                                               (delegate arg own)])))]
                          [else 
                           (delegate arg own)]))
                      delegate-inverse)))

(define primitive-funject-inverse-god (create-primitive (primitive-inverse-of primitive-funject-god) (primitive-vanilla-of primitive-funject-god)))

(define default-primitive-parent (primitive-vanilla-of primitive-funject-god))

(define default-primitive-inverse (primitive-vanilla-of primitive-funject-inverse-god))

(define create-primitive-funject-is (create-primitive-infix-operator lang-any? ;to optimize, change this to sycophant
                                                                     lang-any? ;to optimize, change this to sycophant
                                                                     (lambda (a b)
                                                                       (create-lang 'Boolean (lang-equal? a b)))
                                                                     (lambda (result left)
                                                                       (if (equal? result (mlist 'Boolean true))
                                                                           (create-lang 'List left)
                                                                           (default-primitive-parent (create-lang-list-mlist result 
                                                                                                                             (create-lang 'Unknown)))))))

(define (has?-match self arg . maybe-extra-bindings)
  (or false ;insert non-symbol args here
      (and (primitive? self) ;I ensure that self is primitive; if I try to invoke self, the interpreter may loop between "has?" and invoke.
                                     (try
                                      (begin (invoke self arg) true)
                                      (catch (lambda (ex)
                                               (if (lang-error-cannot-find-match? ex)
                                                   false
                                                   (raise ex))))))
      (and (lang? 'Symbol arg)
           (lang-contents arg
                          (lambda (name)
                            (or (->boolean (member name '("is" "==" "clone" "number?" "string?" "boolean?" "symbol?" "unknown?" "nil?" "list?" "integer?" "float?" "to-string" "inspect" "silently" "apply" "then" "on" "while-true" "do-while-true" "has?" "append" "insert" "member-of?" "kind-of?")))
                                (and (lang? 'Class self)
                                     (or (->boolean (member name '("superclass" "subclasses" "all-subclasses" "subclass?" "superclass?" "methods" "all-methods" "instance")))
                                         (lang-contents self
                                                        (lambda (exports super)
                                                          (if (equal? self primitive-funject-class) (error "I might end up in an infinite loop of class inheritance!") 'ok)
                                                          (or (has?-match exports arg)
                                                              (has?-match super arg))))))
                                (and (lang? 'Number self)
                                     (->boolean (member name '("+" "-" "*" "/" "%" "^" "negated" "sqrt" "ln" "log" "sin" "cos" "tan" "sec" "csc" "cot" "asin" "acos" "atan" "atan/" "abs " "ceil" "floor" "round" "times"))))
                                (and (lang? 'String self)
                                     (->boolean (member name '("*" "+" "length" "contains?" "begins-with?" "ends-with?" "index-of" "last-index-of" "split" "substring" "replace" "replace-first" "repeat" "uppercase" "lowercase" "swapcase" "capitalize" "titlecase"))))
                                (and (lang? 'Boolean self)
                                     (->boolean (member name '("and" "or" "xor" "not"))))
                                (and (lang? 'Nil self)
                                     (->boolean (member name '("nil?"))))
                                (and (lang? 'Unknown self)
                                     (->boolean (member name '("unknown?"))))
                                (and (lang? 'Symbol self)
                                     (->boolean (member name '("to-string"))))
                                (and (lang? 'List self)
                                     (->boolean (member name '("*" "+" "length" "count" "empty?" "contains?" "delete-at!" "delete-at" "insert!" "insert" "pop!" "pop" "push!" "push" "shift!" "shift" "unshift!" "unshift" "index-of" "last-index-of" "join" "sort!" "sort" "sort!" "sort" "map!" "map" "pluck!" "pluck" "invoke!" "invoke" "filter!" "filter" "reject!" "reject" "reduce" "reduce-right" "every" "any" "first" "first" "last" "last" "take!" "take" "take-while!" "take-while" "drop!" "drop" "drop-while!" "drop-while" "union!" "union" "intersection!" "intersection" "unique!" "unique" "difference!" "difference" "remove!" "remove" "remove-all!" "remove-all" "compact!" "compact" "shuffle!" "shuffle" "reverse!" "reverse"))))
                                (and (lang? 'Future self)
                                     (->boolean (member name '("touch"))))
                                (and (lang? 'Funject self)
                                     (funject-has?-match self arg self))))))))
(define (lang-has?-match own arg . maybe-extra-bindings)
  (create-lang 'Boolean (apply has?-match 
                               (append (list own arg)
                                       maybe-extra-bindings))))
(define (funject-has?-match own arg . maybe-extra-bindings)
  (mcadr (apply invoke (append (list (create-primitive-has?-match own) arg)
                               maybe-extra-bindings))))
(define (create-primitive-has?-match own . maybe-extra-bindings)
  (let ((extra-bindings (env-pairs-cons (create-env-pair "own" own)
                                        (if (empty? maybe-extra-bindings)
                                            (env-pairs)
                                            (car maybe-extra-bindings)))))
    (if (lang? 'Funject own)
        (create-primitive (lambda (arg _)
                            (if (mormap (lambda (pair) 
                                          (choice-bindings-from-matching (mcar pair) arg (env-extend extra-bindings (mcaddr pair))))
                                        (funject-pairs-of own))
                                (create-lang 'Boolean true)
                                (lang-has?-match (funject-parent-of own) arg)))
                          (lambda (has-or-doesnt _)
                            (unless (lang? 'Boolean has-or-doesnt)
                              (create-lang 'List '())
                              (if (mcadr has-or-doesnt)
                                  (create-lang 'List '())   ;Todo: invent a proper inverse for .has?
                                  (create-lang 'List '())))))
        (error "I cannot call .has? on a non-funject; I should have passed that to a builtin!"))))

(define funject-god-method-names (list "is" "has?"))
(define primitive-funject-god-has?-match 
  (create-primitive (lambda (arg _)
                      (if (and (lang? 'Symbol arg)
                               (member (mcadr arg) funject-god-method-names))
                          (create-lang 'Boolean true)
                          (create-lang 'Boolean false)))
                    (lambda (result _)
                      (unless (lang? 'Boolean result)
                              (create-lang 'List '())
                              (if (mcadr result)       ;Todo: decide on a real inverse for .has?
                                  (create-lang 'List (map (lambda (str) 
                                                            (create-lang 'Symbol str))
                                                          funject-god-method-names))
                                  (create-lang 'List '()))))))


(define primitive-put
  (create-primitive (lambda (arg own)
                      (unless (and (lang? 'List arg)
                                   (lang-contents arg
                                                  (lambda (elems)
                                                    (mandmap (partial lang? 'String) elems))))
                              (user-error-type "primitive-print" "list of a strings" arg)
                        (begin (lang-contents arg
                                              (partial mmap
                                                       (lambda (str)
                                                         (lang-contents str
                                                                        display))))
                               lang-nil)))
                    default-primitive-inverse))

(define primitive-print
  (create-primitive (lambda (arg own)
                      ((primitive-vanilla-of primitive-put) arg own)
                      (newline)
                      lang-nil)
                    default-primitive-inverse))

(define primitive-by
  (create-primitive (lambda (arg own)
                      (create-primitive (lambda (funject-list own)
                                          (lang-contents-typed funject-list
                                                               'List
                                                               (default-primitive-parent funject-list own)
                                                               (lambda (elems)
                                                                 (invoke (mcar elems) arg))))
                                        default-primitive-inverse))
                    default-primitive-inverse))

(define primitive-futures (make-hash))
(define primitive-future
  (create-primitive (lambda (to-eval own)
                      (let ((f (bridge (future (lambda () (invoke to-eval (create-lang 'List empty)))))))
                        (hash-set! primitive-futures 
                                   f
                                   to-eval)
                        f))
                    (lambda (f own)
                      (hash-if-match primitive-futures
                                     f
                                     (lambda (f to-eval)
                                       (create-lang-list-mlist to-eval))
                                     (lambda (f) 
                                       (create-lang-list-mlist))))))

(define primitive-touch
  (create-primitive (lambda (arg own)
                      (touch (unbridge arg)))
                    default-primitive-inverse))

(define create-primitive-number-+ (create-primitive-unoverloaded-infix-operator 'Number 'Number + -))

(define create-primitive-number-- (create-primitive-unoverloaded-infix-operator 'Number 'Number - (compose - -)))

(define create-primitive-number-* (create-primitive-unoverloaded-infix-operator 'Number 'Number * /))

(define create-primitive-number-/ (create-primitive-unoverloaded-infix-operator 'Number 'Number / (compose / /)))

(define create-primitive-string-+ (create-primitive-infix-operator (partial lang? 'String)
                                                                   (partial lang? 'String)
                                                                   (lambda (left right)
                                                                     (create-lang 'String (string-append left right)))
                                                                   (lambda (result left)
                                                                     (create-lang 'List 
                                                                                  (unless (string-begins-with? result left)
                                                                                          (mlist)
                                                                                          (mlist (create-lang 'String 
                                                                                                              (substring result (string-length left)))))))))

(define create-primitive-list-+ (create-primitive-infix-operator (partial lang? 'List)
                                                                 (partial lang? 'List)
                                                                 (lambda (left right)
                                                                   (create-lang 'List (mappend left right)))
                                                                 (lambda (result left)
                                                                   (create-lang 'List 
                                                                                (unless (mlist-begins-with? result left)
                                                                                        (mlist)
                                                                                        (mlist (create-lang 'List
                                                                                                            (apply-n (mlength left) mcdr result))))))))
                                                                                       


(define (create-primitive-class-new module-env) 
  (let* ((exports (lookup-identifier "exports" module-env))
         (instance (lookup-identifier "instance" module-env)))
    (create-primitive (lambda (constructor-arg own)
                        (define self (create-lang 'Funject
                                                  (gen-funject-id)
                                                  (mlist (bind-funject-pair (create-funject-pair (create-lang 'Symbol "class")
                                                                                                 (mlist 'Evaled exports))
                                                                            module-env)
                                                         (bind-funject-pair (create-funject-pair (tokenize 'Parameter "@else")
                                                                                                 (mlist 'Unevaled (lambda (env)
                                                                                                                    (let ((meth (invoke instance (lookup-identifier "@else" env))))
                                                                                                                      (if (has?-match meth (create-lang 'List (mlist self)))
                                                                                                                          (invoke meth (create-lang 'List (mlist self)) meth (env-pairs-for-private-of self))
                                                                                                                          (create-primitive-class-instance-method meth self))))))
                                                                            module-env))
                                                  primitive-funject-never-to-be-called
                                                  primitive-funject-inverse-god))
                        (env-set! self 
                                  (create-lang 'Funject 
                                               (gen-funject-id) 
                                               '()
                                               primitive-funject-god
                                               primitive-funject-inverse-god)
                                  privates)
                        (invoke-funject-itself-with-bindings (invoke instance 
                                                                     (create-lang 'Symbol 
                                                                                  "initialize"))
                                                             (create-lang 'List (mlist self constructor-arg))
                                                             (env-pairs-for-private-of self))
                        self)
                      default-primitive-inverse)))

(define (create-primitive-class-instance-method meth self)
  (create-primitive (lambda (arg own) 
                      (invoke meth (create-lang 'List (mlist self arg)) meth (env-pairs-for-private-of self)))
                    (lambda (arg own)
                      (assert (and (lang? 'List arg)
                                   (mlength (mcadr arg))) "create-primitive-class-instance-method: my inverse was passed not an array of two arguments, but this:" arg)
                      (let ((result (mcaadr arg))
                            (inverse-arg (mcadadr arg)))
                        (invoke (funject-inverse-of meth) (create-lang 'List (mlist result
                                                                                    (create-lang 'List (mlist self
                                                                                                              inverse-arg)))))))))

(define primitive-funject-never-to-be-called 
  (create-primitive (lambda (arg own)
                      (error "primitive-funject-never-to-be-called: I said don't call me!"))
                    (lambda (arg own)
                      (error "primitive-funject-never-to-be-called: why did you call my inverse?"))))
















;;;;    Builtin Class & Instance Primitives

;;  helpers

(define (placeholder) (error "You called a placeholder!"))
(define primitive-placeholder
  (create-primitive (lambda (arg own)
                      (placeholder))
                    (lambda (arg own)
                      (error "You called the inverse of a placeholder!"))))
(define (primitive-placeholder-named name)
  (create-primitive (lambda (arg own)
                      (error (string-append "You called the placeholder called \"" 
                                            (->string name) 
                                            "\"!")))
                    (lambda (arg own)
                      (error (string-append "You called the inverse of the placeholder called \""
                                            (->string name)
                                            "\"!")))))

(define (create-create-primitive-attribute on-failure on-failure-inverse)
  (lambda (vanilla inverse)
    (create-primitive (lambda (arg own)
                        (lang-list-n-contents arg
                                              1
                                              (lambda ()
                                                (on-failure arg own))
                                              (lambda (self)
                                                (vanilla arg own self))))
                      (lambda (result--arg own)
                        (lang-list-n-contents result--arg
                                              2
                                              (lambda ()
                                                (on-failure-inverse result--arg own))
                                              (lambda (result self-list)
                                                (unless (lang? 'Unknown self-list)
                                                  (lang-list-n-contents self-list
                                                                        1
                                                                        (lambda ()
                                                                          (on-failure-inverse result--arg own))
                                                                        (lambda (self)
                                                                          (inverse result--arg own result self)))
                                                  (lang-contents (inverse result--arg own result (create-lang 'Unknown))
                                                                 (lambda (posses)
                                                                   (mmap (lambda (poss)
                                                                           (create-lang-list-mlist poss))
                                                                         posses))))))))))
(define create-primitive-funject-attribute (create-create-primitive-attribute (lambda (arg own)
                                                                                (user-error-cannot-find-match own arg))
                                                                              (lambda (arg own)
                                                                                (user-error-cannot-find-match own arg))))
(define create-primitive-attribute (create-create-primitive-attribute default-primitive-parent
                                                                      default-primitive-inverse))
(define default-primitive-attribute-parent (lambda (arg own self)
                                             (default-primitive-parent arg own)))
(define default-primitive-attribute-inverse (lambda (arg own result self)
                                              (default-primitive-inverse arg own)))


(define (create-create-primitive-method on-failure on-failure-inverse)
  (lambda (vanilla inverse)
    (create-primitive (lambda (arg own)
                        (lang-list-n-contents arg
                                              2
                                              (lambda ()
                                                (on-failure arg own))
                                              (lambda (self other)
                                                (vanilla arg own self other))))
                      (lambda (result--arg own)
                        (lang-list-contents result--arg
                                            2
                                            (lambda () 
                                              (on-failure result--arg own))
                                            (lambda (result arg)
                                              (lang-list-contents arg
                                                                  2
                                                                  (lambda ()
                                                                    (on-failure result--arg own))
                                                                  (lambda (self other)
                                                                    (inverse result--arg own result self other)))))))))

(define create-primitive-method (create-create-primitive-method default-primitive-parent
                                                                default-primitive-inverse))
(define create-primitive-funject-method (create-create-primitive-method (lambda (arg own)
                                                                          (user-error-cannot-find-match own arg))
                                                                        (lambda (arg own)
                                                                          (user-error-cannot-find-match own arg))))
(define default-primitive-method-parent (lambda (arg own self other)
                                             (default-primitive-parent arg own)))
(define default-primitive-method-inverse (lambda (arg own result self other)
                                              (default-primitive-inverse arg own)))

;; Class 

;class (none)

;instance

(define primitive-class-instance (make-hash))
(hash-set! primitive-class-instance "class" (create-primitive-funject-attribute (lambda (arg own self)
                                                                                  primitive-class-class)
                                                                                default-primitive-inverse))
(hash-set! primitive-class-instance "superclass" (create-primitive-attribute (lambda (arg own self)
                                                                               (cond
                                                                                 [(lang? 'Class self)
                                                                                  (lang-contents self
                                                                                                 (lambda (_ super)
                                                                                                   super))]
                                                                                 [(or (equal? self primitive-class-class)
                                                                                      (equal? self primitive-funject-class)
                                                                                      (equal? self primitive-number-class)
                                                                                      (equal? self primitive-string-class)
                                                                                      (equal? self primitive-boolean-class)
                                                                                      (equal? self primitive-symbol-class)
                                                                                      (equal? self primitive-nil-class)
                                                                                      (equal? self primitive-unknown-class)
                                                                                      (equal? self primitive-list-class)
                                                                                      (equal? self primitive-future-class))
                                                                                  primitive-funject-class]
                                                                                 [else
                                                                                  (user-error 'know-not-superclass (string-append "I know not the superclass of " (->string self) "!"))]))
                                                                             (lambda (arg own result self)
                                                                               (default-primitive-inverse arg own))
                                                                             #|(lambda (arg own result self)
                                                                               (if (lang? 'Unknown self)
                                                                                   (invoke result (create-lang 'Symbol "subclasses"))
                                                                                   (default-primitive-parent arg own))))|#))
(hash-set! primitive-class-instance "subclasses" (primitive-placeholder-named "Class::subclasses"))
(hash-set! primitive-class-instance "all-subclasses" (primitive-placeholder-named "Class::all-subclasses"))
((lambda ()
   (define (subclass? arg own self other)
     (cond 
       [(lang? 'Funject self)
        (or (equal? self other)
            (subclass? arg 
                       own
                       (funject-parent-of self)
                       other)
            (and (lang? 'Class other)
                 (lang-contents other
                                (lambda (exports _)
                                  (equal? self exports)))))]
       [(lang? 'Class self)
        (or (equal? self other)
            (lang-contents self
                           (lambda (exports superclass)
                             (subclass? arg own superclass other)))
            (and (lang? 'Funject other)
                 (lang-contents self
                                (lambda (exports superclass) 
                                  (equal? exports other)))))]
       [(or (equal? self primitive-funject-class)
            (equal? self primitive-class-class)
            (equal? self primitive-number-class)
            (equal? self primitive-string-class)
            (equal? self primitive-boolean-class)
            (equal? self primitive-symbol-class)
            (equal? self primitive-nil-class)
            (equal? self primitive-unknown-class)
            (equal? self primitive-list-class)
            (equal? self primitive-future-class))
        (equal? other primitive-funject-class)]
       [else
        (user-error 'instance-received-wrong-self-type
                    "Class::subclass? can only determine the ancestry of a class, but you passed it this: "
                    self)]))
   
   (define (superclass? own arg self other)
     (cond 
       [(or (lang? 'Class other)
            (lang? 'Funject other))
        (subclass? arg 
                   own 
                   other 
                   self)]
       [(and (equal? self primitive-funject-class)
             (or (equal? other primitive-funject-class)
                 (equal? other primitive-class-class)
                 (equal? other primitive-number-class)
                 (equal? other primitive-string-class)
                 (equal? other primitive-boolean-class)
                 (equal? other primitive-symbol-class)
                 (equal? other primitive-nil-class)
                 (equal? other primitive-unknown-class)
                 (equal? other primitive-list-class)
                 (equal? other primitive-future-class)))]
       [else
        false]))
   (hash-set! primitive-class-instance "subclass?" (create-primitive-method (lambda (arg own self other)
                                                                              (lang-list-n-contents other
                                                                                                    1
                                                                                                    (lambda ()
                                                                                                      (user-error-cannot-find-match own arg))
                                                                                                    (lambda (other)
                                                                                                      (create-lang 'Boolean (subclass? arg own self other)))))
                                                                            default-primitive-inverse))
   (hash-set! primitive-class-instance "superclass?" (create-primitive-method (lambda (arg own self other)
                                                                              (lang-list-n-contents other
                                                                                                    1
                                                                                                    (lambda ()
                                                                                                      (user-error-cannot-find-match own arg))
                                                                                                    (lambda (other)
                                                                                                      (create-lang 'Boolean (superclass? arg own self other)))))
                                                                              default-primitive-inverse))))
(hash-set! primitive-class-instance "methods" (primitive-placeholder-named "Class::methods"))
(hash-set! primitive-class-instance "all-methods" (primitive-placeholder-named "Class::all-methods"))
(hash-set! primitive-class-instance "instance" (primitive-placeholder-named "Class::instance"))


;; Funject

;class (none)

;instance

(define primitive-funject-instance (make-hash))
(hash-set! primitive-funject-instance "class" (create-primitive-funject-attribute (lambda (arg own self)
                                                                                    primitive-funject-class)
                                                                                  default-primitive-inverse))
(hash-set! primitive-funject-instance "is" (create-primitive-funject-method (lambda (arg own self other)
                                                                              (create-lang 'Boolean (lang-equal? self other))) ;TODO: make "is" more strict [] isnt [].
                                                                            (lambda (result--arg own result self other)
                                                                              (user-error-cannot-find-match own result--arg))))
(hash-set! primitive-funject-instance "isnt" (create-primitive-funject-method (lambda (arg own self other)
                                                                                (create-lang 'Boolean (not (lang-equal? self other)))) ;TODO: make "isnt" more strict [] isnt [].
                                                                              (lambda (result--arg own result self other)
                                                                                (user-error-cannot-find-match own result--arg))))
(hash-set! primitive-funject-instance "==" (create-primitive-funject-method (lambda (arg own self other)
                                                                              (create-lang 'Boolean (lang-equal? self other)))
                                                                            (lambda (result--arg own result self other)
                                                                              (user-error-cannot-find-match own result--arg))))
(hash-set! primitive-funject-instance "clone" (create-primitive-funject-attribute (lambda (arg own self)
                                                                                    (cond
                                                                                      [(or (lang? 'Number self)
                                                                                           (lang? 'String self)
                                                                                           (lang? 'Boolean self)
                                                                                           (lang? 'Nil self)
                                                                                           (lang? 'Unknown self)
                                                                                           (lang? 'Class self))
                                                                                       self]
                                                                                      [(primitive? self)
                                                                                       (create-primitive (primitive-vanilla-of self)
                                                                                                         (primitive-inverse-of self))]
                                                                                      [(lang? 'List self)
                                                                                       (lang-contents self
                                                                                                      (lambda (elems)
                                                                                                        (create-lang 'List (mcopy elems))))]
                                                                                      [(lang? 'Funject self)
                                                                                       (lang-contents self
                                                                                                      (lambda (id pairs parent inverse)
                                                                                                        (create-lang 'Funject 
                                                                                                                     (gen-funject-id)
                                                                                                                     (mcopy pairs)
                                                                                                                     parent
                                                                                                                     inverse)))]
                                                                                      [(lang? 'Future self)
                                                                                       (user-error 'cannot-clone-future "I cannot clone a Future! (You cannot repeat the past.)")]
                                                                                      [(lang? 'Class self)
                                                                                       (lang-contents self
                                                                                                      (lambda (exports super)
                                                                                                        (create-lang 'Class exports super)))]))
                                                                                  default-primitive-inverse))
(for-each (lambda (type--meth-name)
            (let ([type (car type--meth-name)]
                  [meth-name (cdr type--meth-name)])
              (hash-set! primitive-funject-instance meth-name (create-primitive-attribute (lambda (arg own self)
                                                                                            (if (lang? type self)
                                                                                                (create-lang 'Boolean true)
                                                                                                (create-lang 'Boolean false)))
                                                                                          default-primitive-inverse))))
          '((Number . "number?")
            (String . "string?")
            (Boolean . "boolean?")
            (Symbol . "symbol?")
            (Nil . "nil?")
            (Unknown . "unknown?")
            (List . "list?")
            (Future . "future?")))
(hash-set! primitive-funject-instance "integer?" (create-primitive-attribute (lambda (arg own self)
                                                                            (create-lang 'Boolean (and (lang? 'Number self)
                                                                                                       (lang-contents self integer?))))
                                                                          default-primitive-inverse))
(hash-set! primitive-funject-instance "float?" (create-primitive-attribute (lambda (arg own self)
                                                                            (create-lang 'Boolean (and (lang? 'Number self)
                                                                                                       (lang-contents self (compose not integer?)))))
                                                                          default-primitive-inverse))
(hash-set! primitive-funject-instance "to-string" (create-primitive-attribute (lambda (arg own self)
                                                                                (create-lang 'String (cond 
                                                                                                       [(lang? 'Number self)
                                                                                                        (lang-contents self number->string)]
                                                                                                       [(lang? 'String self)
                                                                                                        (lang-contents self identity)]
                                                                                                       [(lang? 'Boolean self)
                                                                                                        (lang-contents self
                                                                                                                       (lambda (bool)
                                                                                                                         (if bool
                                                                                                                             "true"
                                                                                                                             "false")))]
                                                                                                       [(lang? 'Symbol self)
                                                                                                        (lang-contents self identity)]
                                                                                                       [(lang? 'Nil self)
                                                                                                        "nil"]
                                                                                                       [(lang? 'Unknown self)
                                                                                                        "unknown"]
                                                                                                       [(lang? 'Class self)
                                                                                                        "#<Class>"]
                                                                                                       [(lang? 'Future self)
                                                                                                        "#<Future>"]
                                                                                                       [(lang? 'List self)
                                                                                                        "#<List>"]
                                                                                                       [else
                                                                                                        "#<Funject>"])))
                                                                              default-primitive-inverse))
(hash-set! primitive-funject-instance "inspect" (create-primitive-attribute (lambda (arg own self)
                                                                                (create-lang 'String (cond 
                                                                                                       [(lang? 'Number self)
                                                                                                        (lang-contents self number->string)]
                                                                                                       [(lang? 'String self)
                                                                                                        (string-append "'"
                                                                                                                       (lang-contents self identity) 
                                                                                                                       "'")]
                                                                                                       [(lang? 'Boolean self)
                                                                                                        (lang-contents self
                                                                                                                       (lambda (bool)
                                                                                                                         (if bool
                                                                                                                             "true"
                                                                                                                             "false")))]
                                                                                                       [(lang? 'Symbol self)
                                                                                                        (string-append "." (lang-contents self identity))]
                                                                                                       [(lang? 'Nil self)
                                                                                                        "nil"]
                                                                                                       [(lang? 'Unknown self)
                                                                                                        "unknown"]
                                                                                                       [(lang? 'Class self)
                                                                                                        "#<Class>"]
                                                                                                       [(lang? 'Future self)
                                                                                                        "#<Future>"]
                                                                                                       [(lang? 'List self)
                                                                                                        "#<List>"]
                                                                                                       [else
                                                                                                        "#<Funject>"])))
                                                                              default-primitive-inverse))
(hash-set! primitive-funject-instance "silently" (primitive-placeholder-named "silently"))
(hash-set! primitive-funject-instance "apply" (primitive-placeholder-named "apply"))
(hash-set! primitive-funject-instance "then" (primitive-placeholder-named "then"))
(hash-set! primitive-funject-instance "on" (primitive-placeholder-named "on"))
(hash-set! primitive-funject-instance "while-true" (primitive-placeholder-named "while-true"))
(hash-set! primitive-funject-instance "do-while-true" (primitive-placeholder-named "do-while-true"))

(hash-set! primitive-funject-instance "has?" (create-primitive (lambda (arg own)
                                                                 (lang-list-n-contents arg
                                                                                       2
                                                                                       (lambda ()
                                                                                         (user-error-cannot-find-match own arg))
                                                                                       (lambda (self arg)
                                                                                         (lang-list-n-contents arg
                                                                                                               1
                                                                                                               (partial default-primitive-parent self arg)
                                                                                                               (lambda (sym)
                                                                                                                 (create-lang 'Boolean (has?-match self sym)))))))
                                                               default-primitive-inverse))

(hash-set! primitive-funject-instance "append!" (primitive-placeholder-named "append"))
(hash-set! primitive-funject-instance "insert!" (create-primitive (lambda (arg own)
                                                                    (lang-list-n-contents arg
                                                                                          2
                                                                                          user-error-cannot-find-match
                                                                                          (lambda (self insertee-list)
                                                                                            (lang-list-n-contents insertee-list
                                                                                                                  2
                                                                                                                  user-error-cannot-find-match
                                                                                                                  (lambda (n insertee)
                                                                                                                    (unless (and (lang? 'Funject self)
                                                                                                                                 (lang? 'Funject insertee))
                                                                                                                            (user-error 'cannot-modify-builtins "You may not modify builtin funjects!")
                                                                                                                            (lang-contents-typed n
                                                                                                                                                 'Number
                                                                                                                                                 user-error-cannot-find-match
                                                                                                                                                 (lambda (n)
                                                                                                                                                   (user-assert (<= n (mlength (funject-pairs-of insertee))) "I cannot insert a pair at an index larger than the number of pairs in the funject")
                                                                                                                                                   (set-funject-pairs! self (let-values ([(before after) (msplit-at (funject-pairs-of self) n)])
                                                                                                                                                                                (mappend before 
                                                                                                                                                                                         (funject-pairs-of insertee)
                                                                                                                                                                                         after)))
                                                                                                                                                   self))))))))
                                                                  default-primitive-inverse))
                                                                                                                                    
(hash-set! primitive-funject-instance "member-of?" (create-primitive-method (lambda (arg own self other)
                                                                                 (lang-list-n-contents other
                                                                                                       1
                                                                                                       (lambda ()
                                                                                                         (user-error-cannot-find-match own arg))
                                                                                                       (lambda (klass)
                                                                                                         (create-lang 'Boolean (equal? klass
                                                                                                                                       (invoke self (create-lang 'Symbol "class")))))))
                                                                               default-primitive-parent))
(hash-set! primitive-funject-instance "kind-of?" (create-primitive-method (lambda (arg own self other)
                                                                               (lang-list-n-contents other
                                                                                                     1
                                                                                                     (lambda ()
                                                                                                       (user-error-cannot-find-match own arg))
                                                                                                     (lambda (klass)
                                                                                                       (invoke (invoke klass (create-lang 'Symbol "superclass?"))
                                                                                                               (create-lang-list-mlist (invoke self (create-lang 'Symbol "class")))))))
                                                                             default-primitive-parent))


;;  Number

;class (none defined externally)

;instance

(define primitive-number-instance (make-hash))
(hash-set! primitive-number-instance "class" (create-primitive-funject-attribute (lambda (arg own self)
                                                                                   primitive-number-class)
                                                                                 default-primitive-inverse))
(hash-set! primitive-number-instance "+"
  (create-primitive (lambda (self--other own)
                      (lang-list-contents self--other
                                          (lambda (self other)    
                                            (user-assert (and (lang? 'Number self)
                                                              (lang? 'Number other))
                                                         "Number.instance.+: you can only add numbers!")
                                            (lang-contents self
                                                           (lambda (a)
                                                             (lang-contents other
                                                                            (lambda (b)
                                                                              (create-lang (+ a b)))))))))
                    (lambda (result--arg own)
                      (inverse-arg-contents-typed result--arg
                                                  'Number
                                                  'List
                                                  (lambda (result elems)
                                                    (lang-contents-typed result
                                                                         'Number
                                                                         (partial default-primitive-parent result--arg own)
                                                                         (lambda (result)
                                                                           (user-assert (= 2 (mlength elems)) (default-primitive-parent result--arg own))
                                                                           (mlist-contents elems
                                                                                           (lambda (a b)
                                                                                             (define (assume-known known)
                                                                                               (create-lang 'List (mlist (create-lang 'Number (- result known)))))
                                                                                             (cond 
                                                                                               [(lang? 'Unknown a)
                                                                                                (assume-known a)]
                                                                                               [(lang? 'Unknown b)
                                                                                                (assume-known b)]
                                                                                               [else
                                                                                                (default-primitive-parent result--arg own)]))))))))))

  
(hash-set! primitive-number-instance "-" (primitive-placeholder-named "Number::-"))
(hash-set! primitive-number-instance "*" (primitive-placeholder-named "Number::*"))
(hash-set! primitive-number-instance "/" (primitive-placeholder-named "Number::/"))
(hash-set! primitive-number-instance "%" (primitive-placeholder-named "Number::%"))
(hash-set! primitive-number-instance "^" (primitive-placeholder-named "Number::^"))
(hash-set! primitive-number-instance "sqrt" (primitive-placeholder-named "Number::sqrt"))
(hash-set! primitive-number-instance "ln" (primitive-placeholder-named "Number::ln"))
(hash-set! primitive-number-instance "log" (primitive-placeholder-named "Number::log"))
(hash-set! primitive-number-instance "sin" (primitive-placeholder-named "Number::sin"))
(hash-set! primitive-number-instance "cos" (primitive-placeholder-named "Number::cos"))
(hash-set! primitive-number-instance "tan" (primitive-placeholder-named "Number::tan"))
(hash-set! primitive-number-instance "sec" (primitive-placeholder-named "Number::sec"))
(hash-set! primitive-number-instance "csc" (primitive-placeholder-named "Number::csc"))
(hash-set! primitive-number-instance "cot" (primitive-placeholder-named "Number::cot"))
(hash-set! primitive-number-instance "asin" (primitive-placeholder-named "Number::asin"))
(hash-set! primitive-number-instance "acos" (primitive-placeholder-named "Number::acos"))
(hash-set! primitive-number-instance "atan" (primitive-placeholder-named "Number::atan"))
(hash-set! primitive-number-instance "atan/" (primitive-placeholder-named "Number::atan/"))
(hash-set! primitive-number-instance "abs " (primitive-placeholder-named "Number::abs "))
(hash-set! primitive-number-instance "ceil" (primitive-placeholder-named "Number::ceil"))
(hash-set! primitive-number-instance "floor" (primitive-placeholder-named "Number::floor"))
(hash-set! primitive-number-instance "round" (primitive-placeholder-named "Number::round"))
(hash-set! primitive-number-instance "times" (primitive-placeholder-named "Number::times"))


;;String

;class (none)

;instance

(define primitive-string-instance (make-hash))
(hash-set! primitive-string-instance "class" (create-primitive-funject-attribute (lambda (arg own self)
                                                                                   primitive-string-class)
                                                                                 default-primitive-inverse))
(hash-set! primitive-string-instance "*" (primitive-placeholder-named "String::*"))
(hash-set! primitive-string-instance "+" (primitive-placeholder-named "String::+"))
(hash-set! primitive-string-instance "length" (create-primitive-attribute (lambda (arg own self)
                                                                            (lang-contents-typed self
                                                                                                 'String
                                                                                                 user-error-cannot-find-match
                                                                                                 (lambda (str)
                                                                                                   (create-lang 'Number (string-length str)))))
                                                                          default-primitive-inverse))
(hash-set! primitive-string-instance "contains?" (primitive-placeholder-named "String::contains?"))
(hash-set! primitive-string-instance "begins-with?" (primitive-placeholder-named "String::begins-with?"))
(hash-set! primitive-string-instance "ends-with?" (primitive-placeholder-named "String::ends-with?"))
(hash-set! primitive-string-instance "index-of" (primitive-placeholder-named "String::index-of"))
(hash-set! primitive-string-instance "last-index-of" (primitive-placeholder-named "String::last-index-of"))
(hash-set! primitive-string-instance "split" (primitive-placeholder-named "String::split"))
(hash-set! primitive-string-instance "substring" (primitive-placeholder-named "String::substring"))
(hash-set! primitive-string-instance "replace" (primitive-placeholder-named "String::replace"))
(hash-set! primitive-string-instance "replace-first" (primitive-placeholder-named "String::replace-first"))
(hash-set! primitive-string-instance "repeat" (primitive-placeholder-named "String::repeat"))
(hash-set! primitive-string-instance "uppercase" (primitive-placeholder-named "String::uppercase"))
(hash-set! primitive-string-instance "lowercase" (primitive-placeholder-named "String::lowercase"))
(hash-set! primitive-string-instance "swapcase" (primitive-placeholder-named "String::swapcase"))
(hash-set! primitive-string-instance "capitalize" (primitive-placeholder-named "String::capitalize"))
(hash-set! primitive-string-instance "titlecase" (primitive-placeholder-named "String::titlecase"))


;;Boolean

;class (none)

;instance

(define primitive-boolean-instance (make-hash))
(hash-set! primitive-boolean-instance "class" (create-primitive-funject-attribute (lambda (arg own self)
                                                                                    primitive-boolean-class)
                                                                                  default-primitive-inverse))
(hash-set! primitive-boolean-instance "and" (create-primitive-method (lambda (arg own self other)
                                                                       (lang-contents-typed self
                                                                                            'Boolean
                                                                                            (lambda ()
                                                                                              (default-primitive-parent arg own))
                                                                                            (lambda (a)
                                                                                              (lang-contents-typed other
                                                                                                                   'Boolean
                                                                                                                   (lambda ()
                                                                                                                     (default-primitive-parent arg own))
                                                                                                                   (lambda (b)
                                                                                                                     (create-lang 'Boolean (and a b)))))))
                                                                     (lambda (arg own result self other)
                                                                       (default-primitive-inverse arg own))))
(hash-set! primitive-boolean-instance "or" (create-primitive-method (lambda (arg own self other)
                                                                       (lang-contents-typed self
                                                                                            'Boolean
                                                                                            (lambda ()
                                                                                              (default-primitive-parent arg own))
                                                                                            (lambda (a)
                                                                                              (lang-contents-typed other
                                                                                                                   'Boolean
                                                                                                                   (lambda ()
                                                                                                                     (default-primitive-parent arg own))
                                                                                                                   (lambda (b)
                                                                                                                     (create-lang 'Boolean (or a b)))))))
                                                                     (lambda (arg own result self other)
                                                                       (default-primitive-inverse arg own))))
(hash-set! primitive-boolean-instance "xor" (create-primitive-method (lambda (arg own self other)
                                                                       (lang-contents-typed self
                                                                                            'Boolean
                                                                                            (lambda ()
                                                                                              (default-primitive-parent arg own))
                                                                                            (lambda (a)
                                                                                              (lang-contents-typed other
                                                                                                                   'Boolean
                                                                                                                   (lambda ()
                                                                                                                     (default-primitive-parent arg own))
                                                                                                                   (lambda (b)
                                                                                                                     (create-lang 'Boolean (xor a b)))))))
                                                                     (lambda (arg own result self other)
                                                                       (default-primitive-inverse arg own))))
(hash-set! primitive-boolean-instance "not" (primitive-placeholder-named "boolean-not"))


;;Nil

;class (none)

;instance

(define primitive-nil-instance (make-hash))
(hash-set! primitive-nil-instance "class" (create-primitive-funject-attribute (lambda (arg own self)
                                                                                primitive-nil-class)
                                                                              default-primitive-inverse))
(hash-set! primitive-nil-instance "nil?" (create-primitive (lambda (arg own)
                                                             (lang-list-n-contents arg
                                                                                   1
                                                                                   (partial default-primitive-parent arg own)
                                                                                   (lambda (possibly-nil)
                                                                                     (cond
                                                                                       [(lang? 'Nil possibly-nil)
                                                                                        (create-lang 'Boolean true)]
                                                                                       [else
                                                                                        (create-lang 'Boolean false)]))))
                                                           (lambda (result--arg own)
                                                             (inverse-arg-contents-typed result--arg
                                                                                         'Boolean
                                                                                         'List
                                                                                         (lambda (result elems)
                                                                                           (unless (and (= 1 (mlength elems))
                                                                                                        (lang? 'Unknown (mcar elems)))
                                                                                             (default-primitive-parent result--arg own)
                                                                                             (if result
                                                                                                 (create-lang-list-mlist (create-lang 'Nil))
                                                                                                 (default-primitive-parent result--arg own))))))))


;;Unknown

;class (none)

;instance

(define primitive-unknown-instance (make-hash))
(hash-set! primitive-unknown-instance "class" (create-primitive-funject-attribute (lambda (arg own self)
                                                                                    primitive-unknown-class)
                                                                                  default-primitive-inverse))
(hash-set! primitive-unknown-instance "unknown?" (create-primitive (lambda (arg own)
                                                                     (cond
                                                                       [(lang? 'Unknown arg)
                                                                        (create-lang 'Boolean true)]
                                                                       [else
                                                                        (create-lang 'Boolean false)]))
                                                                   (lambda (result--arg own)
                                                                     (inverse-arg-contents-typed result--arg
                                                                                                 'Boolean
                                                                                                 'List
                                                                                                 (lambda (result elems)
                                                                                                   (unless (and (= 1 (mlength elems))
                                                                                                                (lang? 'Unknown (mcar elems)))
                                                                                                     (default-primitive-parent result--arg own)
                                                                                                     (if result
                                                                                                         (create-lang-list-mlist (create-lang 'Unknown))
                                                                                                         (default-primitive-parent result--arg own))))))))


;;Symbol

;class (none)

;instance

(define primitive-symbol-instance (make-hash))
(hash-set! primitive-symbol-instance "class" (create-primitive-funject-attribute (lambda (arg own self)
                                                                                   primitive-symbol-class)
                                                                                 default-primitive-inverse))
(hash-set! primitive-symbol-instance "to-string" (create-primitive (lambda (arg own)
                                                                     (lang-list-n-contents arg
                                                                                           1
                                                                                           (partial default-primitive-parent arg own)
                                                                                           (lambda (lang-sym)
                                                                                             (lang-contents-typed lang-sym
                                                                                                                  'Symbol
                                                                                                                  (lambda () ((partial default-primitive-parent arg own) arg own))
                                                                                                                  (partial create-lang 'String)))))
                                                                   (lambda (arg own)
                                                                     (lang-list-n-contents arg
                                                                                           2
                                                                                           (partial default-primitive-parent arg own)
                                                                                           (lambda (result arg)
                                                                                             (lang-list-n-contents arg
                                                                                                                   1
                                                                                                                   (partial default-primitive-parent arg own)
                                                                                                                   (lambda (maybe-unknown)
                                                                                                                     (unless (lang? 'Unknown maybe-unknown)
                                                                                                                             (partial default-primitive-parent arg own)
                                                                                                                             (lang-contents-typed result
                                                                                                                                                  'String
                                                                                                                                                  (partial default-primitive-parent arg own)
                                                                                                                                                  (lambda (internal)
                                                                                                                                                    (create-lang-list-mlist (create-lang 'Symbol internal))))))))))))
           
;;List

;class (none)

;instance

(define primitive-list-instance (make-hash))
(hash-set! primitive-list-instance "class" (create-primitive-funject-attribute (lambda (arg own self)
                                                                                 primitive-list-class)
                                                                               default-primitive-inverse))
(hash-set! primitive-list-instance "*" (primitive-placeholder-named "list-*"))
(hash-set! primitive-list-instance "+" (primitive-placeholder-named "list-+"))
(hash-set! primitive-list-instance "length" (create-primitive-attribute (lambda (arg own self)
                                                                          (lang-list-contents self
                                                                                              user-error-cannot-find-match
                                                                                              (lambda elems
                                                                                                (create-lang 'Number (length elems)))))
                                                                        default-primitive-method-inverse))
(hash-set! primitive-list-instance "count" (primitive-placeholder-named "list-count"))
(hash-set! primitive-list-instance "empty?" (primitive-placeholder-named "list-empty?"))
(hash-set! primitive-list-instance "contains?" (primitive-placeholder-named "list-contains?"))
((lambda ()
   (define (delete!-elems i self on-fail)
     (lang-contents-typed self
                          'List
                          on-fail
                          (lambda (elems) 
                            (cond [(or (<= (mlength elems) i)
                                       (< i (- (mlength elems))))
                                   (user-error 'list-delete-at!-index-out-of-range 
                                               "I cannot delete the " 
                                               (->string i)
                                               "th index of a "
                                               (->string (mlength elems)) 
                                               " element list!")]
                                  [(< i 0)
                                   (delete!-elems (+ i (mlength elems)) self on-fail)] 
                                  [else
                                   (set-mcadr! self (mappend (mtake elems i)
                                                             (mcdr (mdrop elems i))))
                                   self]))))
   (hash-set! primitive-list-instance "delete-at!" (create-primitive-method (lambda (arg own self other)
                                                                              (define on-fail (lambda ()
                                                                                                (default-primitive-parent arg own)))
                                                                              (lang-list-n-contents other
                                                                                                    1
                                                                                                    on-fail
                                                                                                    (lambda (lang-i)
                                                                                                      (lang-contents-typed lang-i
                                                                                                                           'Number
                                                                                                                           on-fail
                                                                                                                           (lambda (i)
                                                                                                                             (delete!-elems i self on-fail))))))
                                                                            default-primitive-method-inverse))))
(hash-set! primitive-list-instance "delete-at" (primitive-placeholder-named "list-delete-at"))
((lambda ()
   (define (insert!-elems i elem t self on-fail)
     (lang-contents-typed self
                          'List
                          on-fail
                          (lambda (elems) 
                            (set-mcadr! self (mappend (mtake elems i)
                                                      (mrepeat t elem)
                                                      (mdrop elems i)))
                            self)))
   (hash-set! primitive-list-instance "insert!" (create-primitive-method (lambda (arg own self other)
                                                                           (define on-fail (lambda ()
                                                                                             (default-primitive-parent arg own)))
                                                                           (lang-list-n-contents other
                                                                                                 2
                                                                                                 (lambda ()
                                                                                                   (lang-list-n-contents other
                                                                                                                         3
                                                                                                                         on-fail
                                                                                                                         (lambda (lang-i to-insert lang-t)
                                                                                                                           (lang-contents-typed lang-i
                                                                                                                                                'Number
                                                                                                                                                on-fail
                                                                                                                                                (lambda (i)
                                                                                                                                                  (lang-contents-typed lang-t
                                                                                                                                                                       'Number
                                                                                                                                                                       on-fail
                                                                                                                                                                       (lambda (t)
                                                                                                                                                                         (insert!-elems i to-insert t self on-fail))))))))
                                                                                                 (lambda (lang-i to-insert)
                                                                                                   (lang-contents-typed lang-i
                                                                                                                        'Number
                                                                                                                        on-fail
                                                                                                                        (lambda (i)
                                                                                                                          (insert!-elems i to-insert 1 self on-fail))))))
                                                                         default-primitive-method-inverse))))
(hash-set! primitive-list-instance "insert" (primitive-placeholder-named "list-insert"))
(hash-set! primitive-list-instance "pop!" (primitive-placeholder-named "list-pop!"))
(hash-set! primitive-list-instance "pop" (primitive-placeholder-named "list-pop"))
(hash-set! primitive-list-instance "push!" (primitive-placeholder-named "list-push!"))
(hash-set! primitive-list-instance "push" (primitive-placeholder-named "list-push"))
(hash-set! primitive-list-instance "shift!" (primitive-placeholder-named "list-shift!"))
(hash-set! primitive-list-instance "shift" (primitive-placeholder-named "list-shift"))
(hash-set! primitive-list-instance "unshift!" (primitive-placeholder-named "list-unshift!"))
(hash-set! primitive-list-instance "unshift" (primitive-placeholder-named "list-unshift"))
(hash-set! primitive-list-instance "index-of" (primitive-placeholder-named "list-index-of"))
(hash-set! primitive-list-instance "last-index-of" (primitive-placeholder-named "list-last-index-of"))
(hash-set! primitive-list-instance "join" (primitive-placeholder-named "list-join"))
(hash-set! primitive-list-instance "sort!" (primitive-placeholder-named "list-sort!"))
(hash-set! primitive-list-instance "sort" (primitive-placeholder-named "list-sort"))
(hash-set! primitive-list-instance "sort!" (primitive-placeholder-named "list-sort!"))
(hash-set! primitive-list-instance "sort" (primitive-placeholder-named "list-sort"))
(hash-set! primitive-list-instance "map!" (primitive-placeholder-named "list-map!"))
(hash-set! primitive-list-instance "map" (primitive-placeholder-named "list-map"))
(hash-set! primitive-list-instance "pluck!" (primitive-placeholder-named "list-pluck!"))
(hash-set! primitive-list-instance "pluck" (primitive-placeholder-named "list-pluck"))
(hash-set! primitive-list-instance "invoke!" (primitive-placeholder-named "list-invoke!"))
(hash-set! primitive-list-instance "invoke" (primitive-placeholder-named "list-invoke"))
(hash-set! primitive-list-instance "filter!" (primitive-placeholder-named "list-filter!"))
(hash-set! primitive-list-instance "filter" (primitive-placeholder-named "list-filter"))
(hash-set! primitive-list-instance "reject!" (primitive-placeholder-named "list-reject!"))
(hash-set! primitive-list-instance "reject" (primitive-placeholder-named "list-reject"))
(hash-set! primitive-list-instance "reduce" (primitive-placeholder-named "list-reduce"))
(hash-set! primitive-list-instance "reduce-right" (primitive-placeholder-named "list-reduce-right"))
(hash-set! primitive-list-instance "every" (primitive-placeholder-named "list-every"))
(hash-set! primitive-list-instance "any" (primitive-placeholder-named "list-any"))
(hash-set! primitive-list-instance "first" (primitive-placeholder-named "list-first"))
(hash-set! primitive-list-instance "first" (primitive-placeholder-named "list-first"))
(hash-set! primitive-list-instance "last" (primitive-placeholder-named "list-last"))
(hash-set! primitive-list-instance "last" (primitive-placeholder-named "list-last"))
(hash-set! primitive-list-instance "take!" (primitive-placeholder-named "list-take!"))
(hash-set! primitive-list-instance "take" (primitive-placeholder-named "list-take"))
(hash-set! primitive-list-instance "take-while!" (primitive-placeholder-named "list-take-while!"))
(hash-set! primitive-list-instance "take-while" (primitive-placeholder-named "list-take-while"))
(hash-set! primitive-list-instance "drop!" (primitive-placeholder-named "list-drop!"))
(hash-set! primitive-list-instance "drop" (primitive-placeholder-named "list-drop"))
(hash-set! primitive-list-instance "drop-while!" (primitive-placeholder-named "list-drop-while!"))
(hash-set! primitive-list-instance "drop-while" (primitive-placeholder-named "list-drop-while"))
(hash-set! primitive-list-instance "union!" (primitive-placeholder-named "list-union!"))
(hash-set! primitive-list-instance "union" (primitive-placeholder-named "list-union"))
(hash-set! primitive-list-instance "intersection!" (primitive-placeholder-named "list-intersection!"))
(hash-set! primitive-list-instance "intersection" (primitive-placeholder-named "list-intersection"))
(hash-set! primitive-list-instance "unique!" (primitive-placeholder-named "list-unique!"))
(hash-set! primitive-list-instance "unique" (primitive-placeholder-named "list-unique"))
(hash-set! primitive-list-instance "difference!" (primitive-placeholder-named "list-difference!"))
(hash-set! primitive-list-instance "difference" (primitive-placeholder-named "list-difference"))
(hash-set! primitive-list-instance "remove!" (primitive-placeholder-named "list-remove!"))
(hash-set! primitive-list-instance "remove" (primitive-placeholder-named "list-remove"))
(hash-set! primitive-list-instance "remove-all!" (primitive-placeholder-named "list-remove-all!"))
(hash-set! primitive-list-instance "remove-all" (primitive-placeholder-named "list-remove-all"))
(hash-set! primitive-list-instance "compact!" (primitive-placeholder-named "list-compact!"))
(hash-set! primitive-list-instance "compact" (primitive-placeholder-named "list-compact"))
(hash-set! primitive-list-instance "shuffle!" (primitive-placeholder-named "list-shuffle!"))
(hash-set! primitive-list-instance "shuffle" (primitive-placeholder-named "list-shuffle"))
(hash-set! primitive-list-instance "reverse!" (primitive-placeholder-named "list-reverse!"))
(hash-set! primitive-list-instance "reverse" (primitive-placeholder-named "list-reverse"))


;;Future

;class (none)

(define primitive-future-instance (make-hash))
(hash-set! primitive-future-instance "class" (create-primitive-funject-attribute (lambda (arg own self)
                                                                                   primitive-future-class)
                                                                                 default-primitive-inverse))
(hash-set! primitive-future-instance "touch" (create-primitive (lambda (arg own)
                                                                 (lang-list-n-contents arg
                                                                                       1
                                                                                       (partial default-primitive-parent arg own)
                                                                                       (lambda (fut)
                                                                                         (invoke primitive-touch fut))))
                                                               default-primitive-inverse))
















;;;;    invoke-<builtin>

;helpers
(define (create-invoke-type type callback)
  (lambda (receiver arg own)
    (lang-contents-typed receiver
                         type
                         (lambda ()
                           (raise-argument-error 'type (string-append "something of the type " (symbol->string type)) receiver))
                         (lambda internals
                           (lang-contents-typed arg
                                                'Symbol
                                                (partial default-primitive-parent arg own)
                                                (lambda (name)
                                                  (apply callback (append (list receiver arg own) internals (list name)))))))))


;number
(define invoke-number
  ((lambda ()
     (define delegate (create-delegate-instance-method-to-class primitive-number-class))
     (lambda (receiver arg own)
       (unless (lang? 'Symbol arg)
               (delegate arg own)
               (lang-contents arg
                              (lambda (str)
                                (cond 
                                  [(equal? str "+") 
                                   (create-primitive-number-+ (mcadr receiver))]
                                  [(equal? str "-") 
                                   (create-primitive-number-- (mcadr receiver))]
                                  [(equal? str "*") 
                                   (create-primitive-number-* (mcadr receiver))]
                                  [(equal? str "/") 
                                   (create-primitive-number-/ (mcadr receiver))]
                                  [else 
                                   (delegate arg own)]))))))))

(define invoke-inverse-number ;takes receiver arg own
  ((lambda ()
     (define delegate (create-delegate-instance-method-inverse-to-class primitive-number-class))
     (lambda (receiver arg own)
       (cond
         [(equal? arg create-primitive-number-+)
          (create-lang-list-mlist (create-lang 'Symbol "+"))]
         [else
          (delegate arg own)])))))

;string
(define invoke-string 
  ((lambda ()
     (define delegate (create-delegate-instance-method-to-class primitive-string-class))
     (create-invoke-type 'String
                         (lambda (receiver arg own internal name)
                           (cond
                             [(eq? name "+") 
                              (create-primitive-string-+ internal)]
                             [else 
                              (delegate arg own)]))))))
  
(define invoke-inverse-string ;takes receiver arg own
  ((lambda ()
     (define delegate (create-delegate-instance-method-inverse-to-class primitive-string-class))
     (lambda (receiver arg own)
       (cond
         [(equal? arg create-primitive-string-+)
          (create-lang-list-mlist (create-lang 'Symbol "+"))]
         [else
          (delegate arg own)])))))

;boolean
(define invoke-boolean
  ((lambda ()
     (define delegate (create-delegate-instance-method-to-class primitive-boolean-class))
     (create-invoke-type 'Boolean
                         (lambda (receiver arg own internal name)
                           (cond
                             [(eq? name "not") 
                              (create-lang 'Boolean (not internal))]
                             [else
                              (delegate arg own)]))))))

(define invoke-inverse-boolean
  ((lambda ()
     (define delegate (create-delegate-instance-method-inverse-to-class primitive-boolean-class))
     (lambda (receiver arg own)
       (cond
         [else
          (delegate arg own)])))))


;symbol
(define invoke-symbol
  ((lambda ()
     (define delegate (create-delegate-instance-method-to-class primitive-symbol-class))
     (create-invoke-type 'Symbol
                         (lambda (receiver arg own internal name)
                           (cond
                             [else
                              (delegate arg own)]))))))

(define invoke-inverse-symbol
  ((lambda ()
     (define delegate (create-delegate-instance-method-inverse-to-class primitive-symbol-class))
     (lambda (receiver arg own)
       (cond
         [else
          (delegate arg own)])))))

;nil
(define invoke-nil
  ((lambda ()
     (define delegate (create-delegate-instance-method-to-class primitive-nil-class))
     (create-invoke-type 'Nil
                         (lambda (receiver arg own name)
                           (cond
                             [(equal? name "nil?") 
                              (create-lang 'Boolean true)]
                             [else
                              (delegate arg own)]))))))

(define invoke-inverse-nil
  ((lambda ()
     (define delegate (create-delegate-instance-method-inverse-to-class primitive-nil-class))
     (lambda (receiver arg own)
       (cond
         [else
          (delegate arg own)])))))

;unknown
(define invoke-unknown
  ((lambda ()
     (define delegate (create-delegate-instance-method-to-class primitive-unknown-class))
     (create-invoke-type 'Unknown
                         (lambda (receiver arg own name)
                           (cond
                             [(equal? name "unknown?") 
                              (create-lang 'Boolean true)]
                             [else
                              (delegate arg own)]))))))

(define invoke-inverse-unknown
  ((lambda ()
     (define delegate (create-delegate-instance-method-inverse-to-class primitive-unknown-class))
     (lambda (receiver arg own)
       (cond
         [else
          (delegate arg own)])))))

;list
(define invoke-list
  ((lambda ()
     (define delegate (create-delegate-instance-method-to-class primitive-list-class))     
     (lambda (receiver arg own)
       (lang-list-contents receiver
                           (lambda ()
                             (raise-argument-error 'type "a list" receiver))
                           (lambda elems
                             (cond
                               [(and (lang? 'Symbol arg)
                                     (lang-contents arg
                                                    (lambda (internal) 
                                                      (eq? "+" internal))))
                                (create-primitive-list-+ elems)]
                               [(and (lang? 'List arg)
                                     (lang-contents arg
                                                    (lambda (maybe-singleton)
                                                      (and (= 1 (mlength maybe-singleton))
                                                           (lang? 'Number (mcar maybe-singleton))))))
                                (lang-list-contents arg
                                                    (lambda () 
                                                      (error "I shouldn't have executed this; I ensured arg was a List!"))
                                                    (lambda (lang-n)
                                                      (lang-contents lang-n
                                                                     (lambda (n)
                                                                       (unless (< n (length elems))
                                                                               (user-error 'index-out-of-range (string-append "I cannot select the "
                                                                                                                              (->string n)
                                                                                                                              "th index of a "
                                                                                                                              (->string (length elems))
                                                                                                                              "element list!"))
                                                                               (list-ref elems n))))))]
                               [else
                                (delegate arg own)])))))))

(define invoke-inverse-list
  ((lambda ()
     (define delegate (create-delegate-instance-method-inverse-to-class primitive-list-class))
     (lambda (receiver arg own)
       (cond
         [else
          (delegate arg own)])))))

;future
(define invoke-future
  ((lambda ()
     (define delegate (create-delegate-instance-method-to-class primitive-future-class))
     (create-invoke-type 'Future
                         (lambda (receiver arg own internal name)
                           (cond
                             [(equal? name "touch") 
                              (invoke primitive-touch own)]
                             [else
                              (delegate arg own)]))))))

(define invoke-inverse-future
  ((lambda ()
     (define delegate (create-delegate-instance-method-inverse-to-class primitive-future-class))
     (lambda (receiver arg own)
       (cond
         [else
          (delegate arg own)])))))




                                 





;;;private
(define (env-pairs-for-private-of funject)
  (env-pairs (create-env-pair-strict "private" (private-of funject))))
(define privates (env-create (env-pairs)))
(define (private-of funject) (env-get funject privates))


(define global-env (env-create (env-pairs (create-env-pair-strict "Funject" primitive-funject-class)
                                          (create-env-pair-strict "Number" primitive-number-class)
                                          (create-env-pair-strict "String" primitive-string-class)
                                          (create-env-pair-strict "Boolean" primitive-boolean-class)
                                          (create-env-pair-strict "Symbol" primitive-symbol-class)
                                          (create-env-pair-strict "Nil" primitive-nil-class)
                                          (create-env-pair-strict "Unknown" primitive-unknown-class)
                                          (create-env-pair-strict "List" primitive-list-class)
                                          (create-env-pair-strict "Class" primitive-class-class)
                                          (create-env-pair-strict "Future" primitive-future-class)
                                          (create-env-pair-strict "+" (create-lang 'Symbol "+"))
                                          (create-env-pair-strict "-" (create-lang 'Symbol "-"))
                                          (create-env-pair-strict "*" (create-lang 'Symbol "*"))
                                          (create-env-pair-strict "/" (create-lang 'Symbol "/"))
                                          (create-env-pair-strict "is" (create-lang 'Symbol "is"))
                                          (create-env-pair-strict "isnt" (create-lang 'Symbol "isnt"))
                                          (create-env-pair-strict "and" (create-lang 'Symbol "and"))
                                          (create-env-pair-strict "or" (create-lang 'Symbol "or"))
                                          (create-env-pair-strict "==" (create-lang 'Symbol "=="))
                                          (create-env-pair-strict "print" primitive-print)
                                          (create-env-pair-strict "identity" (bridge identity))
                                          (create-env-pair-strict "by" primitive-by)
                                          (create-env-pair-strict "future" primitive-future)
                                          (create-env-pair-strict "touch" primitive-touch))))

;I placed these down here because they rely on the global enviroment.
;This serves as default Klass.instance.initialize.
(define default-class-initialize 
  (create-lang 'Funject
               (gen-funject-id)
               (funject-pairs (bind-funject-pair (create-funject-pair (tokenize 'List-literal
                                                                                (mlist (tokenize 'Parameter 
                                                                                                 "@self")
                                                                                       (tokenize 'Parameter
                                                                                                 "@arg")))
                                                                      (mlist 'Unevaled 
                                                                             (lambda (env)
                                                                               lang-nil)))
                                                 global-env))
               primitive-funject-god
               primitive-funject-inverse-god))
(define lang-identity (create-lang 'Funject
                                   (gen-funject-id)
                                   (mlist (bind-funject-pair (create-funject-pair (tokenize 'Parameter "@other")
                                                                                  (mlist 'Unevaled (partial lookup-identifier "@other")))
                                                             global-env))
                                   primitive-funject-god
                                   primitive-funject-inverse-god))


;;;;    user-error

(struct lang-error 
  (name contents) 
  #:mutable 
  #:transparent 
  #:constructor-name create-lang-error
  #:property prop:custom-write
             (lambda (e out w?)
                 (write (lang-error->string e)
                        out)))

(define (lang-error->string e)
  (let ((contents (lang-error-contents e)))
    (string-append " ! error: "
                   (foldl (lambda (str rest)
                            (string-append rest 
                                           "" 
                                           (if (string? str)
                                               str
                                               (->string str))))
                          ""
                          contents))))

(define default-exception-handler (uncaught-exception-handler))
       

(define (user-error name . args)
  (raise (create-lang-error name args)))

(define (user-error-cannot-find-variable var)
  (user-error 'cannot-find-variable "I cannot find the variable \"" var "\"!"))

(define (user-error-cannot-find-match . args)
  (cond
    [(empty? args)
     (user-error 'cannot-find-match "I find no matching pattern!")]
    [(= 1 (length args))
     (user-error 'cannot-find-match "I find no matching pattern in the funject " (car args) "!")]
    [else
     (apply (lambda (receiver arg)
              (user-error 'cannot-find-match "I find no pattern matching " arg " in " receiver))
            args)]))

(define (lang-error-cannot-find-match? ex)
  (and (lang-error? ex)
       (equal? 'cannot-find-match (lang-error-name ex))))

(define (user-error-cannot-reset-unset-variable var) (user-error 'cannot-reset-unset-variable "You tried to reset the variable " var ", but you haven't even assigned it yet!"))

(define (user-error-cannot-push-pair-to-non-funject) (user-error 'cannot-push-pair-to-non-funject "I cannot alter the patterns of a non-funject!"))

(define (user-error-cannot-set-parent-of-non-funject) (user-error 'cannot-set-parent-of-non-funject "I cannot set the parent of a non-funject!"))

(define (user-error-set-inverse-of-non-funject) (user-error 'set-inverse-of-non-funject "I cannot set the inverse of a non-funject!"))

(define (user-error-funject-pattern-cannot-contain pattern) (user-error 'funject-pattern-cannot-contain "A funject pattern cannot contain a funject of the type of " pattern))

(define (user-error-multiple-unknowns-in-pattern-arg) (user-error 'multiple-unknowns-in-pattern-arg "A funject pattern cannot contain multiple unknown matching variables!")) 

(define (user-error-condition-not-boolean) (user-error 'condition-not-boolean "You may only pass a Boolean to the condition of an if-statement!"))

(define (user-error-type actor expected actual) (user-error 'type (string-append actor
                                                                                 ": I expected a "
                                                                                 expected
                                                                                 " but you passed me this: "
                                                                                 (->string actual))))

(define (user-assert condition result)
  (cond 
    [condition 'ok]
    [(string? result) (user-error 'assertion-failure result)]
    [(procedure? result) (result)]
    [else (error "user-assert: use me this way: (user-assert boolean? (or procedure? string?))")]))
  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                Behold:
(define p1 (compose car stream-first parse))
(define (interpret-parsed exps)
  (mmap (lambda (exp)
          (eval exp global-env))
        (mmap analyze exps)))
(define (interpret str)
  (try
   (interpret-parsed (deep-list->mlist (p1 str)))
   (catch 
       (lambda (ex)
         (if (lang-error? ex)
             (raise-user-error (lang-error->string ex))
             (raise ex))))))


 
































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                Testing

;;;begin not translating
(define v deep-mlist->list)
(define (t str) (deep-list->mlist (p1 str)))
(define i interpret)
(define j (compose deep-mlist->list i))
;;;;end not translating
































;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                Executable interface

#|(let ((argv (current-command-line-arguments)))
  (unbridge (mlast (cond
                     [(= 0 (vector-length argv))
                      (interpret (port->string (current-input-port)))]
                     [(and (= 1 (vector-length argv))
                           (equal? "-p" (vector-ref argv 0)))
                      (interpret-parsed (deep-list->mlist (read (current-input-port))))]
                     [else
                      (vector-map (lambda (path)
                                    (interpret (file->string path)))
                                  argv)]))))|#