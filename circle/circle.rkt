#lang racket
(require racket/match)
(require racket/list)

(define-syntax-rule (list-contents xs f)
  (apply (lambda . f) xs))

(define-syntax self
  (syntax-id-rules ()
    [self this]))

(define-syntax self%
  (syntax-id-rules ()
    [self% this%]))

(define-syntax to-conses
  (syntax-rules ()
    [(to-conses ()) 
     '()]
    [(to-conses (a . b))
     (cons a 
           (to-conses b))]
    [(to-conses a) 
     a]))

(define-syntax-rule (funject ((arg ...) 
                              conseq)
                             ...)
  (lambda args
    (match args
      ((list arg ...)
       conseq)
      ...)))

(define (takef f xs)
  (cond
    [(empty? xs)
     '()]
    [(f (car xs))
     (cons (car xs)
           (takef f (cdr xs)))]
    [else
     '()]))

(define (dropf f xs)
  (cond
    [(empty? xs)
     '()]
    [(f (car xs))
     (dropf f (cdr xs))]
    [else
     xs]))

(define isnt-a? (compose not is-a?))

(define (tokens-are? tokens klasses)
  (cond 
    [(or (empty? tokens)
         (empty? klasses))
     (and (empty? tokens)
          (empty? klasses))]
    [(is-a? (car tokens)
            (car klasses))
     (tokens-are? (cdr tokens)
                  (cdr klasses))]
    [else
     #f]))

(define token%
  (class object%
    (init source ->string)
    (define src source)
    (define ->str ->string)
    (super-new)
    (define/public (get-source)
      src)
    (define/public (token->string)
      ->str)
    (define/public (type)
      'token)))

(define (literal-token str)
  (class token%
    (init)
    (define/override (type)
      (string->symbol (string-append "token-"
                                     (if (equal? str "\n") 
                                         "\\n" 
                                         str))))
    (super-new [source str]
               [->string str])))

(define (generate-token-identity%)
  (class token%
    (init source)
    (super-new [source source]
               [->string source])))

(define-syntax-rule (sqr-matrix (x ...) ...)
  (list (list x ...) ...))

;(define (token-brace-open) ...)
(define token-brace-open% (literal-token "{"))
(define (token-brace-open)
  (new token-brace-open%))

;(define (token-brace-close) ...)
(define token-brace-close% (literal-token "}"))
(define (token-brace-close)
  (new token-brace-close%))

;(define (token-paren-open) ...)
(define token-paren-open% (literal-token "("))
(define (token-paren-open)
  (new token-paren-open%))

;(define (token-paren-close) ...)
(define token-paren-close% (literal-token ")"))
(define (token-paren-close)
  (new token-paren-close%))

;(define (token-semicolon) ...)
(define token-semicolon% (literal-token ";"))
(define (token-semicolon)
  (new token-semicolon%))

;(define (token-newline) ...)
(define token-newline% (literal-token "\n"))
(define (token-newline)
  (new token-newline%))

;(define (token-space source) ...)
(define token-space% (generate-token-identity%))
(define (token-space source)
  (new token-space% [source source]))

;(define (token-indent) ...)
(define token-indent%
  (class token%
    (init)
    (super-new [source ""]
               [->string "{"])))
(define (token-indent)
  (new token-indent%))

;(define (token-outdent) ...)
(define token-outdent%
  (class token%
    (init)
    (super-new [source ""]
               [->string "}"])))
(define (token-outdent)
  (new token-outdent%))

;(define (token-other source) ...)
(define token-other% (generate-token-identity%))
(define (token-other source)
  (new token-other% [source source]))

;(define (token-<keyword> source) ...) for <keyword> in (if else unless ...)
(define token-keyword%
  (class token%
    (super-new)
    (define/override (type)
      'token-keyword)))
(define (generate-token-keyword% keyword)
  (class token-keyword%
    (init)
    (define/override (type)
      (string->symbol (string-append "token-"
                                     keyword)))
    (super-new [source keyword]
               [->string (string-append " "
                                        keyword
                                        " ")])))

(define token-if% (generate-token-keyword% "if"))
(define (token-if) (new token-if%))
(define token-unless% (generate-token-keyword% "unless"))
(define (token-unless) (new token-unless%))
(define token-else% (generate-token-keyword% "else"))
(define (token-else) (new token-else%))
(define token-while% (generate-token-keyword% "while"))
(define (token-while) (new token-while%))
(define token-until% (generate-token-keyword% "until"))
(define (token-until) (new token-until%))
(define token-for% (generate-token-keyword% "for"))
(define (token-for) (new token-for%))

(define (token-keyword-positive? x)
  (ormap (lambda (klass)
           (is-a? x klass))
         (list token-if%
               token-while%
               token-for%)))

(define (token-except-newline? token)
     (isnt-a? token
              token-newline%))
                                

;(define-syntax-rule (define-token-keyword name keyword)
;  (begin (struct name token-keyword () #:transparent #:constructor-name foo)
;         (define (bar)
;            (foo keyword))))

;(define-token-keyword token-if "if")
;(define-token-keyword token-else "else")
;(define-token-keyword token-until "until")
;(define-token-keyword token-unless "unless")
;(define-token-keyword token-while "while")
;(define-token-keyword token-for "for")












(define-syntax-rule (define-patterns name 
                      (args body 
                            ...) 
                      ...)
  (define name 
    ((lambda ()
       (define patterns
         (list (lambda args 
                 body 
                 ...) 
               ...))
       (define (name xs . tail)
         (define (iter patterns)
           (cond 
             [(empty? patterns)
              (error "I cannot match " xs)]
             [else
              (or (apply (car patterns) (cons xs tail))
                  (iter (cdr patterns)))]))
         (iter patterns))
       name))))

;Precondition: all patterns receive a string of at least one character and a list of at least one indent.
(define tokenize
  ((lambda ()
     (define-patterns tokenize-with-indents
       ((str indents)
        (and (= 0 (string-length str))
             '()))
       ((str indents)
        (and (equal? "\n" (substring str 0 1))
             (list-contents (try-dent (substring str 1) indents)
                            ((dents str indents)
                             (append (list (token-newline))
                                     dents
                                     (tokenize-with-indents str indents))))))
       ((str indents)
        (define pattern #px"^ +")
        (let ((matches (regexp-match pattern str)))
          (and matches
               (cons (token-space (car matches))
                     (tokenize-with-indents (substring str 
                                                       (cdar (regexp-match-positions pattern
                                                                                     str)))
                                            indents)))))
       ((str indents)
        (define (iter keywords)
          (and (not (empty? keywords))
               (list-contents (car keywords)
                              ((pattern conseq)
                                (if (regexp-match pattern str)
                                    (cons (conseq)
                                          (tokenize-with-indents (substring str
                                                                            (cdar (regexp-match-positions pattern str)))
                                                                 indents))
                                    (iter (cdr keywords)))))))
        (iter (sqr-matrix 
               [#px"^if\\W" token-if]
               [#px"^else\\W" token-else]
               [#px"^unless\\W" token-unless]
               [#px"^while\\W" token-while]
               [#px"^until\\W" token-until]
               [#px"^for\\W" token-for])))
       ((str indents)
        (define pattern #px"^(.(?!\\Wif\\W|\\Welse\\W|\\Wunless\\W|\\Wwhile\\W|\\Wuntil\\W|\\Wfor\\W|\n| ))*.")
        (cons (token-other (car (regexp-match pattern str)))
              (tokenize-with-indents (substring str 
                                                (cdar (regexp-match-positions pattern str)))
                                     indents)))
       ((str indents)
        (and (= 0 (string-length str))
             '())))
     (lambda (str)
       (tokenize-with-indents str '(""))))))

(define (try-dent str indents)
  (let ((ws (car (regexp-match #px"^ *" str))))
    (cond [(equal? ws (car indents))
           (list (list)
                 str
                 indents)]
          [(< (string-length (car indents))
              (string-length ws))
           (list (list (token-indent))
                 str
                 (cons ws indents))]
          [(< (string-length ws)
              (string-length (car indents)))
           (define (iter str indents) 
             (cond 
               [(= (string-length ws)
                   (string-length (car indents)))
                (list (list)
                      str
                      indents)]
               [(< (string-length ws)
                   (string-length (car indents)))
                (list-contents (iter str (cdr indents))
                               ((outdents str indents)
                                (list (cons (token-outdent)
                                            outdents)
                                      str
                                      indents)))]
               [else
                (error "Syntax error: inconsistent indentation.")]))
           (iter str indents)]
          [else
           (error "Logic failed.")])))

(define-patterns transform
  [(tokens)
   (and (empty? tokens)
        '())]
  [(tokens)
   (and (or (and (<= 3 (length tokens))
                 (tokens-are? (take tokens 3)
                              (list token-newline%
                                    token-newline%
                                    token-indent%)))
            (and (<= 4 (length tokens))
                 (tokens-are? (take tokens 4)
                              (list token-newline%
                                    token-space%
                                    token-newline%
                                    token-indent%)))
            (and (<= 3 (length tokens))
                 (tokens-are? (take tokens 3)
                              (list token-outdent%
                                    token-newline%
                                    token-indent%))))
        (raise-user-error "Syntax error: inconsistent indentation."))]
  [(tokens)
   (and (is-a? (car tokens)
               token-unless%)
        (append (list (token-if)
                      (token-paren-open)
                      (token-other "! "))
                (takef token-except-newline? 
                       (cdr tokens))
                (cons (token-paren-close)
                      (transform (dropf token-except-newline? 
                                        tokens)))))]
  [(tokens)
   (and (is-a? (car tokens)
               token-until%)
        (append (list (token-while)
                      (token-paren-open)
                      (token-other "! "))
                (takef token-except-newline? 
                       (cdr tokens))
                (cons (token-paren-close)
                      (transform (dropf token-except-newline? 
                                        tokens)))))]
  [(tokens)
   (and (token-keyword-positive? (car tokens))
        (append (list (car tokens)
                      (token-paren-open))
                (takef token-except-newline? 
                       (cdr tokens))
                (cons (token-paren-close)
                      (transform (dropf token-except-newline? 
                                        tokens)))))]
  [(tokens)
   (and (is-a? (car tokens)
               token-indent%)
        (cons (token-brace-open)
              (cons (token-newline)
                    (transform (cdr tokens)))))]
  [(tokens)
   (and (is-a? (car tokens)
               token-outdent%)
        (cons (token-brace-close)
              (cons (token-newline)
                    (transform (cdr tokens)))))]
  [(tokens)
   (and (is-a? (car tokens)
               token-newline%)
        (let ([tail (cdr tokens)])
          (if (and (not (empty? tail))
                   (is-a? (car tail)
                          token-indent%))
              (cons (token-newline)
                    (transform tail))
              (cons (token-semicolon)
                    (cons (token-newline)
                          (transform tail))))))]
  [(tokens)
   (cons (car tokens)
         (transform (cdr tokens)))])
       

(define (token->string token)
  (send token token->string))

(define (parse str)
  (let ((tokens (tokenize str)))
    (transform tokens)))

(define (compile str)
  (foldl (lambda (a b)
           (string-append b a))
         ""
         (map token->string
              (parse str))))