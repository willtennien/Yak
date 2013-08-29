#lang racket
(require racket/match)

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

(define token%
  (class object%
    (init source ->string)
    (define src source)
    (define ->str ->string)
    (super-new)
    (define/public (get-source)
      src)
    (define/public (get->string)
      ->str)))

(define (literal-token str)
  (class token%
    (init)
    (super-new [source str]
               [->string str])))

(define (generate-token-identity%)
  (class token%
    (init source)
    (super-new [source source]
               [->string source])))

;(define (token-brace-open) ...)
(define token-brace-open% (literal-token "{"))
(define (token-brace-open)
  (new token-brace-open%))

;(define (token-brace-close) ...)
(define token-brace-close% (literal-token "}"))
(define (token-brace-close)
  (new token-brace-close%))

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
(define (generate-token-keyword% keyword)
  (class token%
    (init)
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
       (define (name xs . rest)
         (cond 
           [(or (and (string? xs)
                     (= 0 
                        (string-length xs)))
                (and (list? xs)
                     (= 0
                        (length xs))))
            '()]
           [else
            (define (iter patterns)
              (cond 
                [(empty? patterns)
                 (error "I cannot match " xs)]
                [else
                 (or (apply (car patterns) (cons xs rest))
                     (iter (cdr patterns)))]))
            (iter patterns)]))
       name))))

;Precondition: all patterns receive a string of at least one character and a list of at least one indent.
(define tokenize
  ((lambda ()
     (define-patterns tokenize-with-indents
       ((str indents)
        (if (not (equal? "\n" (substring str 0 1)))
            #f
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
        (define pattern #px"^[^\n]+")
        (cons (token-other (car (regexp-match pattern str)))
              (tokenize-with-indents (substring str 
                                                (cdar (regexp-match-positions pattern str)))
                                     indents))))
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

(define transform
  (funject
   [((cons (token-indent) rest))
    (cons (token-brace-open)
          (cons (token-newline)
                (transform rest)))]
   [((cons (token-outdent) rest))
    (cons (token-brace-close)
          (cons (token-newline)
                (transform rest)))]
   [((cons (token-newline) rest))
    (if (or (token-indent? (car rest)))
        (cons (token-newline)
              (transform rest))
        (cons (token-semicolon)
              (cons (token-newline)
                    (transform rest))))]
   [((cons other rest))
    (cons other
          (transform rest))]
   [(empty)
    '()]))

(define (token->string token)
  (cond
    [(token-brace-open? token) ; replace with is-a?
     "{"]
    [(token-brace-close? token) 
     "}"]
    [(token-newline? token) 
     "\n"]
    [(token-indent? token) 
     "{"]
    [(token-outdent? token) 
     "}"]
    [(token-space? token)
     (token-space-source token)]
    [(token-semicolon? token)
     ";"]
    [(token-keyword? token)
     (string-append " "
                    (token-keyword-name token)
                    " ")]
    [(token-other? token)
     (token-other-source token)]))

(define (parse str)
  (let ((tokens (tokenize str)))
    (transform tokens)))

(define (compile str)
  (foldl (lambda (a b)
           (string-append b a))
         ""
         (map token->string
              (parse str))))