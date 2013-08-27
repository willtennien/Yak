#lang racket
(define-syntax-rule (list-contents xs f)
  (apply (lambda . f) xs))

(define-syntax self
  (syntax-id-rules ()
    [self this]))

(define-syntax self%
  (syntax-id-rules ()
    [self% this%]))

;(define (token-newline) ...)
(struct token-newline () #:transparent)

;(define (token-space source) ...)
(struct token-space (source) #:transparent)

;(define (token-indent) ...)
(struct token-indent () #:transparent)

;(define (token-outdent) ...)
(struct token-outdent () #:transparent)

;(define (token-other source) ...)
(struct token-other (source) #:transparent)

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

(define (parse str)
  (let ((tokens (tokenize str)))
    tokens))