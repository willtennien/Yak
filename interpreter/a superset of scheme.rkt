#lang racket
(define-syntax-rule (funject ((arg ...) 
                              conseq)
                             ...)
  (lambda args
    (match args
      ((list arg ...)
       conseq)
      ...)))

;For example:
((funject 
  [() 0]
  [(a) 1]
  [(a b) 2]
  [(a b c) 3]) 'a 'b)