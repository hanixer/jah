#lang racket
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            #t
            (exists? pred (cdr lst))))))


(exists? number? '(a b c 3 e))
(exists? number? '(a b c d e))