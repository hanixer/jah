#lang racket
(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f))))

(every? number? '(a b c 3 e))
(every? number? '(1 2 3 5 4))