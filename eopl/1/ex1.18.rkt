#lang racket
(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist) 
        '()
        (cons 
         (if (eqv? (car slist) s2) s1 (car slist))
         (swapper s1 s2 (cdr slist))))))

(swapper 'a 'd '(a b c d))
(swapper 'a 'd '(a d () c d))
(swapper 'x 'y '((x) y (z (x))))