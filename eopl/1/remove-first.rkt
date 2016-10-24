#lang racket
(define remove-first
  (lambda (val lst)
    (if (null? lst) lst
        (if (eqv? val (car lst))  (cdr lst)
            (cons (car lst) (remove-first val (cdr lst)))))))

(remove-first 'a '(a b c))
(remove-first 'z '(x y w))
(remove-first 'r '(t tr e r trr r r))
(remove-first 'x '())

        