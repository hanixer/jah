#lang racket
(define invert-pair
  (lambda (p)
    (cons (cadr p) (list (car p)))))

(define invert
  (lambda (lst) 
    (map invert-pair lst)))
    
(invert '((a 1) (a 2) (1 b) (2 b)))


    