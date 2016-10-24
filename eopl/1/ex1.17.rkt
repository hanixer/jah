#lang racket
(define wrap-element
  (lambda (el)
    (list el)))

(define down
  (lambda (lst)
    (map wrap-element lst)))

(down '(1 2 3))
(down '((a) (fine) (idea)))
(down '(a (more (complicated)) object))

(define up-helper
  (lambda (lst sublst)
    (if (null? sublst)
        (if (null? lst)
            '()
            (if (list? (car lst))
                (up-helper (cdr lst) (car lst))
                (cons (car lst) (up-helper (cdr lst) '()))))
        (cons (car sublst) (up-helper lst (cdr sublst))))))

(define up
  (lambda (lst)
    (up-helper lst '())))

(up '((1 2) (3 4)))
(up '((x (y)) z))
(define lst '(1 2 34 (342 (3423 fj a e)) k krug?))
(up (down lst))