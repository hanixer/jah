#lang racket
(define scalar-product
  (lambda (s sos)
    (map (lambda (elem) (list s elem)) sos)))

(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (cons (scalar-product (car sos1) sos2) (product (cdr sos1) sos2) ))))

(product '(a b c) '(x y))

(define product2
  (lambda (sos1 sos2)
    (local
      [(define hlpr
         (lambda (cur1 cur2)
           (if (null? cur1)
               '()
               (if (null? cur2)
                   (hlpr (cdr cur1) sos2)
                   (cons
                    (list (car cur1) (car cur2))
                    (hlpr cur1 (cdr cur2)))))))]
      (hlpr sos1 sos2))))

(product2 '(a b c) '(x y))