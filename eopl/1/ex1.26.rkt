#lang racket
(define flatten-sym
  (lambda (s)
    (if (symbol? s)
        s
        (flatten s))))

(define flatten
  (lambda (slist)
    (local
      [(define go
         (lambda (slist sublst)
           (if (null? sublst)
               (cond 
                 ((null? slist) '())
                 ((symbol? (car slist)) 
                  (cons (car slist) (go (cdr slist) '())))
                 (else
                  (go (cdr slist) (flatten (car slist)))))
               (cons (car sublst) (go slist (cdr sublst))))))]
      (go slist '()))))
                   

(flatten '(a b c))
(flatten '((a) () (b ()) () (c)))
(flatten '((a b) c (((d)) e)))
(flatten '(a b (() (c))))
(flatten '(a (b ((()))) ((())) (c (d (e (fer (gar (re (rer (R(rer)))))))))))