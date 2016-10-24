#lang racket

(define small-bst
  '(100
    (50
     (25 () ())
     (70 () ()))
    (150
     (125 () ())
     (6000  
      (1000 () (1500 () ()))
      ()))))

(define number-leaves-helper
  (lambda (bst counter)
    (if (null? bst)
        (cons counter '())
        (local 
          [(define lresult (number-leaves-helper (cadr bst) (+ counter 1)))
          (define rresult  (number-leaves-helper (caddr bst) (car lresult)))]
          (list (car rresult) (+ counter 1) (cdr lresult) (cdr rresult)))))) 
          

(define number-leaves
  (lambda (bst)
    (cdr (number-leaves-helper bst 0))))

(number-leaves small-bst)
'(1
  (2
   (3 () ())
   (4 () ()))
  (5
   (6 () ())
   (7
    (8 ()
       (9 () ())) ())))