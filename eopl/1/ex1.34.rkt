#lang racket
(define bst
  (lambda (n l r)
    (list n l r)))

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

(define path
  (lambda (n bst)
    (define go
      (lambda (bst path-so-far)
        (cond
          [(null? bst) '()]
          [(= (car bst) n) (cons 'here path-so-far)]
          [(> (car bst) n) (go (cadr bst) (cons 'left path-so-far ))]
          [else            (go (caddr bst) (cons 'right path-so-far))])))
    (reverse (go bst '()))))

(path 100 small-bst)
(path 1 small-bst)
(path 6000 small-bst)
(path 1023 small-bst)
(path 1500 small-bst)