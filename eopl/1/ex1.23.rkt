#lang racket
(define list-index
 (lambda (pred lst)
   (local
     [(define go
        (lambda (lst n)
          (if (null? lst)
              #f
              (if (pred (car lst))
                  n
                  (go (cdr lst) (+ n 1))))))]
     (go lst 0))))

(list-index number? '(a 2 (1 3) b 7))
(list-index symbol? '(a (b c) 17 foo))
(list-index symbol? '(1 2 (a b) 3))