#lang racket
(define occurs-free?
  (lambda (val exp)
    (cond
      ((symbol? exp) (eqv? val exp))
      ((eqv? (car exp) 'lambda) 
       (and
        (not (eqv? (caadr exp) val))
        (occurs-free? val (caddr exp))))
      (else
       (or (occurs-free? val (car exp))
           (occurs-free? val (cadr exp)))))))

(occurs-free? 'x 'x)
(occurs-free? 'x 'y)
(occurs-free? 'x '(lambda (x) (x y)))
(occurs-free? 'x '(lambda (x) (z y)))
(occurs-free? 'x '(lambda (y) (x y)))
(occurs-free? 'x '((lambda (x) x) (x y)))
(occurs-free? 'x '(lambda (y) (lambda (z) (x (y z)))))