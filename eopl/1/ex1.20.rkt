#lang racket

(define count-occurrences
  (lambda (s slist)
    (local
      [(define count-in-sexp
         (lambda (sexp n)
           (if (symbol? sexp)
               (if (eqv? sexp s)
                   (+ n 1)
                   n)
               (count-occurrences s sexp))))]
      (foldl count-in-sexp 0 slist))))

(count-occurrences 'x '((f x) y (((x z) x))))
(count-occurrences 'x '((f x) y (((x z) () x))))
(count-occurrences 'w '((f x) y (((x z) x))))