#lang racket
(define subst-single
  (lambda (new old cur)
    (if (eqv? cur old) new
        cur)))

(define subst
  (lambda (new old slist)
    (if (null? slist) slist
        (if (symbol? (car slist)) 
            (cons (subst-single new old (car slist)) (subst new old (cdr slist)))
            (cons (subst new old (car slist)) (subst new old (cdr slist)))))))

(define subst-map-helper
  (lambda (new old sexp)
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst-map new old sexp))))

(define subst-map
  (lambda (new old slist)
    (local
      [(define subst-map-helper
        (lambda (sexp)
          (if (symbol? sexp)
              (if (eqv? sexp old) new sexp)
              (subst-map new old sexp))))]
      
      (map subst-map-helper slist))))

(subst 'a 'b '((b c) (b () d)))
(subst-map 'a 'b '((b c) (b () d)))