#lang racket


(define *bignum-base* 16)

(define zero-bignum
  (lambda (base)
    (lambda () (list 0))))

(define is-zero?-bignum
  (lambda (base)
    (lambda (n) (= (car n) 0))))

(define predecessor-bignum
  (lambda (base)
    (local
      [(define go
         (lambda (n)
           (cond
             [(null? n) '()]
             [(zero? (car n)) (cons (- base 1) (go (cdr n)))]
             [(= (car n) 1) '()]
             [else (cons (- (car n) 1) (cdr n))])))]
      go)))

(define successor-bignum
  (lambda (base)
    (local
      [(define go
         (lambda (n)
           (if (null? n)
               (list 1)
               (if (< (car n) (- base 1)) 
                   (cons (+ (car n) 1) (cdr n))
                   (cons 0 (go (cdr n)))))))]
      go)))

(define zero (zero-bignum *bignum-base*))
(define is-zero? (is-zero?-bignum *bignum-base*))
(define predecessor (predecessor-bignum *bignum-base*))
(define successor (successor-bignum *bignum-base*))

(provide (all-defined-out))