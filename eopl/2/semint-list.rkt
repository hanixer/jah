#lang racket

(define zero (lambda () '()))
(define is-zero? null?)
(define predecessor cdr)
(define successor (lambda (n) (cons #t n)))

(provide (all-defined-out))