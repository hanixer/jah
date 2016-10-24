#lang racket

(define zero (lambda () 0))
(define is-zero? zero?)
(define predecessor (lambda (n) (- n 1)))
(define successor   (lambda (n) (+ n 1)))

(provide (all-defined-out))