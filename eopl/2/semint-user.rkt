#lang racket

(require "semint-bignum.rkt")

(define nth-number
  (lambda (nth)
    (local
      [(define go
         (lambda (prev counter)
           (if (= counter 0)
               prev
               (go (successor prev) (- counter 1)))))]
      (go (zero) nth))))

(nth-number 0)
(nth-number 1)
(nth-number 2)
(nth-number 6)
(nth-number 10)
(nth-number 100)
(nth-number 15378)
(nth-number 255)
(nth-number 65536)
;(nth-number 720)

(predecessor (nth-number 5))
(predecessor (nth-number 100))
(predecessor (nth-number 65536))
(predecessor (nth-number 3000))
