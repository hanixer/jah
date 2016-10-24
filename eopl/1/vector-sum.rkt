#lang racket
(define vector-partial-sum
  (lambda (v n)
    (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n) 
           (vector-partial-sum v (- n 1))))))

(define vector-sum
  (lambda (v)
    (let ((n (vector-length v)))
      (if (zero? n) 
          0
          (vector-partial-sum v (- n 1))))))


(= (vector-sum #(1 2 3 4 23 2)) (+ 1 2 3 4 23 2))