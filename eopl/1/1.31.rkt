#lang racket

(define leaf
  (lambda (n) n))

(define interior-node
  (lambda (s left right)
    (list s left right)))

(define leaf? integer?)
(define lson cadr)
(define rson caddr)

(define double-tree
  (lambda (t)
    (if (leaf? t)
        (+ t t)
        (interior-node 
         (car t)
         (double-tree (lson t))
         (double-tree (rson t))))))

(define count-with-red
  (lambda (t)
    (local
      [(define go
         (lambda (t depth red-occured)
           (if (leaf? t)
               depth
               (if (eqv? 'red (car t))
                   (interior-node 'red
                                  (go (lson t) 1 #t)
                                  (go (rson t) 1 #t))
                   (local
                     [(define depth-next 
                        (if red-occured (+ depth 1) depth))]
                     (interior-node (car t) 
                                    (go (lson t) depth-next red-occured)
                                    (go (rson t) depth-next red-occured)))))))]
      (go t 0 #f))))

(count-with-red
 (interior-node 'red
                (interior-node 'bar
                               (leaf 26)
                               (leaf 12))
                (interior-node 'red
                               (leaf 11)
                               (interior-node 'quux
                                              (leaf 117)
                                              (leaf 14)))))