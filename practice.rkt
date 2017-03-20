#lang racket

(define fact/k
  (lambda (n k)
    (cond
      (( zero? n) (call/cc (lambda (f) (k f))))
      (else (* n (fact/k (sub1 n) k)))
      )
    ))

(define five (call/cc (lambda (k) (fact/k 5 k))))

