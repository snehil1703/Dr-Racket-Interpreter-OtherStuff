#lang racket
;; CPS
(define fib-cps
  (λ (n k)
    (cond
      ((< n 2) (k n))
      (else
       (fib-cps (- n 1)
                (λ (v)
                  (fib-cps (- n 2)
                           (λ (w) (k (+ v w))))))))))

(fib-cps 5 (λ (v) v))

