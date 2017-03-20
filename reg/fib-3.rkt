#lang racket
;; add apply-k
(define fib-cps
  (λ (n k)
    (cond
      ((< n 2) (apply-k k n))
      (else
       (fib-cps (- n 1)
                (λ (v)
                  (fib-cps (- n 2)
                           (λ (w)
                             (apply-k k (+ v w))))))))))

(define apply-k
  (λ (k v)
    (k v)))

(fib-cps 5 (λ (v) v))

