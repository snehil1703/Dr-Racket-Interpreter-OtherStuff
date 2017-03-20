#lang racket
;; add make continuations RI
(define fib-cps
  (λ (n k)
    (cond
      ((< n 2) (apply-k k n))
      (else
       (fib-cps (- n 1) (fib-cps-outer-k n k))))))

(define fib-cps-inner-k
  (λ (v k)
    (λ (w)
      (apply-k k (+ v w)))))

(define fib-cps-outer-k
  (λ (n k)
    (λ (v)
      (fib-cps (- n 2)
               (fib-cps-inner-k v k)))))

(define apply-k
  (λ (k v)
    (k v)))

(define empty-k
  (λ ()
    (λ (v) 
      v)))

(fib-cps 5 (empty-k))

