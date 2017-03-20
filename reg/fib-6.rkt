#lang racket
;; make continuations data structures

(define fib-cps
  (λ (n k)
    (cond
      ((< n 2) (apply-k k n))
      (else
       (fib-cps (- n 1) (fib-cps-outer-k n k))))))

(define empty-k
  (λ ()
    `(empty-k)))

(define fib-cps-inner-k
  (λ (v^ k^)
    `(fib-cps-inner-k ,v^ ,k^)))

(define fib-cps-outer-k
  (λ (n^ k^)
    `(fib-cps-outer-k ,n^ ,k^)))

(define apply-k
  (λ (k v)
    (match k
      (`(empty-k) v)
      (`(fib-cps-inner-k ,v^ ,k^)
       (apply-k k^ (+ v^ v)))
      (`(fib-cps-outer-k ,n^ ,k^)
       (fib-cps (- n^ 2)
                (fib-cps-inner-k v k^))))))


(fib-cps 5 (empty-k))

