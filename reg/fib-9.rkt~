#lang racket
;; create global variables, set!s out the wazoo

(define n 'hukarz)
(define k 'hukarz)
(define v 'hukarz)

(define empty-k
  (λ ()
    `(empty-k)))

(define fib-cps-inner-k
  (λ (v^ k^)
    `(fib-cps-inner-k ,v^ ,k^)))

(define fib-cps-outer-k
  (λ (n^ k^)
    `(fib-cps-outer-k ,n^ ,k^)))

(define fib-cps
  (λ () ;; n k
    (cond
      ((< n 2)
       (begin 
         (set! v n)
         (apply-k)))
      (else 
       (begin 
         (set! k (fib-cps-outer-k n k))
         (set! n (- n 1))
         (fib-cps))))))

(define apply-k
  (λ () ;; k v
    (match k
      (`(empty-k) v)
      (`(fib-cps-inner-k ,v^ ,k^)
       (begin 
         (set! k k^)
         (set! v (+ v^ v))
         (apply-k)))
      (`(fib-cps-outer-k ,n^ ,k^)
       (begin 
         (set! k (fib-cps-inner-k v k^))
         (set! n (- n^ 2))
         (fib-cps))))))

(begin 
  (set! k (empty-k))
  (set! n 5)
  (fib-cps))

