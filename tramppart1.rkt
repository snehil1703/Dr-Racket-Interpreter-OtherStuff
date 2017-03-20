#lang racket

;; (define fib
;;   (λ (n)
;;     (cond
;;       ((< n 2) n)
;;       (else
;;        (+ (fib (- n 1))
;;           (fib (- n 2)))))))

;; (fib 5)

(define fib-outer-k
  (λ (n k)
    `(fib-outer-k ,n^ ,k^)))

(define fib-inner-k
  (λ (v k)
    `(fib-inner-k ,v^ ,k^)))

(define empty-k 
  (λ ()
    `(empty-k)))

(define apply-k
  (λ (k v)
    (match k
      [`(empty-k) v]
      (`(fib-outer-k ,n^ ,k^)
       (fib-cps 
        (- n^ 2)
        (fib-inner-k v k^)))
      (`(fib-inner-k ,v^ ,k^)
       (apply-k k^ (+ v^ v))))))

(define fib-cps
  (λ (n k)
    (cond
      ((< n 2) (apply-k k n))
      (else
       (fib-cps 
        (- n 1)
        (fib-outer-k n k))))))

(fib-cps 5 (empty-k))
