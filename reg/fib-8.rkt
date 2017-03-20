#lang racket
;; let* for a-normal form

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
  (λ (n k)
    (cond
      ((< n 2) (let* ((v n))
                 (apply-k k v)))
      (else 
       (let* ((k (fib-cps-outer-k n k))
              (n (- n 1)))
         (fib-cps n k))))))

(define apply-k
  (λ (k v)
    (match k
      (`(empty-k) v)
      (`(fib-cps-inner-k ,v^ ,k^)
       (let* ((k k^)
              (v (+ v^ v)))
         (apply-k k v)))
      (`(fib-cps-outer-k ,n^ ,k^)
       (let* ((k (fib-cps-inner-k v k^))
              (n (- n^ 2)))
        (fib-cps n k))))))

(let* ((k (empty-k))
       (n 5)) 
  (fib-cps n k))

