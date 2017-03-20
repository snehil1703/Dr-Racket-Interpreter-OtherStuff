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
  (λ (n^ k^)
    `(fib-outer-k ,n^ ,k^)))

(define fib-inner-k
  (λ (v^ k^)
    `(fib-inner-k ,v^ ,k^)))

(define empty-k 
  (λ (k) `(empty-k ,k)))

(define apply-k
  (λ (k v)
    (match k
      (`(empty-k ,jumpout) (λ () (jumpout v)))
      (`(fib-outer-k ,n^ ,k^)
       (λ ()
         (fib-cps 
          (- n^ 2)
          (fib-inner-k v k^))))
      (`(fib-inner-k ,v^ ,k^)
       (λ ()
         (apply-k k^ (+ v^ v)))))))

(define fib-cps
  (λ (n k)
    (cond
      ((< n 2) (λ () 
                 (apply-k k n)))
      (else
       (λ ()
         (fib-cps 
          (- n 1)
          (fib-outer-k n k)))))))

(define fib-driver
  (λ (n)
    (call/cc 
     (λ (jumpout)
       (loop 
        (λ ()
          (fib-cps n (empty-k jumpout))))))))

(define loop
  (λ (th) 
    (loop (th))))
