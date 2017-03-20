#lang racket

(define empty-k
  (λ (jumpout)
    `(empty-k ,jumpout)))

(define ack-k
  (λ (m^ k^)
    `(ack-k ,m^ ,k^)))

(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (λ () (apply-k k (add1 n)))]
      [(zero? n) (λ () (ack-cps (sub1 m) 1 k))]
      [else (λ () (ack-cps m (sub1 n) (ack-k m k)))])))

(define apply-k
  (λ (k v)
    (match k
      [`(empty-k ,jumpout) (jumpout v)]
      (`(ack-k ,m^ ,k^) 
       (λ () (ack-cps (sub1 m^) v k^))))))

(define ack-driver
  (λ (m n) 
    (call/cc
     (λ (jumpout)
       (tramp 
        (λ ()
          (ack-cps m n (empty-k jumpout))))))))

(define tramp
  (λ (th)
    (tramp (th))))

