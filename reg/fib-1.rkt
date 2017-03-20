#lang racket

(define fib
  (λ (n)
    (cond
      ((< n 2) n)
      (else
       (+ (fib (- n 1))
          (fib (- n 2)))))))

(fib 5)
