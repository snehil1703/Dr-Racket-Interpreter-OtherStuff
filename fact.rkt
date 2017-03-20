#lang racket

;; poor man's y-combinator
(((λ (x) (x x))
  (λ (f)
     (λ (n)
       (if (zero? n)
           1
           (* n ((f f) (sub1 n))))))) 5)

;; QED: Λ-Calculus is Turing complete
