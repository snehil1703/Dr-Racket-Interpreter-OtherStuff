#lang racket
(require "../parentheC.rkt")

;; Require the file
;; turn the constructors into a union
;; get rid of the ` and ,
;; get rid of them in the match lines
;; turn the match into union-case
;; and match the union name
;; add the <union-name>_ to the front of
;; the constructor calls

(define-union kont
  (empty-k)
  (outer-k n^ k^)
  (inner-k v^ k^))

(define apply-k
  (λ (k v)
    (union-case k kont
      ((empty-k) v)
      ((outer-k n^ k^)
       (fib-cps (- n^ 2) (kont_inner-k v k^)))
      ((inner-k v^ k^)
       (apply-k k^ (+ v^ v))))))

(define fib-cps
  (λ (n k)
    (cond
      ((zero? n) (apply-k k 0))
      ((zero? (- n 1)) (apply-k k 1))
      (else
       (fib-cps (- n 1) (kont_outer-k n k))))))

(fib-cps 5 (kont_empty-k))

