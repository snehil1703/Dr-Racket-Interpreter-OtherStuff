#lang racket

;; 1. let
;; http://docs.racket-lang.org/guide/let.html
(define fact
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (fact (sub1 n)))])))

(define double-fact-naive
  (lambda (n)
    (+ (fact n) (fact n))))

(define double-fact-with-let
  (lambda (n)
    (let ([x (fact n)])
      (+ x x))))

;; 2. higher-order functions
(define apply-one
  (lambda (f x)
    (f x)))

;; 2.2. curried functions
;; https://en.wikipedia.org/wiki/Haskell_Curry
(define plus-curried
  (lambda (x)
    (lambda (y)
      (+ x y))))

;; ((plus-curried 5) 2)
;; > 7

;; 3. plus. the hard way
;; (define plus
;;   (lambda (x y)
;;     (cond
;;       [(zero? y) x]
;;       [else (+ 1 (plus x (sub1 y)))])))

;; (define times
;;   (lambda (x y)
;;     (cond
;;       [(zero? y) 0]
;;       [else (plus x (times x (sub1 y)))])))

;; (define ^ ;; power
;;   (lambda (x y)
;;     (cond
;;       [(zero? y) 1]
;;       [else (times x (^ x (sub1 y)))])))

;; hmm. can we write a function to generate these functions?

;; (define G
;;   (lambda (i)
;;     (cond
;;       [(zero? i) (lambda (x y)
;;                    (cond
;;                      [(zero? y) x]
;;                      [else (add1 ((G i) x (sub1 y)))]))]
;;       [(zero? (sub1 i)) (lambda (x y)
;;                           (cond
;;                             [(zero? y) 0]
;;                             [else ((G (sub1 i)) x ((G i) x (sub1 y)))]))]
;;       [else (lambda (x y)
;;               (cond
;;                 [(zero? y) 1]
;;                 [else ((G (sub1 i)) x ((G i) x (sub1 y)))]))])))

;; it works!
;; we realize that no matter what question we ask we always return a new function (lambda (x y) ...)
;; so lets just automatically return that function from G

;; (define G
;;   (lambda (i)
;;     (lambda (x y)
;;       (cond
;;         [(zero? i) (cond
;;                      [(zero? y) x]
;;                      [else (add1 ((G i) x (sub1 y)))])]
;;         [(zero? (sub1 i)) (cond
;;                             [(zero? y) 0]
;;                             [else ((G (sub1 i)) x ((G i) x (sub1 y)))])]
;;         [else (cond
;;                 [(zero? y) 1]
;;                 [else ((G (sub1 i)) x ((G i) x (sub1 y)))])]))))

;; We then realized that we could reverse the order of those
;; tests. Handle all the cases where y is zero at once, and then handle
;; all the cases where y is non-zero.

;; (define G
;;   (lambda (i)
;;     (lambda (x y)
;;       (cond
;;         [(zero? y) (cond
;;                      [(zero? i) x]
;;                      [(zero? (sub1 i)) 0]
;;                      [else 1])]
;;         [else (cond
;;                 [(zero? i) (add1 ((G i) x (sub1 y)))]
;;                 [(zero? (sub1 i)) ((G (sub1 i)) x ((G i) x (sub1 y)))]
;;                 [else ((G (sub1 i)) x ((G i) x (sub1 y)))])]))))

;; We found a duplicated line, so we removed it. We then pulled out

;; (define G
;;   (lambda (i)
;;     (lambda (x y)
;;       (cond
;;         [(zero? y) (cond
;;                      [(zero? i) x]
;;                      [(zero? (sub1 i)) 0]
;;                      [else 1])]
;;         [else (cond
;;                 [(zero? i) (add1 ((G i) x (sub1 y)))]
;;                 [else ((G (sub1 i)) x ((G i) x (sub1 y)))])]))))

;; That [else (cond ...) ...] was a little verbose for our tastes, so we
;; got rid of it.

;; (define G
;;   (lambda (i)
;;     (lambda (x y)
;;       (cond
;;         [(zero? y) (cond
;;                      [(zero? i) x]
;;                      [(zero? (sub1 i)) 0]
;;                      [else 1])]
;;         [(zero? i) (add1 ((G i) x (sub1 y)))]
;;         [else ((G (sub1 i)) x ((G i) x (sub1 y)))]))))

;; We can get rid of that nested cond by in-lining the (zero? y) test.

;; (define G
;;   (lambda (i)
;;     (lambda (x y)
;;       (cond
;;         [(and (zero? y) (zero? i)) x]
;;         [(and (zero? y) (zero? (sub1 i))) 0]
;;         [(zero? y) 1]
;;         [(zero? i) (add1 ((G i) x (sub1 y)))]
;;         [else ((G (sub1 i)) x ((G i) x (sub1 y)))]))))

;; And we really have no need to have nested those function calls, so
;; let's de-nest them.

(define G
  (lambda (i x y)
    (cond
      [(and (zero? y) (zero? i)) x]
      [(and (zero? y) (zero? (sub1 i))) 0]
      [(zero? y) 1]
      [(zero? i) (add1 (G i x (sub1 y)))]
      [else (G (sub1 i) x (G i x (sub1 y)))])))

;; now to show we have plus, times, ^ that we started with among
;; the infinitely others we define them

(define plus
  (lambda (x y)
    (G 0 x y)))

(define times
  (lambda (x y)
    (G 1 x y)))

(define ^
  (lambda (x y)
    (G 2 x y)))

(define ^^
  (lambda (x y)
    (G 3 x y)))

(define ^^^ ;; dont try this at home
  (lambda (x y)
    (G 4 x y)))

;; https://en.wikipedia.org/wiki/Ackermann_function
;; ^ just recreational stuff. you won't need it for the course

