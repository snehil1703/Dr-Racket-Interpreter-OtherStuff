#lang racket

;; based on http://www.slideshare.net/yinwang0/reinventing-the-ycombinator
;; the factorial version

;; https://vimeo.com/45140590
;; ^ a great talk explaining the y combinator if you are interested

;; (define fact
;;   (lambda (n)
;;     (cond
;;       [(zero? n) 1]
;;       [else (* n (fact (sub1 n)))])))

(define define 'no-way)

;; not gonna work
(lambda (fact)
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n ((fact fact) (sub1 n)))])))

;; this works! but we dont like duplication.
(((lambda (fact)
    (lambda (n)
      (cond
        [(zero? n) 1]
        [else (* n ((fact fact) (sub1 n)))])))
  (lambda (fact)
    (lambda (n)
      (cond
        [(zero? n) 1]
        [else (* n ((fact fact) (sub1 n)))]))))
 5)

;; self-application. cool.
(((lambda (x) (x x))
  (lambda (fact)
    (lambda (n)
      (cond
        [(zero? n) 1]
        [else (* n ((fact fact) (sub1 n)))]))))
 5)

;; can we abstract the `(fact fact)` part?
(((lambda (x) (x x))
  (lambda (fact)
    ((lambda (g)
       (lambda (n)
         (cond
           [(zero? n) 1]
           [else (* n ((fact fact) (sub1 n)))])))
     (lambda (v) ((fact fact) v)))))
 5)

;; y combinator, take 1
(((lambda (f)
    ((lambda (u) (u u))
     (lambda (x)
       (f (lambda (n)
            ((x x) n))))))
  (lambda (fact)
    (lambda (n)
      (cond
        [(zero? n) 1]
        [else (* n (fact (sub1 n)))]))))
 5)

;; y combinator, take 2
((((lambda (y) (lambda (f) (f (lambda (x) (((y y) f) x)))))
   (lambda (y) (lambda (f) (f (lambda (x) (((y y) f) x))))))
  (lambda (imp)
    (lambda (n)
      (cond
        [(< n 2) n]
        [else (* n (imp (sub1 n)))]))))
 5)
