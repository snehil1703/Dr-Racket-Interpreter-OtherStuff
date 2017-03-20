#lang racket
(require "mk.rkt")

(call/cc (lambda (k) (+ (k (+ (k 2) 1)) 3)))

(call/cc ((lambda (a) (lambda (b) 5)) (lambda (k) (k 7))))

((lambda (x)
   ((lambda (f)
      (begin
        (set! x (f (f 3)))
        (f x)))
    (lambda (y)
      (begin
        (set! y (add1 x))
        x)))) 6)

(run* (q)
      (fresh (a b c)
             (== `(,b ,c . ,a) `(fish ,a ,b))
             (== `(,a ,b ,c) q)))

(run* (q)
      (conde
       [(== 'cat q)]
       [(== 'dog q)]
       [(== 'res q)]
       )
      (conde
       [(== 'cat q)
        (conde
         [(== 'fish q)]
         [(== q 'cat)])]
       [(== q 'dog)]))

(define f
  (lambda (a b c)
    