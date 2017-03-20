#lang racket

(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
           (if (zero? n)
               ls
               (cdr (nth-cdr (sub1 n))))
              )))
      (car (nth-cdr n)))
    ))

;(list-ref '(a b c) 2)

;(list-ref '(a b c) 0)

(define (union n s)
  (cond
    [(null? n) s]
    [(if (memv (car n) s)
         (union (cdr n) s)
         (union (cdr n) (cons (car n) s)))]
    ))
  
;(union '() '())

;(union '(x) '())

;(union '(x) '(x))

;(union '(x y) '(x z))

(define (extend n s)
  (λ (v)
    (or
     (eqv? n v)
     (s v))
    ))

;((extend 1 even?) 0)

;((extend 1 even?) 1)

;((extend 1 even?) 2)

;((extend 1 even?) 3)

;(filter (extend 1 even?) '(0 1 2 3 4 5))

;(filter (extend 3 (extend 1 even?)) '(0 1 2 3 4 5))

;(filter (extend 7 (extend 3 (extend 1 even?))) '(0 1 2 3 4 5))


(define (walk-symbol x s)
  (cond
    [(boolean? (assv x s)) x]
    [else
     (cond
       [(number? (cdr (assv x s))) (cdr (assv x s))]
       [(pair? (cdr (assv x s))) (cdr (assv x s))]
       [else (walk-symbol (cdr (assv x s)) s)]
     )]))

;(walk-symbol 'a '((a . 5)))

;(walk-symbol 'a '((b . c) (a . b)))

;(walk-symbol 'a '((a . 5) (b . 6) (c . a)))

;(walk-symbol 'c '((a . 5) (b . (a . c)) (c . a)))

;(walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))

;(walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)))

;(walk-symbol 'd '((a . 5) (b . 6) (c . f) (e . c) (d . e)))


(define (var-occurs? ns exp)
  (match exp
    [`,y #:when (symbol? y) (if (eqv? y ns) #t #f)]
    [`(lambda (,y) ,body) (var-occurs? ns body)]
    [`(,rator ,rand) (or (var-occurs? ns rator) (var-occurs? ns rand))]
    ))

;(var-occurs? 'x 'x) 

;(var-occurs? 'x '(lambda (x) y))

;(var-occurs? 'x '(lambda (y) x))

;(var-occurs? 'x '((z y) x))









