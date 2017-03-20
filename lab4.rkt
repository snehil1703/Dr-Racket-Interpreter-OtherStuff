#lang racket
#| Today, we will be adding binary functions to our
base interpreter (i.e., (λ (x y) ...)). Using
the interpreter given below, implement the missing
expressions.
|#

;; RI Environments
(define (empty-env)
  `(empty-env))
(define (extend-env x a env)
  `(extend-env ,x ,a ,env))
(define (apply-env env y)
  (match env
    [`(empty-env)
     (error 'valof-bin "unbound variable ~a\n" y)]
    [`(extend-env ,x ,a ,env)
     (if (eqv? x y) a
         (apply-env env y))]))

;; RI interpreter
(define valof-bin
  (λ (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(begin2 ,e1 ,e2)
       (begin
         (valof-bin e1 env)
         (valof-bin e2 env))]
      [`(set! ,var ,val)
       (set-box! (apply-env env var) (valof-bin val env))]
      [`(λ (,x1 ,x2) ,b)
       (λ (a1 a2)
         (let ([a1 (box a1)]
               [a2 (box a2)])
           (valof-bin b (extend-env x1 a1 (extend-env x2 a2 env)))))]
      [`(λ (,x) ,b)
       (λ (a)
         (let ([a (box a)])
           (valof-bin b (extend-env x a env))))]
      [`(,ra ,rd1 ,rd2)
       ((valof-bin ra env)
        (valof-bin rd1 env)
        (valof-bin rd2 env))]
      [`(,ra ,rd)
       ((valof-bin ra env)
        (valof-bin rd env))])))

(valof-bin '((λ (x y) x) 5 6)  (empty-env))
(valof-bin '((λ (x y) y) 5 6) (empty-env))
(valof-bin '((λ (x) x) 5) (empty-env))
(valof-bin '(((λ (x) (λ (x y) y)) 5) 120 6) (empty-env))
(valof-bin '((λ (x) (begin2 (set! x 120) x)) 5) (empty-env))



