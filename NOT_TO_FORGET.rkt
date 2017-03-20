#lang racket

(define valof
  (λ (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (env y)]
      [`(λ (,x) ,b) (λ(a) (valof b (λ(y) (if (eqv? x y) a (env y)))))]
      [`(,rator ,rand) ((valof rator env) (valof rand env))]
      )))

; (valof '((λ(x)x) 5) (λ(y) '()) (((λ(x) (λ(y) x)) 5) 6) ((λ(x) y) 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define apply-env
  (λ(env y)
    (match env
      (`(extend-env ,x ,a ,env) (if(eqv? x y) a (apply-env env y)))
      (`(empty-env) '())
      (else (env y)))))

(define extend-env
  (λ(x a env)
    `(extend-env ,x ,a ,env)))

(define empty-env
  (λ()
    '()))

(define valof1
  (λ (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(λ (,x) ,b) (λ(a) (valof1 b (extend-env x a env)))]
      [`(,rator ,rand) ((valof1 rator env) (valof1 rand env))]
      )))

; (valof '((λ(x)x) 5) (λ(y) '()) (((λ(x) (λ(y) x)) 5) 6) ((λ(x) y) 5))

; ((λ(s) (λ(z) (s (s (s z)))) add1) 0)

; ((λ(x) (x x)) (λ(x) (x x)))

(((λ(x) (x x))
(λ (!)
  (λ (n)
    (cond
      ((zero? n) 1)
      (else (* n ((! !) (sub1 n)))))))) 5)