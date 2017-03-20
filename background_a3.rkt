#lang racket

;Question 1

(define value-of
  (λ (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (env y)]
      [`(λ (,x) ,body) (λ(a) (value-of body (λ(y) (if (eqv? x y) a (env y)))))]
      [`(,rator ,rand) ((value-of rator env) (value-of rand env))]
      )))

;(value-of '((lambda(x) x) 3) (λ(var) (error 'empty-env "unbound variable  ̃s" var)))

;(value-of '((λ(x) y) 3) (λ(var) (error 'empty-env "unbound variable  ̃s" var)))
;          (λ (var) (if (eq? var 'y) 7 ((λ (var) (error 'empty-env "unbound variable  ̃s" ;var)) var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define val-of
  (λ (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`(λ (,x) ,body) (λ(a) (val-of body (extend-env x a env)))]
      [`(,rator ,rand) ((val-of rator env) (val-of rand env))]
      )))

(define empty-env
  (λ()
    (λ(y) (error 'empty-env "unbound variable ̃s" y))
    ))

(define apply-env
  (λ (env y)
    (env y)
    ))

(define extend-env
  (λ(x a env)
    (λ(y) (if (eqv? x y) a (apply-env env y)))
    ))

;(val-of '((λ(x) x) 3) (empty-env))

;(val-of '((λ(x) y) 3) (empty-env))

;(val-of '((λ(x) y) 3) (extend-env 'y 7 (empty-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define val_of
  (λ (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (apply_env env y)]
      [`(λ (,x) ,body) (λ(a) (val_of body (extend_env x a env)))]
      [`(,rator ,rand) ((val_of rator env) (val_of rand env))]
      )))

(define empty_env
  (λ () `(empty_env))
  )

(define extend_env
  (λ (x a env) `(extend_env ,x ,a ,env))
  )

(define apply_env
  (λ (env y)
    (match env
      [`(empty_env) (error 'empty_env "unbound variable ̃s" y)]
      [`(extend_env ,x ,a ,env) (if(eqv? x y) a (apply_env env y))]
      )))

;(val_of '((λ(x) x) 3) (empty_env))

;(val_of '((λ(x) y) 3) (empty_env))

;(val_of '((λ(x) y) 3) (extend_env 'y 7 (empty_env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


      