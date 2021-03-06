#lang racket
(require "parenthec.rkt")

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (capture body)
  (return kexp vexp)
  (let exp body)              
  (lambda body)
  (app rator rand))

(define-union clos
  (closure body env))

(define-union envr
  (empty-env)
  (extend-env a^ env^))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *-inner-k
  (lambda (k^ k^^)
    `(*-inner-k ,k^ ,k^^)))

(define *-outer-k
  (lambda (x1^ env^ k^)
    `(*-outer-k ,x1^ ,env^ ,k^)))

(define sub1-k
  (lambda (k^)
    `(sub1-k ,k^)))

(define zero-k
  (lambda (k^)
    `(zero-k ,k^)))

(define if-k
  (lambda (conseq^ alt^ env^ k^)
    `(if-k ,conseq^ ,alt^ ,env^ ,k^)))

(define return-k
  (lambda (v-exp^ env^)
    `(return-k ,v-exp^ ,env^)
    ))

(define let-k
  (lambda (body^ env^ k^)
    `(let-k ,body^ ,env^ ,k^)))

(define app-inner-k
  (lambda (v^ k^)
    `(app-inner-k ,v^ ,k^)))

(define app-outer-k
  (lambda (rand^ env^ k^)
    `(app-outer-k ,rand^ ,env^ ,k^)))

(define value-of-cps
  (lambda (exp env k)
    (union-case exp expr
      [(const cexp) (apply-k k cexp)]
      [(mult nexp1 nexp2) (value-of-cps nexp2 env (*-outer-k nexp1 env k))]
      [(sub1 nexp) (value-of-cps nexp env (sub1-k k))]
      [(zero nexp) (value-of-cps nexp env (zero-k k))]
      [(if test conseq alt) (value-of-cps test env (if-k conseq alt env k))]
      [(capture body) (value-of-cps body (envr_extend-env k env) 
                                                 k)]
      [(return kexp vexp) (value-of-cps kexp env (return-k vexp env))]
      [(let exp body) (value-of-cps exp env (let-k body env k))]
      [(var n) (apply-env env n k)]
      [(lambda body) (apply-k k (clos_closure body env)
                         )]
      [(app rator rand) (value-of-cps rator env (app-outer-k rand env k))]
      )))
#|
(define extend-env
  (lambda (a^ env^)
    `(extend-env ,a^ ,env^)
    ))

(define empty-env
  (lambda ()
    `(empty-env)))
|#
(define apply-env
  (lambda(env y k^)
    (union-case env envr
      [(extend-env a^ env^) (if (zero? y)
                                (apply-k k^ a^)
                                (apply-env env^ (sub1 y) k^))]
      [(empty-env) (error 'value-of "unbound identifier")]
      )))
#|
(define closure
  (lambda (body env)
    `(closure ,body ,env)))
|#
(define apply-closure
  (lambda (cl a k^)
    (union-case cl clos
      [(closure body env) (value-of-cps body (envr_extend-env a env) k^)]
      )))

(define empty-k
  (lambda ()
    `(empty-k)))

(define apply-k
  (lambda (k v)
    (match k
      [`(*-inner-k ,k^ ,k^^) (apply-k k^^ (* k^ v))]
      [`(*-outer-k ,x1^ ,env^ ,k^) (value-of-cps x1^ env^ (*-inner-k v k^))]
      [`(sub1-k ,k^) (apply-k k^ (sub1 v))]
      [`(zero-k ,k^) (apply-k k^ (zero? v))]
      [`(if-k ,conseq^ ,alt^ ,env^ ,k^) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^))]
      [`(return-k ,v-exp^ ,env^) (value-of-cps v-exp^ env^ v)]
      [`(let-k ,body^ ,env^ ,k^) (value-of-cps body^ (envr_extend-env v env^) k^)]
      [`(app-inner-k ,v^ ,k^) (apply-closure v^ v k^)]
      [`(app-outer-k ,rand^ ,env^ ,k^) (value-of-cps rand^ env^ (app-inner-k v k^))]
      [`(empty-k) v]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define main 
  (lambda ()
    (value-of-cps 
     (expr_let 
      (expr_lambda
       (expr_lambda 
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_capture
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_return (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (envr_empty-env)
     (empty-k))))



