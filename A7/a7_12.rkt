#lang racket

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
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2) (value-of-cps x2 env (*-outer-k x1 env k))]
      [`(sub1 ,x) (value-of-cps x env (sub1-k k))]
      [`(zero ,x) (value-of-cps x env (zero-k k))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (if-k conseq alt env k))]
      [`(capture ,body) (value-of-cps body (extend-env k env) 
                                                 k)]
      [`(return ,k-exp ,v-exp) (value-of-cps k-exp env (return-k v-exp env))]
      [`(let ,e ,body) (value-of-cps e env (let-k body env k))]
      [`(var ,expr) (apply-env env expr k)]
      [`(lambda ,body) (apply-k k (closure body env)
                         )]
      [`(app ,rator ,rand) (value-of-cps rator env (app-outer-k rand env k))]
      )))

(define extend-env
  (lambda (a^ env^)
    `(extend-env ,a^ ,env^)
    ))

(define empty-env
  (lambda ()
    `(empty-env)))

(define apply-env
  (lambda(env y k^)
    (match env
      [`(extend-env ,a^ ,env^) (if (zero? y)
                                    (apply-k k^ a^)
                                    (apply-env env^ (sub1 y) k^))]
      [`(empty-env) (error 'value-of "unbound identifier")]
      )))

(define closure
  (lambda (body env)
    `(closure ,body ,env)))

(define apply-closure
  (lambda (cl a k^)
    (match cl
      [`(closure ,body ,env) (value-of-cps body (extend-env a env) k^)]
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
      [`(let-k ,body^ ,env^ ,k^) (value-of-cps body^ (extend-env v env^) k^)]
      [`(app-inner-k ,v^ ,k^) (apply-closure v^ v k^)]
      [`(app-outer-k ,rand^ ,env^ ,k^) (value-of-cps rand^ env^ (app-inner-k v k^))]
      [`(empty-k) v]
      )))

;(require "a7-student-tests.rkt")
;(test-file #:file-name "a7_11.rkt")