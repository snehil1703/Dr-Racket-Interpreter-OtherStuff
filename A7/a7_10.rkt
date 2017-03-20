#lang racket

(define *-inner-k
  (lambda (k^ k^^)
    (lambda (v)
      (apply-k k^^ (* k^ v)))))

(define *-outer-k
  (lambda (x1^ env^ k^)
    (lambda (v)
      (value-of-cps x1^ env^
          (*-inner-k v k^)))))

(define sub1-k
  (lambda (k^)
    (lambda (v)
      (apply-k k^ (sub1 v)))))
  
(define zero-k
  (lambda (k^)
    (lambda (v)
      (apply-k k^ (zero? v)))))

(define if-k
  (lambda (conseq^ alt^ env^ k^)
    (lambda (v)
      (if v
          (value-of-cps conseq^ env^ k^)
          (value-of-cps alt^ env^ k^)
          ))))

(define return-k
  (lambda (v-exp^ env^)
    (lambda (v)
      (value-of-cps v-exp^ env^ v))
    ))

(define let-k
  (lambda (body^ env^ k^)
    (lambda (v)
      (value-of-cps body^
                    (extend-env v env^)
                    k^))))

(define app-k
  (lambda (rand^ env^ k^)
    (lambda (v)
      (value-of-cps rand^ env^ (lambda (k^^) (apply-closure v k^^ k^))))))
    
(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (k expr)]
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
      [`(app ,rator ,rand) (value-of-cps rator env (app-k rand env k))]
      )))

(define extend-env
  (lambda (a^ env^)
    `(extend-env ,a^ ,env^)
    ))

(define apply-env
  (lambda(env y k^)
    (match env
      [`(extend-env ,a^ ,env^) (if (zero? y)
                                    (k^ a^)
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

     
(define apply-k
  (lambda (k v)
    (k v)))

(define empty-env
  (lambda ()
    `(empty-env)))

(define empty-k
  (lambda ()
    (lambda (v)
      v)))

;(require "a7-student-tests.rkt")
;(test-file #:file-name "a7_10.rkt")