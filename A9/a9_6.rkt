#lang racket
(require "parenthec.rkt")

(define-registers exp env k y k^ cl a v)

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

(define-union kt
  (*-inner-k k^ k^^)
  (*-outer-k x1^ env^ k^)
  (sub1-k k^)
  (zero-k k^)
  (if-k conseq^ alt^ env^ k^)
  (return-k v-exp^ env^)
  (let-k body^ env^ sk^)
  (app-inner-k v^ k^)
  (app-outer-k rand^ env^ k^)
  (empty-k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
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
|#
(define value-of-cps
  (lambda () ;; exp env k
    (union-case exp expr
      [(const cexp^) (begin (set! v cexp^)
                           (apply-k))]
      [(mult nexp1^ nexp2^) (begin (set! exp nexp2^)
                                 (set! k (kt_*-outer-k nexp1^ env k))
                                 (value-of-cps))]
      [(sub1 nexp^) (begin (set! exp nexp^)
                          (set! k (kt_sub1-k k))
                          (value-of-cps))]
      [(zero nexp^) (begin (set! exp nexp^)
                          (set! k (kt_zero-k k))
                          (value-of-cps))]
      [(if test^ conseq^ alt^) (begin (set! exp test^)
                                   (set! k (kt_if-k conseq^ alt^ env k))
                                   (value-of-cps))]
      [(capture body^) (begin (set! exp body^)
                             (set! env (envr_extend-env k env))
                             (value-of-cps))]
      [(return kexp vexp) (begin (set! exp kexp)
                                 (set! k (kt_return-k vexp env))
                                 (value-of-cps))]
      [(let exp^ body^) (begin
                         (set! exp exp^)
                         (set! k (kt_let-k body^ env k))
                             (value-of-cps))]
      [(var n^) (begin (set! k^ k)
                      (set! y n^)
                      (apply-env))]
      [(lambda body^) (begin (set! v (clos_closure body^ env))
                            (apply-k))]
      [(app rator^ rand^) (begin (set! exp rator^)
                               (set! k (kt_app-outer-k rand^ env k))
                               (value-of-cps))]
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
  (lambda () ;; env y k^
    (union-case env envr
      [(extend-env a^ env^) (if (zero? y)
                                (begin (set! k k^)
                                       (set! v a^)
                                       (apply-k))
                                (begin (set! env env^)
                                       (set! y (sub1 y))
                                       (apply-env)))]
      [(empty-env) (error 'value-of "unbound identifier")]
      )))
#|
(define closure
  (lambda (body env)
    `(closure ,body ,env)))
|#
(define apply-closure
  (lambda () ;; cl a k^
    (union-case cl clos
      [(closure body^ env^) (begin (set! exp body^)
                                 (set! env (envr_extend-env a env^))
                                 (set! k k^)
                                 (value-of-cps))]
      )))
#|
(define empty-k
  (lambda ()
    `(empty-k)))
|#
(define apply-k
  (lambda () ;; k v
    (union-case k kt
      [(*-inner-k k^ k^^) (begin (set! k k^^)
                                 (set! v (* k^ v))
                                 (apply-k))]
      [(*-outer-k x1^ env^ k^) (begin (set! exp x1^)
                                      (set! env env^)
                                      (set! k (kt_*-inner-k v k^))
                                      (value-of-cps))]
      [(sub1-k k^) (begin (set! k k^)
                          (set! v (sub1 v))
                          (apply-k))]
      [(zero-k k^) (begin (set! k k^)
                          (set! v (zero? v))
                          (apply-k))]
      [(if-k conseq^ alt^ env^ k^) (if v
                                       (begin (set! exp conseq^)
                                              (set! env env^)
                                              (set! k k^)
                                              (value-of-cps))
                                       (begin (set! exp alt^)
                                              (set! env env^)
                                              (set! k k^)
                                              (value-of-cps)))]
      [(return-k v-exp^ env^) (begin (set! exp v-exp^)
                                     (set! env env^)
                                     (set! k v)
                                     (value-of-cps))]
      [(let-k body^ env^ k^) (begin (set! exp body^)
                                     (set! env (envr_extend-env v env^))
                                     (set! k k^)
                                     (value-of-cps))]
      [(app-inner-k v^ k^^) (begin (set! cl v^)
                                  (set! a v)
                                  (set! k^ k^^)
                                  (apply-closure))]
      [(app-outer-k rand^ env^ k^) (begin (set! exp rand^)
                                          (set! env env^)
                                          (set! k (kt_app-inner-k v k^))
                                          (value-of-cps))]
      [(empty-k) v]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define main 
  (lambda ()
    (begin (set! exp (expr_let
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
                  (expr_const 5))))
           (set! env (envr_empty-env))
           (set! k (kt_empty-k))
           (value-of-cps))
    ))


