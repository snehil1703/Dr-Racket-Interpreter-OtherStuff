#lang racket

(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (k expr)]
      [`(mult ,x1 ,x2) (apply-k k (value-of-cps x2 env (lambda (k^)
                                              (value-of-cps x1 env (lambda (k^^)
                                                                     (* k^ k^^))))))]
      [`(sub1 ,x) (apply-k k (value-of-cps x env (lambda (k^)
                                           (sub1 k^))))]
      [`(zero ,x) (apply-k k (value-of-cps x env (lambda (k^)
                                           (zero? k^))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (lambda (k^)
                                                         (if k^
                                                             (value-of-cps conseq env k)
                                                             (value-of-cps alt env k)
                                                            )))]
      [`(capture ,body) (value-of-cps body (lambda (y k^)
                                             (if (zero? y)
                                                 (k^ k)
                                                 (apply-env env (sub1 y) k^))) 
                                                 k)]
      [`(return ,k-exp ,v-exp) (value-of-cps k-exp env (lambda (k^)
                                                         (value-of-cps v-exp env k^)))]
      [`(let ,e ,body) (value-of-cps e env (lambda (k^)
                                             (value-of-cps body
                                                           (lambda (y k^^)
                                                             (if (zero? y)
                                                                 (k^^ k^)
                                                                 (apply-env env (sub1 y) k^^)
                                                                 ))
                                                           k)))]
      [`(var ,expr) (apply-env env expr k)]
      [`(lambda ,body) (apply-k k (lambda (a k^)
                            (value-of-cps body
                                          (lambda (y k^^)
                                            (if (zero? y)
                                                (k^^ a)
                                                (apply-env env (sub1 y) k^^)))
                                          k^)
                         ))]
      [`(app ,rator ,rand) (value-of-cps rator env (lambda (k^)
                                              (value-of-cps rand env (lambda (k^^)
                                                                (apply-closure k^ k^^ k)))))]
      )))

(define apply-env
  (lambda(env y k)
    (env y k)
    ))

(define apply-closure
  (lambda (cl a k)
    (cl a k)
    ))

(define apply-k
  (lambda (k v)
    (k v)))

(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))

(define empty-k
  (lambda ()
    (lambda (v)
      v)))

;(require "a7-student-tests.rkt")
;(test-file #:file-name "a7_2.rkt")

