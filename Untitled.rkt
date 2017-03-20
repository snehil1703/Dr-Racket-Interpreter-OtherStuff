#lang racket

(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`,b #:when (boolean? b) b]
      [`(lambda (,x) ,body) (closure-ds x body env)]
      [`(if ,chk ,t ,f) (if (value-of-ds chk env) (value-of-ds t env) (value-of-ds f env))]
      [`(zero? ,x) (if(zero? (value-of-ds x env)) #t #f)]
      [`(* ,x ,y) (* (value-of-ds x env) (value-of-ds y env))]
      [`(sub1 ,x) (sub1 (value-of-ds x env))]
      [`(let ([,id ,expr]) ,body) ((lambda(q) (value-of-ds body (extend-env id q env))) (value-of-ds expr env))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of-ds nexp1 env) (value-of-ds nexp2 env))]
      [`(,rator ,rand) (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))]
      )))

(define closure-fn
  (lambda (x body env)
    (lambda(a) (value-of-fn body (extend-env x a env)))))

(define apply-closure-fn
  (lambda (cl a)
    (cl a)
    ))

(define closure-ds
  (lambda (x body env)
    `(closure-ds ,x ,body ,env)
    ))

(define apply-closure-ds
  (lambda (cl a)
    (match cl
      [`(closure-ds ,x ,body ,env) (value-of-ds body (extend-env x a env))]
      )))

((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))
((lambda (x) (`(value-of-fn) x `(empty-env-fn))) '((lambda (x) x) 5))
