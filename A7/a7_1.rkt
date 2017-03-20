#lang racket

(define value-of
  (lambda (expr env)
    (match expr
      [`(const ,expr) expr]
      [`(mult ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(capture ,body) (call/cc (lambda (k)
                                   (value-of body (lambda (y) (if (zero? y) k (env (sub1 y)))))))]
      [`(return ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(let ,e ,body) (let ((a (value-of e env)))
                         (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(var ,expr) (env expr)]
      [`(lambda ,body) (lambda (a) (value-of body (lambda (y) (if (zero? y) a (env (sub1 y))))))]
      [`(app ,rator ,rand) ((value-of rator env) (value-of rand env))])))
 
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (k expr)]
      [`(mult ,x1 ,x2) (k (value-of-cps x2 env (lambda (k^)
                                              (value-of-cps x1 env (lambda (k^^)
                                                                     (* k^ k^^))))))]
      [`(sub1 ,x) (k (value-of-cps x env (lambda (k^)
                                           (sub1 k^))))]
      [`(zero ,x) (k (value-of-cps x env (lambda (k^)
                                           (zero? k^))))]
      [`(if ,test ,conseq ,alt) (value-of-cps test env (lambda (k^)
                                                         (if k^
                                                             (value-of-cps conseq env k)
                                                             (value-of-cps alt env k)
                                                            )))]
      [`(capture ,body) (value-of-cps body (lambda (y k^)
                                             (if (zero? y)
                                                 (k^ k)
                                                 (env (sub1 y) k^))) 
                                                 k)]
      [`(return ,k-exp ,v-exp) (value-of-cps k-exp env (lambda (k^)
                                                         (value-of-cps v-exp env k^)))]
      [`(let ,e ,body) (value-of-cps e env (lambda (k^)
                                             (value-of-cps body
                                                           (lambda (y k^^)
                                                             (if (zero? y)
                                                                 (k^^ k^)
                                                                 (env (sub1 y) k^^)
                                                                 ))
                                                           k)))]
      [`(var ,expr) (env expr k)]
      [`(lambda ,body) (k (lambda (a k^)
                            (value-of-cps body
                                          (lambda (y k^^)
                                            (if (zero? y)
                                                (k^^ a)
                                                (env (sub1 y) k^^)))
                                          k^)
                         ))]
      [`(app ,rator ,rand) (value-of-cps rator env (lambda (k^)
                                              (value-of-cps rand env (lambda (k^^)
                                                                (k^ k^^ k)))))]
      )))

