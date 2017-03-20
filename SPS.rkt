#lang racket
#|(define fibXsub-com
  (lambda (n store)
    (cond
      [(assv n store) => (lambda (pr) (values (cdr pr) store))]
      [(zero? n) (values 0 `((,n . 0) . ,store))]
      [(zero? (sub1 n)) (values 1 `((,n . 1) . store))]
      [else (let*-values (((u store) (fibXsub-com (sub1 (sub1 n)) store))
                          ((v store) (fibXsub-com (sub1 n) store)))
              (values (+ u v) `((,n . ,(+ u v) . ,store))))]
      )))

(let-values (((ans store) (fibXsub-com 100 '())))
  ans)
|#
(define (empty-store) (hash))
(define (update-store store loc val) (hash-set store loc val))
(define (ref-store store loc) (hash-ref store loc))

(define (get-loc x env)
  (cond
    [(null? env) -1]
    [(eqv? (car env) x) 0]
    [else (add1 (get-loc x (cdr env)))]
    ))

(define (snoc x ls) (append ls (list x)))

(define (empty-env) '())
(define apply-env get-loc)
(define extend-env snoc)

(define valof
  (lambda (exp env store)
    (match exp
      [(? number?) exp]
      [(? symbol?) (values (ref-store store (apply-env exp env)) store)]
      [`(lambda (,x) ,body) (values (lambda (a)
                                      (let* ((loc (length env))
                                             (store (update-store store loc a))
                                             (env (extend-env x env)))
                                        (valof body env store)))
                                    store)]
      [`(set! ,x ,rhs) (let-values (((ans store) (valof rhs env store)))
                         (values (void)
                                 (update-store store (apply-env x env) ans)))]
      [`(begin ,e0 ,e1) (let-values (((_ store) (valof e0 env store))
                                     ((ans store) (valof e1 env store)))
                          (values ans store))]
      [`(,rator ,rand) (let*-values (((arg store) (valof rand env store))
                                     (clos store) (valof rator env store))
                       (clos arg))]
      )))

(valof '(((lambda (x)
            (lambda (y)
              (begin
                (set! x y)
                y)))
          5)
         6)
       (empty-env)
       (empty-store)
       )