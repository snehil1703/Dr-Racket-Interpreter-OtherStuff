#lang racket
(require "monads.rkt")

;1
#|
(define return-maybe
  (lambda (a) `(Just ,a)))

(define bind-maybe
  (lambda (ma f)
    (match ma
      [`(Just ,a) (f a)]
      ['(Nothing) '(Nothing)])))

(define fail
  (lambda ()
    '(Nothing)))
|#
(define assv-maybe
  (lambda (ns ls)
    (cond
      [(null? ls) (fail)]
      [(equal? ns (caar ls)) (bind-maybe (return-maybe (cdar ls)) (λ(x) (return-maybe x)))]
      [else (assv-maybe ns (cdr ls))]
      )
    )
  )

;2
#|
(define return-writer
  (lambda (a) `(,a . ())))

(define bind-writer
  (lambda (ma f)
    (match-let* ((`(,a . ,la) ma)
                 (`(,b . ,lb) (f a)))
      `(,b . ,(append la lb)))))

(define tell-writer
  (lambda (msg)
    `(_ . (,msg))))
|#
(define partition-writer
  (lambda (ns ls)
    (cond
      [(null? ls) (return-writer '())]
      [(ns (car ls)) (bind-writer (tell-writer (car ls)) (λ(x) (partition-writer ns (cdr ls))))]
      [else (bind-writer (return-writer (car ls)) (λ(x)
                                                    (let ([nv (partition-writer ns (cdr ls))])
                                                      `(,(cons x (car nv)) . ,(cdr nv)))))]
      )
    )
  )

;3
#|
(define power
  (lambda (x n)
    (cond
      [(zero? n) 1]
      [(= n 1) x]
      [(odd? n) (* x (power x (sub1 n)))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (let ((y (power x nhalf)))
                     (* y y)))])))
|#
(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (return-writer 1)]
      [(equal? n 1) (return-writer x)]
      [(odd? n) (bind-writer (powerXpartials x (sub1 n)) (λ(r) (bind-writer
                                                                (tell-writer r)
                                                                (λ(nv) (return-writer (* x r))))))]
      [(even? n) (bind-writer (powerXpartials x (/ n 2)) (λ(r) (bind-writer
                                                                (tell-writer r)
                                                                (λ(nv) (return-writer (* r r))))))]
      )))

;4
#|
(define return-state
  (lambda (a)
    (lambda (s)
      `(,a . ,s))))

(define bind-state
  (lambda (ma f)
    (lambda (s)
      (match-let ((`(,v . ,s^) (ma s)))
        ((f v) s^)))))

(define get-state
  (lambda (s) `(,s . ,s)))

(define put-state
  (lambda (new-s)
    (lambda (s)
      `(_ . ,new-s))))
|#
(define replace-with-count
  (lambda (n ls)
    (cond
      [(null? ls) (return-state ls)]
      [(eqv? n (car ls)) (bind-state get-state (λ(x) (bind-state
                                                      (put-state (add1 x))
                                                      (λ(x^) (bind-state
                                                              (replace-with-count n (cdr ls))
                                                              (λ(x^^) (return-state (cons x x^^))))))))]
      [(pair? (car ls)) (bind-state
                         (replace-with-count n (car ls))
                         (λ(x) (bind-state
                                (replace-with-count n (cdr ls))
                                (λ(x^) (return-state (cons x x^))))))]
      [else (bind-state (return-state (car ls)) (λ(x) (bind-state
                                                       (replace-with-count n (cdr ls))
                                                       (λ(x^) (return-state (cons x x^))))))]
                                                      
      )))

;5
#|
(define-syntax do
  (syntax-rules (<-)
    ((_ bind e) e)
    ((_ bind (v <- e) e* e** ...)
     (bind e (lambda (v) (do bind e* e** ...))))
    ((_ bind e e* e** ...)
     (bind e (lambda (_) (do bind e* e** ...))))))
|#
(define traverse
    (lambda (return bind f)
      (letrec
        ((trav
           (lambda (tree)
             (cond
               [(pair? tree)
                (do bind
                  (a <- (trav (car tree)))
                  (d <- (trav (cdr tree)))
                  (return (cons a d)))]
               [else (f tree)]))))
        trav)))

(define reciprocal
  (lambda (ns)
    (cond
      [(zero? ns) (fail)]
      [else (bind-maybe (return-maybe (/ 1 ns)) (λ(x) (return-maybe x)))]
      )
    )
  )

(define traverse-reciprocal
    (traverse return-maybe bind-maybe reciprocal))
 
;6

(define halve
  (lambda (ns)
    (cond
      [(even? ns) (return-writer (/ ns 2))]
      [else (bind-writer (tell-writer ns) (λ(x) (return-writer ns)))]
      )
    )
  )
 
(define traverse-halve
    (traverse return-writer bind-writer halve))
 
;7

(define state/sum
  (lambda (n)
    (lambda (s)
      ((return-state s) (+ n s))
      )
    )
  )

(define traverse-state/sum
    (traverse return-state bind-state state/sum))
 
;Brainteaser
#|
(define return-cont
  (lambda (a)
    (lambda (k)
      (k a))))

(define bind-cont
  (lambda (ma f)
    (lambda (k)
      (let ((k^ (lambda (a)
                  (let ((mb (f a)))
                    (mb k)))))
        (ma k^)))))

(define callcc
  (lambda (g)
    (lambda (k)
      (let ((k-as-proc (lambda (a) (lambda (k^) (k a)))))
        (let ((ma (g k-as-proc)))
          (ma k))))))
|#
(define value-of
  (lambda (expr env)
    (match expr
      [(? number?) expr]
      [(? boolean?) expr]      
      [(? symbol?) (apply-env env expr)]
      [`(* ,x1 ,x2) (* (value-of x1 env) (value-of x2 env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(zero? ,x) (zero? (value-of x env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                   (value-of conseq env)
                                   (value-of alt env))]
      [`(capture ,k-id ,body) (call/cc (lambda (k)
                                         (value-of body (extend-env k-id k env))))]
      [`(return ,k-exp ,v-exp) ((value-of k-exp env) (value-of v-exp env))]
      [`(lambda (,id) ,body) (closure id body env)]
      [`(,rator ,rand) (apply-proc (value-of rator env) (value-of rand env))])))

(define empty-env
  (lambda()
    (lambda(y) (error 'empty-env "unbound variable ̃s" y))
    ))

(define apply-env
  (lambda (env y)
    (env y)
    ))

(define extend-env
  (lambda(x a env)
    (lambda(y) (if (eqv? x y) a (apply-env env y)))
    ))

(define closure
  (lambda (x body env)
    (lambda(a) (value-of-cps body (extend-env x a env)))))

(define apply-closure
  (lambda (cl a)
    (cl a)
    ))

(define apply-proc
  (lambda (n s)
    (n s)
    ))

(define value-of-cps
  (lambda (expr env)
    (match expr
      [(? number?) (return-cont expr)]
      [(? boolean?) (return-cont expr)]      
      [(? symbol?) (return-cont (apply-env env expr))]
      [`(* ,x1 ,x2) (bind-cont
                     (value-of-cps x1 env)
                     (lambda (n) (bind-cont
                                  (value-of-cps x2 env)
                                  (lambda (s) (return-cont (* n s))))))]
      [`(sub1 ,x) (bind-cont
                   (value-of-cps x env)
                   (lambda (n) (return-cont (sub1 n))))]
      [`(zero? ,x) (bind-cont
                    (value-of-cps x env)
                    (lambda (n) (return-cont (zero? n))))]
      [`(if ,test ,conseq ,alt) (bind-cont
                                 (value-of-cps test env)
                                 (lambda (ns) (if ns
                                                  (value-of-cps conseq env)
                                                  (value-of-cps alt env))))]
      [`(capture ,k-id ,body) (callcc (lambda (k)
                                         (value-of-cps body (extend-env k-id k env))))]
      [`(return ,k-exp ,v-exp) (bind-cont
                                (value-of-cps k-exp env)
                                (lambda (n) (bind-cont
                                             (value-of-cps v-exp env)
                                             (lambda (s) (n s)))))]
      [`(lambda (,id) ,body) (return-cont (closure id body env))]
      [`(,rator ,rand) (bind-cont
                        (value-of-cps rator env)
                        (lambda (n) (bind-cont
                                     (value-of-cps rand env)
                                     (lambda (s) (apply-proc n s)))))])))

(define fact-5
    '((lambda (f)
        ((f f) 5))
      (lambda (f)
        (lambda (n)
          (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))
 
(define capture-fun
    '(* 3 (capture q (* 2 (return q 4)))))

;(require "a12-student-tests.rkt")
;(test-file #:file-name "a12.rkt")
