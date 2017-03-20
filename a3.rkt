#lang racket

;Part 1

;Interpreter 1

(define value-of
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (unbox (env y))]
      [`,b #:when (boolean? b) b]
      [`(lambda (,x) ,body) (lambda(a) (value-of body  (let ([a (box a)]) (lambda(y) (if (eqv? x y) a (env y))))))]
      [`(if ,chk ,t ,f) (if (value-of chk env) (value-of t env) (value-of f env))]
      [`(zero? ,x) (if(zero? (value-of x env)) #t #f)]
      [`(* ,x ,y) (* (value-of x env) (value-of y env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(let ([,id ,expr]) ,bdy) (value-of bdy (let ([q (box (value-of expr env))]) (lambda(r) (if (eqv? id r) q (env r)))))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of nexp1 env) (value-of nexp2 env))]
      [`(begin2 ,e1 ,e2) (value-of e2 (let ([q (value-of e1 env)]) (lambda(p) (if (eqv? p q) q (env p)))))]
      [`(set! ,x ,expr) (set-box! (env x) (value-of expr env))]
      [`(,rator ,rand) ((value-of rator env) (value-of rand env))]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Interpreter 2

(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (apply-env-fn env y)]
      [`,b #:when (boolean? b) b]
      [`(lambda (,x) ,body) (lambda(a) (value-of-fn body (extend-env-fn x a env)))]
      [`(if ,chk ,t ,f) (if (value-of-fn chk env) (value-of-fn t env) (value-of-fn f env))]
      [`(zero? ,x) (if(zero? (value-of-fn x env)) #t #f)]
      [`(* ,x ,y) (* (value-of-fn x env) (value-of-fn y env))]
      [`(sub1 ,x) (sub1 (value-of-fn x env))]
      [`(let ([,id ,expr]) ,bdy) ((lambda(q) (value-of-fn bdy (extend-env-fn id q env))) (value-of-fn expr env))]
      [`(,rator ,rand) ((value-of-fn rator env) (value-of-fn rand env))]
      )))

(define empty-env-fn
  (lambda()
    (lambda(y) (error 'empty-env-fn "unbound variable ̃s" y))
    ))

(define apply-env-fn
  (lambda (env y)
    (env y)
    ))

(define extend-env-fn
  (lambda(x a env)
    (lambda(y) (if (eqv? x y) a (apply-env-fn env y)))
    ))

;(value-of-fn
 ;  '((lambda (x) (if (zero? x)
  ;                   #t
   ;                  #f))
    ; 0)
   ;(empty-env-fn))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Interpreter 3

(define value-of-ds
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (apply-env-ds env y)]
      [`,b #:when (boolean? b) b]
      [`(lambda (,x) ,body) (lambda(a) (value-of-ds body (extend-env-ds x a env)))]
      [`(if ,chk ,t ,f) (if (value-of-ds chk env) (value-of-ds t env) (value-of-ds f env))]
      [`(zero? ,x) (if(zero? (value-of-ds x env)) #t #f)]
      [`(* ,x ,y) (* (value-of-ds x env) (value-of-ds y env))]
      [`(sub1 ,x) (sub1 (value-of-ds x env))]
      [`(let ([,id ,expr]) ,bdy) ((lambda(q) (value-of-ds bdy (extend-env-ds id q env))) (value-of-ds expr env))]
      [`(,rator ,rand) ((value-of-ds rator env) (value-of-ds rand env))]
      )))
      
(define empty-env-ds
  (lambda () `(empty-env-ds))
  )

(define extend-env-ds
  (lambda (x a env) `(extend-env-ds ,x ,a ,env))
  )

(define apply-env-ds
  (lambda (env y)
    (match env
      [`(empty-env-ds) (error 'empty-env-ds "unbound variable ̃s" y)]
      [`(extend-env-ds ,x ,a ,env) (if(eqv? x y) a (apply-env-ds env y))]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Part 2

;Interpreter 4

(define fo-eulav
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (env y)]
      [`,b #:when (boolean? b) b]
      [`(,body (,x) adbmal) (lambda(a) (fo-eulav body (lambda(y) (if (eqv? x y) a (env y)))))]
      [`(,x 1bus) (sub1 (fo-eulav x env))]
      [`(,x ?orez) (if(zero? (fo-eulav x env)) #t #f)]
      [`(,y ,x *) (* (fo-eulav x env) (fo-eulav y env))]
      [`(,f ,t ,chk fi) (if (fo-eulav chk env) (fo-eulav t env) (fo-eulav f env))]
      [`(,rator ,rand) ((fo-eulav rand env) (fo-eulav rator env))]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Brainteasers

;Interpreter 5

; SET! and BEGIN2 added to "value-of" with required changes in symbol, λ-abstraction and let-expression

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Brainteasers

;Interpreter 6

(define value-of-lex
  (lambda (exp env)
    (match exp
      ((? boolean?) exp)
      ((? number?) exp)
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(zero? ,body) (zero? (value-of-lex body env)))
      (`(* ,n1 ,n2) (* (value-of-lex n1 env) (value-of-lex n2 env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))

(define empty-env-lex 
  (lambda () '()))

(define extend-env-lex
  (lambda(a env)
    (if(eqv? a 'lambda) env (cons a env)) 
    ))

(define apply-env-lex
  (lambda(env y)
    (memv y env)
    ))

(value-of-lex '((lambda (var 0)) 5) (empty-env-lex))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;