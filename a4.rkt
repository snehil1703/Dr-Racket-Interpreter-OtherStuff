#lang racket

;Part 1

(define lex
  (lambda (exp acc)
    (match exp
      [`,n #:when (number? n) (list 'const n)]
      [`,y #:when (symbol? y) (if (member y acc)
                                  (list 'var
                                        (letrec ([position (lambda (n)
                                                       (if (eqv? (car n) y)
                                                           0
                                                           (add1 (position (cdr n)))))])
                                          (position acc)))
                                  '()
                                  )]
      [`(if ,chk ,t ,f) (cons 'if (cons (lex chk acc) (cons (lex t acc) (lex f acc))))]
      [`(zero? ,x) (list 'zero? (lex x acc))]
      [`(* ,x ,y) (cons (cons '* (list (lex x acc) (lex y acc))) '())]
      [`(sub1 ,x) (list 'sub1 (lex x acc))]
      [`(let ([,id ,expr]) ,body) (cons 'let (cons
                                              (lex expr acc)
                                              (cons (lex body (cons id acc)) '()))
                                        )]
      [`(lambda (,x) ,body)
       (cons 'lambda
             (cons (lex body (cons x acc)) '()))]
      [`(,rator ,rand) (list (lex rator acc) (lex rand acc))]
      )))

;(lex '((lambda (x) x) 5)  '())

;(lex '(lambda (!)
 ; 	  (lambda (n)
  ;	    (if (zero? n) 1 (* n (! (sub1 n))))))
;	'())

;(lex '(let ((! (lambda (!)
 ; 		   (lambda (n)
  ;		     (if (zero? n) 1 (* n ((! !) (sub1 n))))))))
   ;       ((! !) 5))
    ;   '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Part 2

(define value-of
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (unbox (env y))]
      [`,b #:when (boolean? b) b]
      [`(lambda (,x) ,body) (lambda(a) (value-of body (lambda(y) (if (eqv? x y) a (env y)))))]
      [`(if ,chk ,t ,f) (if (value-of chk env) (value-of t env) (value-of f env))]
      [`(zero? ,x) (if(zero? (value-of x env)) #t #f)]
      [`(* ,x ,y) (* (value-of x env) (value-of y env))]
      [`(sub1 ,x) (sub1 (value-of x env))]
      [`(let ([,id ,expr]) ,body) (value-of body (let ([q (value-of expr env)]) (lambda(r) (if (eqv? id r) q (env r)))))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of nexp1 env) (value-of nexp2 env))]
      [`(,rator ,rand) ((value-of rator env) (value-of rand env))]
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

(define empty-env
  (lambda () `(empty-env))
  )

(define extend-env
  (lambda (x a env) `(extend-env ,x ,a ,env))
  )

(define apply-env
  (lambda (env y)
    (match env
      [`(empty-env) (error 'empty-env "unbound variable ̃s" y)]
      [`(extend-env ,x ,a ,env) (if(eqv? x y) a (apply-env env y))]
      )))

;Functional Representation of Closures

(define value-of-fn
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (apply-env env y)]
      [`,b #:when (boolean? b) b]
      [`(lambda (,x) ,body) (closure-fn x body env)]
      [`(if ,chk ,t ,f) (if (value-of-fn chk env) (value-of-fn t env) (value-of-fn f env))]
      [`(zero? ,x) (if(zero? (value-of-fn x env)) #t #f)]
      [`(* ,x ,y) (* (value-of-fn x env) (value-of-fn y env))]
      [`(sub1 ,x) (sub1 (value-of-fn x env))]
      [`(let ([,id ,expr]) ,body) ((lambda(q) (value-of-fn body (extend-env id q env))) (value-of-fn expr env))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of-fn nexp1 env) (value-of-fn nexp2 env))]
      [`(,rator ,rand) (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))]
      )))

(define closure-fn
  (lambda (x body env)
    (lambda(a) (value-of-fn body (extend-env x a env)))))

(define apply-closure-fn
  (lambda (cl a)
    (cl a)
    ))

;(value-of-fn 
 ;   '((lambda (x) (if (zero? x) 
  ;                    12 
   ;                   47)) 
    ;   0) 
    ;(empty-env))
  
;(value-of-fn
 ;  '(let ([y (* 3 4)])
  ;    ((lambda (x) (* x y)) (sub1 6)))
   ;(empty-env))

;(value-of-fn
 ;  '(let ([x (* 2 3)])
  ;    (let ([y (sub1 x)])
   ;     (* x y)))
   ;(empty-env))

;(value-of-fn
 ;  '(let ([x (* 2 3)])
  ;    (let ([x (sub1 x)])
   ;     (* x x)))
   ;(empty-env))

;Data-Structural Representation of Closures

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

(define closure-ds
  (lambda (x body env)
    `(closure-ds ,x ,body ,env)
    ))

(define apply-closure-ds
  (lambda (cl a)
    (match cl
      [`(closure-ds ,x ,body ,env) (value-of-ds body (extend-env x a env))]
      )))

;(value-of-ds
 ;   '((lambda (x) (if (zero? x) 
  ;                    12 
   ;                   47)) 
    ;   0) 
    ;(empty-env))

;(value-of-ds
 ;  '(let ([y (* 3 4)])
  ;    ((lambda (x) (* x y)) (sub1 6)))
   ;(empty-env))

;(value-of-ds
 ;  '(let ([x (* 2 3)])
  ;    (let ([y (sub1 x)])
   ;     (* x y)))
   ;(empty-env))

;(value-of-ds
 ;  '(let ([x (* 2 3)])
  ;    (let ([x (sub1 x)])
   ;     (* x x)))
   ;(empty-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Part 3

(define value-of-dynamic
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (apply-env-ds env y)]
      [`,b #:when (boolean? b) b]
      [`(lambda (,x) ,body) `(lambda (,x) ,body)]
      [`(if ,chk ,t ,f) (if (value-of-dynamic chk env) (value-of-dynamic t env) (value-of-dynamic f env))]
      [`(zero? ,x) (if(zero? (value-of-dynamic x env)) #t #f)]
      [`(* ,x ,y) (* (value-of-dynamic x env) (value-of-dynamic y env))]
      [`(sub1 ,x) (sub1 (value-of-dynamic x env))]
      [`(let ([,id ,expr]) ,body) ((lambda(q) (value-of-dynamic body (extend-env-ds id q env))) (value-of-dynamic expr env))]
      [`(+ ,nexp1 ,nexp2) (+ (value-of-dynamic nexp1 env) (value-of-dynamic nexp2 env))]
      [`(null? ,ns) (if (null? (value-of-dynamic ns env)) #t #f)]
      [`(cons ,n ,s) (cons (value-of-dynamic n env) (value-of-dynamic s env))]
      [`(car ,ns) (car (value-of-dynamic ns env))]
      [`(cdr ,ns) (cdr (value-of-dynamic ns env))]
      [`(quote ,v) v]
      [`(,rator ,rand) (match-let ((`(lambda (,x) ,body) (value-of-dynamic rator env))
                                   (`,a (value-of-dynamic rand env)))
                         (value-of-dynamic body (extend-env-ds x a env)))]
      )))

(value-of-dynamic
 '(let (( map (lambda (f)
               (lambda (ls)
                 (if (null? ls) '()
                     (cons (f (car ls)) (( map f) (cdr ls))))))))
   (let ([f (lambda (e) (cons e ls))])
     (( map f) (cons 2 (cons 3 '()))))) (empty-env))

;(value-of-dynamic '(let ([x 2])
 ;                      (let ([f (lambda (e) x)])
  ;                       (let ([x 5])
   ;                        (f 0))))
    ;                (empty-env))

;(value-of-dynamic
 ;   '(let ([! (lambda (n)
  ;              (if (zero? n) 
   ;                 1
    ;                (* n (! (sub1 n)))))])
     ;  (! 5))
    ;(empty-env))

;(value-of-dynamic
 ;   '(let ([f (lambda (x) (cons x l))])
  ;     (let ([cmap 
;	      (lambda (f)
;		(lambda (l)               
;		  (if (null? l) 
;		      '()
;		      (cons (f (car l)) ((cmap f) (cdr l))))))])
;	 ((cmap f) (cons 1 (cons 2 (cons 3 '())))))) 
 ;   (empty-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Brainteasers

(define value-of-ri
  (lambda (empty extend apply closure apply-closure)
    (lambda (ns)
      (letrec ([func (lambda (exp env)
                       (match exp
                         [`,n #:when (number? n) n]
                         [`,y #:when (symbol? y) (apply env y)]
                         [`,b #:when (boolean? b) b]
                         [`(lambda (,x) ,body) (closure x body env empty extend apply closure apply-closure)]
                         [`(if ,chk ,t ,f) (if (func chk env) (func t env) (func f env))]
                         [`(zero? ,x) (if(zero? (func x env)) #t #f)]
                         [`(* ,x ,y) (* (func x env) (func y env))]
                         [`(sub1 ,x) (sub1 (func x env))]
                         [`(let ([,id ,expr]) ,body) ((lambda(q) (func body (extend id q env))) (func expr env))]
                         [`(+ ,nexp1 ,nexp2) (+ (func nexp1 env) (func nexp2 env))]
                         [`(null? ,ns) (if (null? (func ns env)) #t #f)]
                         [`(cons ,n ,s) (cons (func n env) (func s env))]
                         [`(car ,ns) (car (func ns env))]
                         [`(cdr ,ns) (cdr (func ns env))]
                         [`(quote ,v) v]
                         [`(,rator ,rand) (apply-closure (func rator env) (func rand env))]
                         ))])
        (func ns empty))
      )))

(define closure-fn-ri
  (lambda (x body env empty extend apply closure apply-closure)
    (lambda(a) ((value-of-ri (extend x a env) extend apply closure apply-closure) body))))

(define apply-closure-fn-ri
  (lambda (cl a)
    (cl a)
    ))

(define closure-ds-ri
  (lambda (x body env empty extend apply closure apply-closure)
    `(closure-ds ,x ,body ,env)
    ))

(define apply-closure-ds-ri
  (lambda (cl a)
    (match cl
      [`(closure-ds ,x ,body ,env) (value-of-ds body (extend-env x a env))]
      )))

;((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))

;((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))

;((value-of-ri empty-env-fn extend-env-fn apply-env-fn closure-ds-ri apply-closure-ds-ri) '((lambda (x) x) 5))

;((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-fn-ri apply-closure-fn-ri) '((lambda (x) x) 5))

;((value-of-ri empty-env-ds extend-env-ds apply-env-ds closure-fn-ri apply-closure-fn-ri)
 ;'(let ([x 2])
  ;  (let ([f (lambda (e) x)])
   ;   (let ([x 5])
    ;    (f 0)
     ;   )
      ;)
    ;)
 ;)

;(require "a4-student-tests.rkt")
;(test-file #:file-name "a4.rkt")