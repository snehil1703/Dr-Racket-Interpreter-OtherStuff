#lang racket

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

(define apply-closure-fn
  (lambda (cl a)
    (cl a)
    ))

;CALL BY VALUE

;(define val-of-cbv
 ; (lambda (exp env)
  ;  (match exp
   ;   [`,n #:when (number? n) n]
    ;  [`,y #:when (symbol? y) (unbox (apply-env env y))]
     ; [`,b #:when (boolean? b) b]
      ;[`(lambda (,x) ,body) (closure-cbv x body env)]
      ;[`(if ,chk ,t ,f) (if (val-of-cbv chk env) (val-of-cbv t env) (val-of-cbv f env))]
      ;[`(zero? ,x) (if(zero? (val-of-cbv x env)) #t #f)]
      ;[`(* ,x ,y) (* (val-of-cbv x env) (val-of-cbv y env))]
      ;[`(sub1 ,x) (sub1 (val-of-cbv x env))]
      ;[`(let ([,id ,expr]) ,body) ((lambda(q) (val-of-cbv body (extend-env id q env))) (val-of-cbv expr env))]
      ;[`(+ ,nexp1 ,nexp2) (+ (val-of-cbv nexp1 env) (val-of-cbv nexp2 env))]
      ;[`(begin2 ,n1 ,n2) (begin (val-of-cbv n1 env) (val-of-cbv n2 env))]
      ;[`(set! ,id ,expr) (let ([val-expr (val-of-cbv expr env)]) (set-box! (apply-env env id) val-expr))]
      ;[`(random ,s) (random (val-of-cbv s env))]
      ;[`(,rator ,y) #:when (symbol? y) (apply-closure-fn (val-of-cbv rator env) (box (unbox (apply-env env y))))]
      ;[`(,rator ,rand) (apply-closure-fn (val-of-cbv rator env) (box (val-of-cbv rand env)))]
      ;)))

;(define closure-cbv
 ; (lambda (x body env)
  ;  (lambda(a) (val-of-cbv body (extend-env x a env)))))

(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`,n #:when (number? n) n]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`,b #:when (boolean? b) b]
      [`(quote ,v) v]
      [`(lambda (,x) ,body) (closure-cbv x body env)]
      [`(if ,chk ,t ,f) (if (val-of-cbv chk env) (val-of-cbv t env) (val-of-cbv f env))]
      [`(zero? ,x) (if (zero? (val-of-cbv x env)) #t #f)]
      [`(* ,x ,y) (* (val-of-cbv x env) (val-of-cbv y env))]
      [`(sub1 ,x) (sub1 (val-of-cbv x env))]
      [`(cons^ ,n ,s) (box (lambda() (cons (val-of-cbv n env) (val-of-cbv s env))))]
      [`(car^ ,ns) (car ((unbox (val-of-cbv ns env))))]
      [`(cdr^ ,ns) (cdr ((unbox (val-of-cbv ns env))))]
      [`(cons ,n ,s) (cons (val-of-cbv n env) (val-of-cbv s env))]
      [`(car ,ns) (car (val-of-cbv ns env))]
      [`(cdr ,ns) (cdr (val-of-cbv ns env))]
      [`(add1 ,n) (add1 (val-of-cbv n env))]
      [`(null? ,x) (if (null? x) #t #f)]
      [`(let ([,id ,expr]) ,body) ((lambda(q) (val-of-cbv body (extend-env id q env))) (box (val-of-cbv expr env)))]
      [`(+ ,nexp1 ,nexp2) (+ (val-of-cbv nexp1 env) (val-of-cbv nexp2 env))]
      [`(begin2 ,n1 ,n2) (begin (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(set! ,id ,expr) (let ([val-expr (val-of-cbv expr env)]) (set-box! (apply-env env id) val-expr))]
      [`(random ,s) (random (val-of-cbv s env))]
      [`(,rator ,y) #:when (symbol? y) (apply-closure-fn (val-of-cbv rator env) (box (unbox (apply-env env y))))]
      [`(,rator ,rand) (apply-closure-fn (val-of-cbv rator env) (box (val-of-cbv rand env)))]
      )))

(define closure-cbv
  (lambda (x body env)
    (lambda(a) (val-of-cbv body (extend-env x a env)))))

(val-of-cbv
   '((lambda (a)
       ((lambda (p)
          (begin2
           (p a)
           a)) (lambda (x) (set! x 4)))) 3)
   (empty-env))

(val-of-cbv
   '((lambda (f)
       ((lambda (g)
          ((lambda (z) (begin2
                        (g z)
                        z))
           55))
        (lambda (y) (f y)))) (lambda (x) (set! x 44)))
   (empty-env))

(val-of-cbv
   '((lambda (swap)
       ((lambda (a)
          ((lambda (b)
             (begin2
              ((swap a) b)
              a)) 44)) 33))
     (lambda (x)
       (lambda (y)
         ((lambda (temp)
            (begin2
             (set! x y)
             (set! y temp))) x))))
   (empty-env))

(define cons-test
    '(let ((fix (lambda (f)
                 ((lambda (x) (f (lambda (v) ((x x) v))))
                  (lambda (x) (f (lambda (v) ((x x) v))))))))
        (let ((map (fix (lambda (map)
                          (lambda (f)
                            (lambda (l)
                               (if (null? l)
                                   '()
                                   (cons^ (f (car^ l))
                                          ((map f) (cdr^ l))))))))))
          (let ((take (fix (lambda (take)
                             (lambda (l)
                               (lambda (n)
                                 (if (zero? n)
                                     '()
                                      (cons (car^ l) 
                                            ((take (cdr^ l)) (sub1 n))))))))))
            ((take ((fix (lambda (m)
                           (lambda (i)
                             (cons^ 1 ((map (lambda (x) (add1 x))) (m i)))))) 0)) 5)))))
(val-of-cbv cons-test (empty-env))

