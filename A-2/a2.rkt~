#lang racket

;Question 1

(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
	   (if(zero? n)
              ls
              (cdr(nth-cdr (sub1 n))))
              )))
      (car (nth-cdr n)))))

;Question 2

(define (union ls1 ls2)
  (cond
    [(null? ls2) ls1]
    [(boolean? (memv (car ls2) ls1)) (union (cons (car ls2) ls1) (cdr ls2))]
    [else (union ls1 (cdr ls2))]))

;Question 3

(define (extend x pred)
  (lambda (y)
    (or
     (pred y)
     (eqv? x y)
     )
    )
  )
                            
;Question 4

(define (walk-symbol x s)
  (cond
    [(boolean? (assv x s)) x]
    [else
     (cond
       [(number? (cdr (assv x s))) (cdr (assv x s))]
       [(pair? (cdr (assv x s))) (cdr (assv x s))]
       [else (walk-symbol (cdr (assv x s)) s)]
     )]))

;(walk-symbol 'a '((a . 5)))
;(walk-symbol 'a '((a . 6) (a . 5))) 

;Question 5

(define (lambda->lumbda exp)
  (match exp
    [`(lambda (,x) ,newexp) `(lumbda (,x) ,(lambda->lumbda newexp))]
    [`(,exp1 ,exp2) `(,(lambda->lumbda exp1) ,(lambda->lumbda exp2))]
    [`,x x]
    ))

;Question 6

(define (var-occurs? z exp)
  (match exp
      [`,y #:when (symbol? y) (if (eqv? y z) #t #f)]
      [`(lambda (,y) ,body) (var-occurs? z body)]
      [`(,rator ,rand) (or (var-occurs? z rator) (var-occurs? z rand))]
    ))

;Question 7

(define (vars exp)
  (match exp
      [`,y #:when (symbol? y) `(,y)]
      [`(lambda (,y) ,body) (vars body)]
      [`(,rator ,rand) (append (vars rator) (vars rand))]
    ))

;Question 8

(define (unique-vars exp)
  (match exp
      [`,y #:when (symbol? y) `(,y)]
      [`(lambda (,y) ,body) (unique-vars body)]
      [`(,rator ,rand) (union (unique-vars rator) (unique-vars rand))]
    ))

;Question 9

(define (var-occurs-free? z exp)
  (match exp
      [`,y #:when (symbol? y) (if (eqv? y z) #t #f)]
      [`(lambda (,y) ,body) (if (eqv? z y)
                                #f
                                (var-occurs-free? z body))]
      [`(,rator ,rand) (or (var-occurs-free? z rator) (var-occurs-free? z rand))]
    ))

;Question 10

(define (var-occurs-bound? z exp)
  (match exp
      [`,y #:when (symbol? y) `()]
      [`(lambda (,y) ,body) (if (var-occurs-free? z body)
                                (if (eqv? z y)
                                    z
                                    (var-occurs-bound? z body)
                                    )
                                (var-occurs-bound? z body))]
                               
      [`(,rator ,rand) (append (var-occurs-bound? z rator)    
                           (var-occurs-bound? z rand)
                           )]
    ))

;(var-occurs-bound? 'x '(lambda (x) (lambda (x) x)))

;Question 11

(define (unique-free-vars exp)
  (match exp
      [`,y #:when (symbol? y) `(,y)]
      [`(lambda (,y) ,body) (if (var-occurs-free? y body)
                                (cons y (unique-free-vars body))
                                (unique-free-vars body))]
      [`(,rator ,rand) (union (unique-free-vars rator) (unique-free-vars rand))]
    ))

;Question 12

(define (unique-bound-vars exp)
  (match exp
      [`,y #:when (symbol? y) `(,y)]
      [`(lambda (,y) ,body) (if (var-occurs-bound? y body)
                                (cons y (unique-bound-vars body))
                                (unique-bound-vars body)
                                )]
      [`(,rator ,rand) (union (unique-free-vars rator) (unique-free-vars rand))]
    ))

; Question 13

(define lex
  (lambda (exp acc)
    (match exp
      [`,y #:when (symbol? y) (if (member y acc)
                                  (cons 'var
                                        (letrec ([position (lambda (n)
                                                       (if (eqv? (car n) y)
                                                           0
                                                           (add1 (position (cdr n)))))])
                                          (position acc)))
                                  '()
                                  )]
      [`(lambda (,x) ,body)
       (cons 'lambda
             (cons (lex body (cons x acc)) '()))]
      [`(,rator ,rand) (list (lex rator acc) (lex rand acc))]
      )))
                                                    
;(lex '(lambda (x) x) '())

;(lex '(lambda (y) (lambda (x) y)) '())

;(lex '(lambda (y) (lambda (x) (x y))) '())

;(lex '(lambda (x) (lambda (x) (x x))) '())

;(lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 

;(lex '(lambda (a)
 ;         (lambda (b)
  ;          (lambda (c)
   ;           (lambda (a)
    ;            (lambda (b)
     ;             (lambda (d)
      ;              (lambda (a)
       ;               (lambda (e)
        ;                (((((a b) c) d) e) a))))))))) '())









