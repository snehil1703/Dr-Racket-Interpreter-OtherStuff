#lang racket

(define (countdown n)
  (if (>= n 0)
       (if (eqv? n 0)
           (cons n '())
           (cons n (countdown (sub1 n))))
      '()
      ))

;(countdown 5)

(define (insertR n s ls)
  (if (eqv? ls '())
      '()
      (if (eqv? (car ls) n)
          (cons (car ls) (cons s (insertR n s (cdr ls))))
          (cons (car ls) (insertR n s (cdr ls))))
      ))

;(insertR 'x 'y '(x z z x y x))

(define (remv-1st n ls)
  (if (eqv? ls '())
      '()
      (if (eqv? (car ls) n)
          (remv-1st '() (cdr ls))
          (cons (car ls) (remv-1st n (cdr ls))))
      ))

;(remv-1st 'x '(x y z x))

;(remv-1st 'y '(x y z y x))

(define (count-?s ns)
  (if (eqv? ns '())
      0
      (if (eqv? (car ns) '?)
          (add1 (count-?s (cdr ns)))
          (count-?s (cdr ns)))
      ))
      
;(count-?s '(? y z ? ?))

(define (filter n ls)
  (cond
    [(null? ls) '()]
    [else
     (if (n (car ls))
         (cons (car ls) (filter n (cdr ls)))
         (filter n (cdr ls)))]
    ))

;(filter even? '(1 2 3 4 5 6))

;(filter odd? '(1 2 3 4 5 6 7))

;(filter number? '(aas 1 3 r 42))

(define (zip n s)
  (if (or (null? n) (null? s))
      '()
      (cons (cons (car n) (car s)) (zip (cdr n) (cdr s)))
      ))

;(zip '(1 2 3) '(a b c))

;(zip '(1 2 3 4 5 6) '(a b c))

;(zip '(1 2 3 4) '(a b c d e f))

(define (map n ls)
  (cond
    [(null? ls) '()]
    [else
     (cons (n (car ls)) (map n (cdr ls)))]
    ))

;(map add1 '(1 2 3 4))

;(map sub1 '(1 2 3 4))

(define (append n s)
  (if (null? n)
      s
      (cons (car n) (append (cdr n) s)))
  )
  
;(append '(a b c d e f) '(1 2 3))

(define (reverse ns)
  (if (null? ns)
      '()
      (cons (reverse (cdr ns)) ns))
  )

(reverse '(a 3 x))
;(x 3 a)

(define (fact n)
  (cond
    [(zero? n) 1]
    [else (* n (fact (sub1 n)))]
    ))

;(fact 0)

;(fact 6)

(define (member-?* ns)
  (cond
    [(null? ns) #f]
    [(pair? (car ns))
     (if (or (member-?* (car ns)) (member-?* (cdr ns)))
         #t
         #f
         )]
    [(eqv? '? (car ns)) #t]
    [else (member-?* (cdr ns))]
    ))

;(member-?* '(a b c))

;(member-?* '(a ? c))

;(member-?* '((a ((d)) ((c) b ?))))

(define (fib ns)
  (cond
    [(eqv? ns 0) 0]
    [(eqv? ns 1) 1]
    [else (+ (fib (- ns 1)) (fib (- ns 2)))]
    ))

(define (binary->natural ns)
  (if (null? ns)
      0
      (+ (car ns) (* 2 (binary->natural (cdr ns))))
      ))

;(binary->natural '())

;(binary->natural '(0 0 1))

;(binary->natural '(0 0 1 1))

;(binary->natural '(1 1 1 1))

;(binary->natural '(1 0 1 0 1))

;(binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1))



;((w x) y (z))
;( (w . (x . () ) ) . (y . ( (z . () ) ) ) ) )



(define (minus s1 s2)
  (cond
    [(zero? s2) s1]
    [(> s2 0) (sub1 (minus s1 (sub1 s2)))]
    [else (add1 (minus s1 (add1 s2)))]))

;(minus 5 3)

;(minus 5 30)

;(minus 5 5)

(define (div s1 s2)
  (cond
    [(zero? s2) '(CANNOT DIVIDE BY ZERO. BAD DATA)]
    [(zero? s1) 0]
    [else (add1 (div (- s1 s2) s2))]))

;(div 25 5)

(define (joinit n s)
  (if (null? n)
      s
      (cons (car n) (joinit (cdr n) s)))
  )

(define (append-map n s)
  (cond
    [(null? s) '()] 
    [else (joinit
           (n (car s)) (append-map n (cdr s)))]
    ))

;(append-map countdown (countdown 5))

(define (chk s ls)
  (cond
    [(null? ls) 0]
    [(eqv? s (car ls)) 1]
    [else (chk s (cdr ls))]))

(define (set-difference ls1 ls2)
  (cond
    [(null? ls1) '()]
    [(eqv? (chk (car ls1) ls2) 1) (set-difference (cdr ls1) ls2)]
    [else (cons (car ls1) (set-difference (cdr ls1) ls2))]))

;(set-difference '(1 2 3 4 5) '(5 2 4 6 8))

(define (powerset ns)
  (cond
    [(null? ns) '()]
    [(null? (cdr ns)) ns]
    [else (cons
           ns
           (powerset (cdr ns))
           )]
    ))


;(car ns) (cdr ns)

(powerset '(3 2 1))
;((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ())
(powerset '())
;(())


(define plus
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1* n (plus n (sub1 m)))))))

(define add1*
  (lambda (n s)
    (add1 s)))

(define times
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (plus n (times n (sub1 m)))))))

(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (times n (^ n (sub1 m)))))))

(define ^^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (^ n (^^ n (sub1 m)))))))

(define ^^^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (^^ n (^^^ n (sub1 m)))))))

(define G
  (lambda (k)
    (letrec
      ((G^ (lambda (n m)
             (cond
               ((zero? m) (cond
                            ((zero? k) n)
                            ((zero? (sub1 k)) 0)
                            (else 1)))
               (else
                 (let ((builder (cond
                                  ((zero? k) add1*)
                                  ((zero? (sub1 k)) plus)
                                  ((zero? (sub1 (sub1 k))) times)
                                  (else (G (sub1 k))))))
                   (builder n (G^ n (sub1 m)))))))))
      G^)))

;((G 2) 3 2)

(define collatz
  (letrec
    ((odd-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
            (else (recur x))))))
     (even-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (even? x)) (collatz (/ x 2))) 
            (else (recur x))))))
     (one-case
       (lambda (recur)
         (lambda (x)
           (cond
            ((zero? (sub1 x)) 1)
            (else (recur x))))))
     (base
       (lambda (x)
         (error 'error "Invalid value ~s~n" x))))
    base
    ))

((lambda (x) (list x (list 'quote x)))
  '(lambda (x) (list x (list 'quote x))))

;((lambda (x) (list x (list 'quote x)))
 ; '(lambda (x) (list x (list 'quote x))))


;(equal? quine (eval quine))

;(equal? quine (eval (eval quine)))


;REVERSE

;POWERSET

;CARTESIAN PRODUCT ALSO

;ALSO COLLATZ CONJECTURE

;ALSO QUINE


























