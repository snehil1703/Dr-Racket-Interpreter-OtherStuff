#lang racket

;Question 1

(define (countdown s)
  (cons s
        (cond
          [(zero? s) '()]
          [else (countdown (sub1 s))])))

;Question 2

(define (insertR s1 s2 ls)
  (cond
    [(null? ls) '()]
    [(eqv? s1 (car ls)) (cons (car ls) (insertR s1 s2 (cons s2 (cdr ls))))]
    [else (cons (car ls) (insertR s1 s2 (cdr ls)))]))

;Question 3

(define (remv-1st s ls)
  (cond
    [(null? ls) '()]
    [(eqv? s (car ls)) (remv-1st '() (cdr ls))]
    [else (cons (car ls) (remv-1st s (cdr ls)))]))

;Question 4

(define (count-?s ls)
  (cond
    [(null? ls) 0]
    [(eqv? '? (car ls)) (add1 (count-?s (cdr ls)))]
    [else (count-?s (cdr ls))]))

;Question 5

(define (filter s ls)
  (cond
    [(null? ls) '()]
    [(s (car ls)) (cons (car ls) (filter s (cdr ls)))]
    [else (filter s (cdr ls))]))

;Question 6

(define (zip ls1 ls2)
  (cond
    [(null? ls1) '()]
    [(null? ls2) '()]
    [else (cons (cons (car ls1) (car ls2)) (zip (cdr ls1) (cdr ls2)))]))

;Question 7

(define (map s ls)
  (cond
    [(null? ls) '()]
    [else (cons (s (car ls)) (map s (cdr ls)))]))

;Question 8

(define (append ls1 ls2)
  (cond
    [(null? ls1) ls2]
    [else (cons (car ls1) (append (cdr ls1) ls2))]))

;Question 9 - PENDING

(define (reverse ls)
  (cond
    [(null? ls) '()]
    [else (append (reverse (cdr ls)) (list (car ls)))]))  

;Question 10

(define (fact s)
  (cond
    [(zero? s) 1]
    [else (* s (fact (sub1 s)))]))

;Question 11

(define (member-?* ls)
  (cond
    [(null? ls) #f]
    [(eqv? '? (car ls)) #t]
    [(pair? (car ls)) (member-?* (append (car ls) (cdr ls)))]
    [else (member-?* (cdr ls))]))

;Question 12

(define (fib s)
  (cond
    [(eqv? s 0) 0]
    [(eqv? s 1) 1]
    [else (+ (fib (- s 1)) (fib (- s 2)))]))

;Question 13

;((w .(x . ())) . (y . ((z . ()))))

;Question 14 - PENDING

;(define (^ s1 s2)
; (cond
;  [(zero? s2) 1]
; [else (* s1 (^ s1 (sub1 s2)))]))

(define (binary->natural ls)
  (cond
    [(null? ls) 0]
    [(eqv? (car ls) 0) ]
    ;;[else ]
    ))

;Question 15

(define (minus s1 s2)
  (cond
    [(zero? s2) s1]
    [(> s2 0) (sub1 (minus s1 (sub1 s2)))]
    [else (add1 (minus s1 (add1 s2)))]))

;Question 16

(define (div s1 s2)
  (cond
    [(zero? s2) '(CANNOT DIVIDE BY ZERO. BAD DATA)]
    [(zero? s1) 0]
    [else (add1 (div (- s1 s2) s2))]))

;Question 17 - PENDING

(define (append-map s ls)
  (cond
    [(null? ls) '()]
    [else (append (map s (list (car ls))) (append-map s (cdr ls)))]))
    ;[else (append (list (s (car ls))) (append-map s (cdr ls)))]))

;Question 18

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

;Question 19 - PENDING

;(define (power_set ls)
; (cons ls
;      (cond
;       [(null? ls) '()]
;      [else (cons (car ls) (power_set (cdr ls)))])))
