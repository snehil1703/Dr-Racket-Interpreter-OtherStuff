#lang racket
(require racket/trace) ;; use this if you want to trace stuff
;; http://docs.racket-lang.org/reference/debugging.html

;; some historical stuff we've talked
;; https://en.wikipedia.org/wiki/Cons
;; https://en.wikipedia.org/wiki/CAR_and_CDR

;; Lisp was originally implemented on the IBM 704 computer, in the late 1950s. The 704 hardware had special support for splitting a 36-bit machine word into four parts, an "address part" and "decrement part" of 15 bits each and a "prefix part" and "tag part" of three bits each.

;; car (short for "Contents of the Address part of Register number"),
;; cdr ("Contents of the Decrement part of Register number"),

;; if you are an algorithm guy, then it's just a linked list.

;; function definition, lambda, and cond
;; (cond
;;   [Q A]
;;   [Q A]
;;   ...
;;   [else default-answer])

;; natural recursion
(define factorial
  (lambda (n)
    (cond
      [(zero? n) 1]
      [else (* n (factorial (sub1 n)))])))

;; (rember 'q '(a b q c))
;; > '(a b c)

;; this works on a flat list
(define rember
  (λ (x ls)
    (cond
      [(null? ls) '()]
      [(eqv? x (car ls)) (rember x (cdr ls))]
      [else (cons (car ls) (rember x (cdr ls)))])))

;; how about a non-flat list?
;; after this course you will be an expert on linked-lists
(define rember*
  (lambda (x ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls)) (cons (rember* x (car ls)) (rember* x (cdr ls)))]
      [(eqv? x (car ls)) (rember* x (cdr ls))]
      [else (cons (car ls) (rember* x (cdr ls)))])))

;; The Fourth Commandment, from _The Little Schemer_
;; - Always change at least one argument while recurring.
;; - ...
;; - It must be changed to be closer to termination.

;; plus. the hard way
(define plus
  (lambda (n1 n2) ;; 2 numbers
    (cond
      [(zero? n2) n1]
      [else (add1 (plus n1 (sub1 n2)))])))

(define times
  (lambda (n1 n2)
    (cond
      [(zero? n2) 0]
      [else (plus n1 (times n1 (sub1 n2)))])))

(define ^^ ;; a.k.a. power
  (lambda (n1 n2)
    (cond
      [(zero? n2) 1]
      [else (times n1 (^^ n1 (sub1 n2)))]))) ;; hey the pattern never changes


;; some extra notes from Fall 2014
;; Rule #0
;; Stick simple things (tools, principles) in your head well — So that you will be able to solve hard problems smoothly

;; Why people love Lisp
;; (cat dog) :: this is a program
;; '(cat dog) :: this is data

(define member?
  (lambda (a set)
    (cond
      [(null? set) #f]
      [(eqv? (car set) a) #t]
      [else (member? a (cdr set))])))

;; (member? a (cdr set)) ;; natural recursion (shrinking the elements)

