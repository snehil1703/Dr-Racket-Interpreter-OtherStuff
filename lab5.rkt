#lang racket

;; This is the Y combinator.
((λ (y) (λ (f) (f (λ (x) (((y y) f) x)))))
 (λ (y) (λ (f) (f (λ (x) (((y y) f) x))))))

#|

Using the skeleton below implement `member`.

The "normal" member function behaves like:
(member 3 '(1 2 3 4 5)) => #t
(member 1 '(1 2 3 4 5)) => #t
(member 6 '(1 2 3 4 5)) => #f

You're not allowed to use the name `my-member` inside your
implementation.
|#

(define (my-member x ls)
  (((((λ (y) (λ (f) (f (λ (x) (((y y) f) x)))))
      (λ (y) (λ (f) (f (λ (x) (((y y) f) x))))))
     (λ (member)
       (λ (x)
         (λ (ls)
           (if (null? ls) #f
               (or (eqv? (car ls) x)
                   ((member x) (cdr ls)))))))) x) ls))

;; Hint     :: Mind the parenthesis
;; Hint'    :: Don't create any free variables
;; Hint''   :: Taste of India has good curry. Japanese curry is good too. English curry is great.
;; Hint'''  :: It starts with (λ (member) ...) or whatever name
;; Hint'''' :: Ignore the Y Combinator. Be true to yourself.


(define (i-use-match exp)
  (match exp
    [`(,a . ,d) #:when (eqv? a 'tag) d]
    [else "sorry"]))

;; there are multiple ways to do this
(define (i-use-cond exp)
  (cond
    [(pair? exp)
     (let ([a (car exp)]
           [d (cdr exp)])
       (if (eqv? a 'tag) d "sorry"))]
    [else "sorry"]))

((λ (x) (and (pair? x) (+ (car x) (cdr x)))) 5)          ;; #f, no error
((λ (x) (and (pair? x) (+ (car x) (cdr x)))) (cons 3 4)) ;; 7, happiness

;;; Common environment structures
;;; Everything below represents the same context.

;; Function
(λ (y)
  (if (eqv? 'x y) 1
      ((λ (y) (if (eqv? 'y y) 2
                  ((λ (y) (if (eqv? 'a y) 4
                              ((λ (y) "error") y))) y))) y)))
;; A-List
'((x . 1) (y . 2) (a . 4))
;; List
'(x 1 y 2 a 4)




