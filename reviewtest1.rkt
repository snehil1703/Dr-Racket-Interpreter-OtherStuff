#lang racket

;; Questions from CSCI-B521 Test 1 Review

;; Pairs and Lists
(eqv? '(a b) '(a b . ())) ;; => #f, because it's not the same value
(equal? '(a b) '(a b . ())) ;; => #t

;; this only works for lists/nested lists
(define list-equal?
  (λ (l1 l2)
    (cond
      [(and (null? l1) (null? l2)) #t]
      [(and (pair? (car l1))
            (pair? (car l2)))
       (and (list-equal? (car l1) (car l2))
            (list-equal? (cdr l1) (cdr l2)))]
      [else (and (eqv? (car l1) (car l2))
                 (list-equal? (cdr l1) (cdr l2)))])))
(list-equal? '(a b) '(a b . ())) ;; => #t, they are the same.

(define omega-loop
  (λ ()
    (let loop ([ω '(λ (x) (x x))])
      (display ((λ (x) `(,x ,x)) ω))
      (printf " evaluates to: \n")
      (omega-loop))))

;; (fact 5), using the Y Combinator
((((λ (y) (λ (f) (f (λ (x) (((y y) f) x)))))
   (λ (y) (λ (f) (f (λ (x) (((y y) f) x))))))
  (λ (fact)
    (λ (n)
      (if (zero? n) 1
          (* n (fact (sub1 n))))))) 5)

;; (member 3 '(1 2 3 4 5)), using the Y Combinator
(((((λ (y) (λ (f) (f (λ (x) (((y y) f) x)))))
    (λ (y) (λ (f) (f (λ (x) (((y y) f) x))))))
   (λ (member)
     (λ (x)
       (λ (ls)
         (if (null? ls) #f
             (or (eqv? (car ls) x)
                 ((member x) (cdr ls)))))))) 3) '(1 2 3 4 5))

(((lambda (f) (lambda (x) (f x))) (lambda (n) (- n 1))) 121)

((λ (x)
   (let ([y 115])
     (+ y x))) 5)
