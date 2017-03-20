#lang racket

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 1

(define binary-to-decimal
  (lambda (n)
    (binary-to-decimal-cps n (empty-k))))

(define binary-to-decimal-cps
  (lambda (n s)
    (cond
      [(null? n) (s 0)]
      [else (binary-to-decimal-cps (cdr n) (lambda (v)
                                             (s (+
                                                 (car n)
                                                 (* 2 v)))))]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 2

(define times
  (lambda (ls)
    (times-cps ls (empty-k))))

(define times-cps
  (lambda (ls ns)
    (cond
      [(null? ls) (ns 1)]
      [(zero? (car ls)) (ns 0)]
      [else (times-cps (cdr ls) (lambda (v)
                                  (ns (*
                                       (car ls)
                                       v))))]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 3

(define times-cps-shortcut
  (lambda (ls ns)
    (cond
      [(null? ls) (ns 1)]
      [(zero? (car ls)) 0]
      [else (times-cps (cdr ls) (lambda (v)
                                  (ns (*
                                       (car ls)
                                       v))))]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 4

(define plus
  (lambda (m)
    (lambda (n)
      (plus-cps m n (empty-k))
      )))
      
(define plus-cps
  (lambda (m n k)
    (k (+ m n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 5

(define remv-first-9*
  (lambda (ls)
    (remv-first-9*-cps ls (empty-k))))

(define remv-first-9*-cps
  (lambda (ls ns)
    (cond
      [(null? ls) (ns '())]
      [(pair? (car ls))
       (cond
         [(remv-first-9*-cps (car ls) (lambda (x) (equal? (car ls) x)))
          (remv-first-9*-cps (cdr ls) (lambda (x) (ns (cons (car ls) x))))]
         [else (remv-first-9*-cps (car ls) (lambda (x) (ns (cons x (cdr ls)))))]
         )]
      [(eqv? (car ls) '9) (ns (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (lambda (x) (ns (cons (car ls) x))))]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 6

(define count-syms*
  (lambda (ls)
    (count-syms*-cps ls (empty-k))))

(define count-syms*-cps
  (lambda (ls ns)
    (cond
      [(null? ls) (ns 0)]
      [(pair? (car ls)) (count-syms*-cps (cdr ls)
                                         (lambda (x)
                                           (ns
                                            (+
                                             (count-syms*-cps (car ls)
                                                              (lambda (y) y))
                                             x))))]
      [(symbol? (car ls)) (count-syms*-cps (cdr ls) (lambda (x) (ns (add1 x))))]
      [else (count-syms*-cps (cdr ls) (lambda (x) (ns x)))]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 7

(define cons-cell-count
  (lambda (ls)
    (cons-cell-count-cps ls (empty-k))))

(define cons-cell-count-cps
  (lambda (ls ns)
    (cond
      [(pair? ls)
       (cons-cell-count-cps (cdr ls)
                        (lambda (x)
                          (ns
                           (add1 (+
                            (cons-cell-count-cps (car ls)
                                             (lambda (y) y))
                            x)))))]
      [else (ns 0)]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 8

(define find
  (lambda (u s)
    (find-cps u s (empty-k))))

(define find-cps
  (lambda (u s ns)
    (let [(pr (assv u s))]
      (if pr
          (find-cps (cdr pr) s (lambda (x) x))
          u)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 9

(define ack
  (lambda (m n)
    (ack-cps m n (empty-k))))

(define ack-cps
  (lambda (m n s)
    (cond
      [(zero? m) (s (add1 n))]
      [(zero? n) (ack-cps (sub1 m) 1 (lambda (x) (s x)))]
      [else (ack-cps (sub1 m)
                     (ack-cps m (sub1 n)
                              (lambda (y) y))
                     (lambda (x) (s x)))]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 10

(define fib
  (lambda (n)
    (fib-cps n (empty-k))))

(define fib-cps
  (lambda (n ns)
    ((lambda (fib ns)
       (fib fib n ns))
      (lambda (fib n ns)
       (cond
	 [(zero? n) (ns 0)]
	 [(= 1 n) (ns 1)]
         [else (fib fib (sub1 n) (lambda (x)
                                   (fib fib (sub1 (sub1 n)) (lambda (y)
                                                                 (ns
                                                                  (+
                                                                  x
                                                                  y)))
                                    ))
                                   )]
         )
       )
      ns)
     )
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 11

(define unfold-cps
  (lambda (p f g seed ns)
    ((lambda (h ns)
       (h h (lambda (x) (x seed '() ns))))
     (lambda (h ns)
       (ns
        (lambda (seed ans ns)
          (p seed (lambda (x)
                    (if x
                        (ns ans)
                        (h h (lambda (y)
                               (g seed (lambda (z)
                                         (f seed (lambda (w)
                                                   (y z (cons
                                                         w
                                                         ans) ns)))))))))))
        )
       )
     ns)
    ))

(define null?-cps
    (lambda (ls k)
      (k (null? ls))))
(define car-cps
    (lambda (pr k)
      (k (car pr))))
(define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 12

(define empty-s
  (lambda ()
    '()))

(define unify
  (lambda (u v s)
    (unify-cps u v s (empty-k))
    ))

(define unify-cps
  (lambda (u v s ns)
    (cond
      [(eqv? u v) (ns s)]
      [(number? u) (ns `((,u . ,v) . ,s))]
      [(number? v) (unify-cps v u s (lambda(x) (ns x)))]
      [(and (pair? u) (pair? v))
       (unify-cps (find (car u) s) (find (car v) s) s (lambda (x)
                                                        (if x
                                                            (unify-cps (find (cdr u) s) (find (cdr v) s) x (lambda (y) (ns y)))
                                                            #f))
                  )]
      [else (ns #f)]
      )))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 13

(define M-cps
  (lambda (f ns)
    (ns
     (lambda (ls ns)
      (cond
        [(null? ls) (ns '())]
        [else (f (car ls) (lambda (x)
                            (M-cps f (lambda (y)
                                       (y (cdr ls) (lambda (z)
                                                     (ns (cons x z))))))))]
        )
       )
     )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 14

(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n))) (empty-k)) '(1 2 3 4 5) (empty-k)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 15

(define strange-cps
  (lambda (x ns)
    ((lambda (g ns)
       (ns (lambda (x ns) (g g ns))))
     (lambda (g ns)
       (ns (lambda (x ns) (g g ns))))
     ns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 16

(define use-of-strange-cps
  ((((strange-cps 5 (empty-k)) 6 (empty-k)) 7 (empty-k))
   (lambda (x)
     (((x 8) 9) 10)) (empty-k))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Question 17

(define why-cps
  (lambda (f ns)
    ((lambda (g ns)
       (f (lambda (x ns)
            (g g (lambda (y) (y x ns)))) ns))
     (lambda (g ns)
       (f (lambda (x ns)
            (g g (lambda (y) (y x ns)))) ns))
     )
    ns)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
