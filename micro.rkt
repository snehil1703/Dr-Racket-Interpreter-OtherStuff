#lang racket

;; Term :: Sym | Bool | Nil | Var | (Term . Term)

#| Term -> Bool |#
(define (var? x) (number? x))
(define (var x) x)

#| Var X Term X Subst -> Maybe Subst |#
(define (ext-s x u s)
  (cond
    ((occurs? x u s) #f)
    (else `((,x . ,u) . ,s))))

#| Term X Subst -> Term |#
(define (find u s)
  (let ((pr (and (var? u) (assv u s))))
    (if pr (find (cdr pr) s) u)))

#| Var X Term X Subst -> Bool |# 
(define (occurs? x u s)
  (cond
    ((var? u) (eqv? x u))
    ((pair? u) (or (occurs? x (find (car u) s) s)
                   (occurs? x (find (cdr u) s) s)))
    (else #f)))

#| Term X Term X Subst -> Maybe Subst |#
(define (unify u v s)
  (cond
    ((eqv? u v) s)
    ((var? u) (ext-s u v s))
    ((var? v) (ext-s v u s))
    ((and (pair? u) (pair? v))
     (let ((s^ (unify (find (car u) s) (find (car v) s) s)))
       (if s^ (unify (find (cdr u) s^) (find (cdr v) s^) s^) #f)))
    (else #f)))

;; State :: Subst X Counter
;; Goal :: State -> Stream 

#| Term X Term -> Goal |# 
(define ((== u v) s/c)
  (let ((s (car s/c)))
    (let ((s^ (unify (find u s) (find v s) s)))
      (if s^ (list `(,s^ . ,(cdr s/c))) '()))))

#| (Var -> Goal) -> Goal |#
(define ((call/fresh f) s/c)
  (let ((c (cdr s/c)))
    ((f (var c)) `(,(car s/c) . ,(add1 c)))))

#| Goal -> Stream |# 
(define (call/initial-state n g)
  (take n (pull (g '(() . 0)))))

(define (pull $) (if (promise? $) (pull (force $)) $))

(define (take n $)
  (cond
    ((null? $) '())
    ((and n (zero? (sub1 n))) (list (car (pull $))))
    (else (cons (car $)
                (take (and n (sub1 n))
                      (pull (cdr $)))))))

#| Goal X Goal -> Goal |#
(define ((disj g1 g2) s/c) 
  ($append (g1 s/c) (g2 s/c)))

#| Goal X Goal -> Goal |#
(define ((conj g1 g2) s/c)
  ($append-map (g1 s/c) g2))

#| Stream X Stream -> Stream |#
(define ($append $1 $2)
  (cond
    ((null? $1) $2)
    ((promise? $1) (delay/name ($append $2 (force $1))))
    (else (cons (car $1) ($append (cdr $1) $2)))))

#| Stream X Goal -> Stream |# 
(define ($append-map $ g)
  (cond
    ((null? $) '())
    ((promise? $) (delay/name ($append-map (force $) g)))
    (else ($append (g (car $)) ($append-map (cdr $) g)))))

(define-syntax-rule (define-relation (defname . args) g)
  (define ((defname . args) s/c)
    (delay/name (g s/c))))

(define-relation (cats x)
  (disj
   (cats x)
   (== x 'cat)))

(define-relation (append x y z)
  (disj
   (conj
    (== x '()) (== y z))
   (call/fresh
    (λ (a)
      (call/fresh
       (λ (d)
         (conj
           (== x `(,a . ,d))
           (call/fresh 
            (λ (res)
              (conj
                (== z `(,a . ,res))
                (append d y res)))))))))))
