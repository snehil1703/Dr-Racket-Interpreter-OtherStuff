#lang racket
(require "mk.rkt")

(define apply-Go
  (lambda (G e t)
    (fresh (a G^)
      (== `(,a . ,G^) G)
      (fresh (aa da)
        (== `(,aa . ,da) a)
        (conde
          ((== aa e) (== da t))
          ((=/= aa e) (apply-Go G^ e t)))))))

(define !-
  (lambda (G e t)
    (conde
      ((numbero e) (== 'Nat t))
      ((== t 'Bool)
       (conde
         ((== #t e))
         ((== #f e))))
      ;Implementation of *
      ((fresh (ns1 ns2)
         (== `(* ,ns1 ,ns2) e)
         (== 'Nat t)
         (!- G ns1 'Nat)
         (!- G ns2 'Nat)))
      ;Implementation of NOT
      ((fresh (ns)
         (== `(not ,ns) e)
         (== 'Bool t)
         (!- G ns 'Bool)))
      ;Implementation of ZERO?
      ((fresh (ns)
         (== `(zero? ,ns) e)
         (== 'Bool t)
         (!- G ns 'Nat)))
      ;Implementation of SUB1
      ((fresh (ns)
         (== `(sub1 ,ns) e)
         (== 'Nat t)
         (!- G ns 'Nat)))
      ;Implementation of FIX
      ((fresh (ns)
         (== `(fix ,ns) e)
         (!- G ns `(,t -> ,t))))

      ; BRAINTEASER
      
      ;Implementation of CONS
      ((fresh (ns1 ns2)
         (== `(cons ,ns1 ,ns2) e)
         (fresh (t1 t2)
           (== `(pairof ,t1 ,t2) t)
           (!- G ns1 t1)
           (!- G ns2 t2))))

      ;Implementation of CAR
      ((fresh (ns)
         (== `(car ,ns) e)
         (fresh (t1 t2)
           (== t1 t)
           (!- G ns `(pairof ,t1 ,t2)))))

      ;Implementation of CDR
      ((fresh (ns)
         (== `(cdr ,ns) e)
         (fresh (t1 t2)
           (== t2 t)
           (!- G ns `(pairof ,t1 ,t2)))))
      
      ((fresh (ne1 ne2)
         (== `(+ ,ne1 ,ne2) e)
         (== 'Nat t)
         (!- G ne1 'Nat)
         (!- G ne2 'Nat)))
      ((fresh (teste anse elsee)
        (== `(if ,teste ,anse ,elsee) e)
        (!- G teste 'Bool)
        (!- G anse t)
        (!- G elsee t)))
      ((symbolo e) (apply-Go G e t))
      ((fresh (x b)
        (== `(lambda (,x) ,b) e)
        (symbolo x)
        (fresh (tx tb)          
          (== `(,tx -> ,tb) t)
          (!- `((,x . ,tx) . ,G) b tb))))
      ((fresh (e1 arg)
        (== `(,e1 ,arg) e)
        (fresh (targ)
          (!- G e1 `(,targ -> ,t))
          (!- G arg targ)))))))

#|
(run* (q) (!- '() #t q))
;(Bool)
(run* (q) (!- '() 17 q))
;(Nat)
(run* (q) (!- '() '(zero? 24) q))
;(Bool)
(run* (q) (!- '() '(zero? (sub1 24)) q))
;(Bool)
(run* (q) (!- '() '(not (zero? (sub1 24))) q))
;(Bool)
(run* (q)
    (!- '() '(zero? (sub1 (sub1 18))) q))
;(Bool)
(run* (q)
    (!- '()  '(lambda (n) (if (zero? n) n n)) q))
;((Nat -> Nat))
(run* (q)
    (!- '() '((lambda (n) (zero? n)) 5) q))
;(Bool)
(run* (q)
    (!- '() '(if (zero? 24) 3 4) q))
;(Nat)
(run* (q)
    (!- '() '(if (zero? 24) (zero? 3) (zero? 4)) q))
;(Bool)
(run* (q)
    (!- '() '(lambda (x) (sub1 x)) q))
;((Nat -> Nat))
(run* (q)
    (!- '() '(lambda (a) (lambda (x) (+ a x))) q))
;((Nat -> (Nat -> Nat)))
(run* (q)
    (!- '() '(lambda (f)
               (lambda (x)
                 ((f x) x)))
         q))
;(((_.0 -> (_.0 -> _.1)) -> (_.0 -> _.1)))
(run* (q)
    (!- '() '(sub1 (sub1 (sub1 6))) q))
;(Nat)
(run 1 (q)
    (fresh (t)
      (!- '() '(lambda (f) (f f)) t)))
;()
(length (run 20 (q)
             (fresh (lam a b)
               (!- '() `((,lam (,a) ,b) 5) 'Nat)
               (== `(,lam (,a) ,b) q))))
;20
(length (run 30 (q) (!- '() q 'Nat)))
;30
(length (run 30 (q) (!- '() q '(Nat -> Nat))))
;30
(length (run 500 (q) (!- '() q '(Nat -> Nat))))
;500
;; At this point, stop and take a look at maybe the 500th 
;; program you generate
;; (last (run 500 (q) (!- '() q '(Nat -> Nat))))
;; You should be amazed at how quickly it's generating them.
;; If it isn't fast, consider reordering your clauses. 
(length (run 30 (q) (!- '() q '(Bool -> Nat))))
;30
(length (run 30 (q) (!- '() q '(Nat -> (Nat -> Nat)))))
;30
(length (run 100 (q)
             (fresh (e t)
               (!- '() e t)
               (== `(,e ,t) q))))
;100
(length (run 100 (q)
             (fresh (g e t)
               (!- g e t)
               (== `(,g ,e ,t) q))))
;100
(length
   (run 100 (q)
     (fresh (g v)
       (!- g `(var ,v) 'Nat)
       (== `(,g ,v) q))))
;100
(run 1 (q)
       (fresh (g)
	 (!- g
	      '((fix (lambda (!)
		       (lambda (n)
			 (if (zero? n)
			     1
			     (* n (! (sub1 n)))))))
		5)
	      q)))
;(Nat)
(run 1 (q)
       (fresh (g)
	 (!- g
	      '((fix (lambda (!)
		       (lambda (n)
			 (* n (! (sub1 n))))))
		5)
	      q)))
;(Nat)
(run* (q) (!- '() '(cons (zero? 1) (zero? 0)) q))
;((pairof Bool Bool))
(run* (q) (!- '() '(cons (zero? 1) (cons (zero? 1) (zero? 0))) q))
;((pairof Bool (pairof Bool Bool)))
(run* (t) (!- '() '(lambda (x) (cons x x)) t))
;((_.0 -> (pairof _.0 _.0)))
(run* (t) (!- '() '(lambda (x) (lambda (y) (cons (zero? x) (+ x y)))) t))
;((Nat -> (Nat -> (pairof Bool Nat))))

(run* (t) (!- '() '(lambda (x) (zero? (car x))) t))
;(((pairof Nat _.0) -> Bool))
(run* (t) (!- '() '((lambda (x) (zero? (car x))) (cons 0 1)) t))
;(Bool)
(run* (t) (!- '() '((lambda (x) (zero? (car x))) (cons 0 #f)) t))
;(Bool)
(run* (t) (!- '() '((lambda (x) (zero? (car x))) (cons #f 0)) t))
;()
;; a function that accepts a pair of anything and an Nat
(run* (t) (!- '() '(lambda (x) (zero? (cdr x))) t))
;(((pairof _.0 Nat) -> Bool))
(run* (t) (!- '() '((lambda (x) (zero? (cdr x))) (cons 0 1)) t))
;(Bool)
(run* (t) (!- '() '((lambda (x) (zero? (cdr x))) (cons 0 #f)) t))
;()
(run* (t) (!- '() '((lambda (x) (zero? (cdr x))) (cons #f 0)) t))
;(Bool)
|#

;(require "a11-student-tests.rkt")
;(test-file #:file-name "a11.rkt")