#lang racket

#|
1. Make a tail-recursion
2. CPS it
3. Add apply-k
4. Make continuations RI
5. Alias continuation constructor variable names
6. Make continuations data structures
7. let* for a-normal form
8. Create global variables, set!s out the wazoo
|#

(define empty-k
  (lambda ()
    `(empty-k)
    ))

(define apply-k
  (lambda (k v)
    (match k
      [`(empty-k) v]
      [`(ack-k ,m^ ,k^) (ack (sub1 m^) v k^)]
      [`(depth-outer-k ,ls^ ,k^) (depth (cdr ls^)
                                        (depth-inner-k v k^))]
      [`(depth-inner-k ,l^ ,k^) (let ((l (add1 l^)))
                                  (if (< l v)
                                      (apply-k k^ v)
                                      (apply-k k^ l)))]
      [`(fact-k ,k^ ,n^) (apply-k k^ (* n^ v))]
      [`(pascal-inner-k ,m^ ,a^ ,k^) (v
                                      (add1 m^)
                                      a^
                                      (pascal-middle-k a^ k^))]
      [`(pascal-middle-k ,a^ ,k^) (apply-k k^ (cons a^ v))]
      [`(pascal-outer-k ,k^) (v 1 0 k^)]
      [`(fib-inner-k ,x^ ,k^) (apply-k k^ (+ x^ v))]
      [`(fib-outer-k ,n^ ,k^) (fib
                               (sub1 n^)
                               (fib-inner-k v k^))]
      )))
    
;1

(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (apply-k k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else (ack m (sub1 n) (ack-k m k))])))

(define ack-k
  (lambda (m^ k^)
    `(ack-k ,m^ ,k^)
    ))

;2

(define depth
  (lambda (ls k)
    (cond
      [(null? ls) (apply-k k 1)]
      [(pair? (car ls))
       (depth (car ls) (depth-outer-k ls k))]
      [else (depth (cdr ls) k)])))

(define depth-outer-k
  (lambda (ls^ k^)
    `(depth-outer-k ,ls^ ,k^)
    ))
    
(define depth-inner-k
  (lambda (l^ k^)
    `(depth-inner-k ,l^ ,k^)
    ))

;3

(define fact
  (lambda (n k)
    ((lambda (fact k)
       (fact fact n k))
     (lambda (fact n k)
       (cond
         [(zero? n) (apply-k k 1)]
         [else (fact fact (sub1 n) (fact-k k n))]))
     k)))

(define fact-k
  (lambda (k^ n^)
    `(fact-k ,k^ ,n^)
    ))
    
;4

(define pascal
  (lambda (n k)
    (let ((pascal
           (lambda (pascal k)
             (apply-k k (lambda (m a k)
		  (cond
		    [(> m n) (apply-k k '())]
		    [else (let ((a (+ a m)))
			    (pascal pascal (pascal-inner-k m a k)))]))
                ))))
      (pascal pascal (pascal-outer-k k)))))

(define pascal-inner-k
  (lambda (m^ a^ k^)
    `(pascal-inner-k ,m^ ,a^ ,k^)
    ))

(define pascal-middle-k
  (lambda (a^ k^)
    `(pascal-middle-k ,a^ ,k^)
    ))

(define pascal-outer-k
  (lambda (k^)
    `(pascal-outer-k ,k^)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Brainteaser

(define fib
  (lambda (n k)
    (cond
      [(= n 0) (apply-k k 1)]
      [(= n 1) (apply-k k 1)]
      [else (fib
             (sub1 (sub1 n))
             (fib-outer-k n k)
             )]
      )))

(define fib-inner-k
  (lambda (x^ k^)
    `(fib-inner-k ,x^ ,k^)
    ))

(define fib-outer-k
  (lambda (n^ k^)
    `(fib-outer-k ,n^ ,k^)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(ack 2 2 (empty-k))
;7

;(depth '(1 (2 (3 (4)))) (empty-k))
;4

;(fact 5 (empty-k))
;120

;(pascal 6 (empty-k))
;(1 3 6 10 15 21 28 36 45 55)

;(fib 4 (empty-k))
;5