#lang racket

(define m 'ns)
(define n 'ns)
(define k 'ns)
(define v 'ns)
(define ls 'ns)
(define a 'ns)
(define ns 'ns)

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
  (lambda () ;; k v
    (match k
      [`(empty-k) v]
      [`(ack-k ,m^ ,k^) (begin
                          (set! m (sub1 m^))
                          (set! n v)
                          (set! k k^)
                          (ack))]
      [`(depth-outer-k ,ls^ ,k^) (begin
                                   (set! ls (cdr ls^))
                                   (set! k (depth-inner-k v k^))
                                   (depth))]
      [`(depth-inner-k ,l^ ,k^) (let ((l (add1 l^)))
                                  (if (< l v)
                                      (begin
                                        (set! k k^)
                                        (apply-k))
                                      (begin
                                        (set! k k^)
                                        (set! v l)
                                        (apply-k))))]
      [`(fact-k ,k^ ,n^) (begin
                           (set! k k^)
                           (set! v (* n^ v))
                           (apply-k))]
      [`(pascal-inner-k ,m^ ,a^ ,k^) (v
                                      (add1 m^)
                                      a^
                                      (pascal-middle-k a^ k^))]
      [`(pascal-middle-k ,a^ ,k^) (begin
                                    (set! k k^)
                                    (set! v (cons a^ v))
                                    (apply-k))]
      [`(pascal-outer-k ,k^) (v 1 0 k^)]
      )))
      
;1

(define ack
  (lambda () ;; m n k
    (cond
      [(zero? m) (begin
                   (set! v (add1 n))
                   (apply-k))]
      [(zero? n) (begin
                   (set! m (sub1 m))
                   (set! n 1)
                   (ack))]
      [else (begin
              (set! n (sub1 n))
              (set! k (ack-k m k))
              (ack))]
      )))

(define ack-k
  (lambda (m^ k^)
    `(ack-k ,m^ ,k^)
    ))

(define ack-reg-driver
  (lambda (m^ n^)
    (begin
      (set! m m^)
      (set! n n^)
      (set! k (empty-k))
      (ack))
    ))

;2

(define depth
  (lambda () ;; ls k
    (cond
      [(null? ls) (begin
                    (set! v 1)
                    (apply-k))]
      [(pair? (car ls)) (begin
                          (set! ls (car ls))
                          (set! k (depth-outer-k ls k))
                          (depth))]
      [else (begin
              (set! ls (cdr ls))
              (depth))]
      )))

(define depth-outer-k
  (lambda (ls^ k^)
    `(depth-outer-k ,ls^ ,k^)
    ))
    
(define depth-inner-k
  (lambda (l^ k^)
    `(depth-inner-k ,l^ ,k^)
    ))

(define depth-reg-driver
  (lambda (ls^)
    (begin
      (set! ls ls^)
      (set! k (empty-k))
      (depth))
    ))

;3

(define fact
  (lambda () ;; n k
    ((lambda (fact)
       (fact fact))
     (lambda (fact) ;; n k
       (cond
         [(zero? n) (begin
                      (set! v 1)
                      (apply-k))]
         [else (begin
                 (set! k (fact-k k n))
                 (set! n (sub1 n))
                 (fact fact))]
         ))
     )))

(define fact-k
  (lambda (k^ n^)
    `(fact-k ,k^ ,n^)
    ))

(define fact-reg-driver
  (lambda (n^)
    (begin
      (set! n n^)
      (set! k (empty-k))
      (fact))
    ))

;4

(define pascal
  (lambda () ;; n k
    (letrec ((pascal
           (lambda () ;; pascal k
             (begin
               (set! k k)
               (set! v (lambda (m a k^) ;; 
                         (cond
                           [(> m n) (begin
                                      (set! k k^)
                                      (set! v '())
                                      (apply-k))]
                           [else (let ((a (+ a m)))
                                   (begin
                                     (set! k (pascal-inner-k m a k^))
                                     (pascal)))])))
               (apply-k)))))
      (begin
        (set! k (pascal-outer-k k))
        (pascal)))
    ))

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

(define pascal-reg-driver
  (lambda (n^)
    (begin
      (set! n n^)
      (set! k (empty-k))
      (pascal))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Brainteaser

(define apply-k-fib
  (lambda (k v)
    (match k
      [`(fib-inner-k ,x^ ,k^) (lambda () (apply-k-fib k^ (+ x^ v)))]
      [`(fib-outer-k ,n^ ,k^) (lambda () (fib (sub1 n^) (fib-inner-k v k^)))]
      [`(empty-k-fib ,jumpout) (lambda () (jumpout v))]
      )))

(define empty-k-fib
  (lambda (jumpout)
    `(empty-k-fib ,jumpout)
    ))

(define fib
  (lambda (n k)
    (cond
      [(< n 1) (lambda () (apply-k-fib k 1))]
      [(< n 2) (lambda () (apply-k-fib k 1))]
      [else (lambda () (fib (sub1 (sub1 n)) (fib-outer-k n k)))]
      )))

(define fib-inner-k
  (lambda (x^ k^)
    `(fib-inner-k ,x^ ,k^)
    ))

(define fib-outer-k
  (lambda (n^ k^)
    `(fib-outer-k ,n^ ,k^)
    ))

(define rampoline
  (lambda (func1 func2 func3)
    (begin
      (set! ns (random 3))
      (cond
        [(= ns 0) (rampoline (func1) func2 func3)]
        [(= ns 1) (rampoline func1 (func2) func3)]
        [(= ns 2) (rampoline func1 func2 (func3))]
        ;[(= ns 3) (rampoline func1 func2 (func3))]
        ;[(= ns 4) (rampoline func1 func2 (func3))]
        ;[(= ns 5) (rampoline func1 func2 (func3))]
        ;[(= ns 6) (rampoline func1 func2 (func3))]
        ;[(= ns 7) (rampoline func1 func2 (func3))]
        ;[(= ns 8) (rampoline func1 func2 (func3))]
        ;[(= ns 9) (rampoline func1 func2 (func3))]
        ))
    ))

(define fib-ramp-driver
  (lambda (n1 n2 n3)
    (call/cc
     (lambda (jumpout)
       (rampoline
        (lambda ()
          (fib n1 (empty-k-fib jumpout)))
	(lambda ()
          (fib n2 (empty-k-fib jumpout)))
        (lambda ()
          (fib n3 (empty-k-fib jumpout)))
        )))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(require "a8-student-tests.rkt")
;(test-file #:file-name "a8.rkt")

