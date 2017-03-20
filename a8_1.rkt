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
      [`(ack-k ,m^ ,k^) (let* ((m (sub1 m^))
                               (n v)
                               (k k^))
                          (ack m n k))]
      [`(depth-outer-k ,ls^ ,k^) (let* ((ls (cdr ls^))
                                        (k (depth-inner-k v k^)))
                                   (depth ls k))]
      [`(depth-inner-k ,l^ ,k^) (let ((l (add1 l^)))
                                  (if (< l v)
                                      (let* ((k k^))
                                        (apply-k k v))
                                      (let* ((k k^)
                                             (v l))
                                        (apply-k k v))))]
      [`(fact-k ,k^ ,n^) (let* ((k k^)
                                (v (* n^ v)))
                           (apply-k k v))]
      [`(pascal-inner-k ,m^ ,a^ ,k^) (v
                                      (add1 m^)
                                      a^
                                      (pascal-middle-k a^ k^))]
      [`(pascal-middle-k ,a^ ,k^) (let* ((k k^)
                                         (v (cons a^ v)))
                                    (apply-k k v))]
      [`(pascal-outer-k ,k^) (v 1 0 k^)]
      [`(fib-inner-k ,x^ ,k^) (let* ((k k^)
                                     (v (+ x^ v)))
                                (apply-k k v))]
      [`(fib-outer-k ,n^ ,k^) (let* ((k (fib-inner-k v k^))
                                     (n (sub1 n^)))
                                (fib n k))]
      )))

;1

(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (let* ((v (add1 n)))
                   (apply-k k v))]
      [(zero? n) (let* ((m (sub1 m))
                        (n 1))
                   (ack m n k))]
      [else (let* ((n (sub1 n))
                   (k (ack-k m k)))
              (ack m n k))]
      )))

(define ack-k
  (lambda (m^ k^)
    `(ack-k ,m^ ,k^)
    ))

;2

(define depth
  (lambda (ls k)
    (cond
      [(null? ls) (let* ((v 1))
                    (apply-k k v))]
      [(pair? (car ls)) (let* ((ls (car ls))
                               (k (depth-outer-k ls k)))
                          (depth ls k))]
      [else (let* ((ls (cdr ls)))
              (depth ls k))]
      )))

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
         [(zero? n) (let* ((v 1))
                      (apply-k k v))]
         [else (let* ((k (fact-k k n))
                      (n (sub1 n)))
                 (fact fact n k))]
         ))
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
             (let* ((k k)
                   (v (lambda (m a k)
                        (cond
                          [(> m n) (let* ((k k)
                                          (v '()))
                                     (apply-k k v))]
                          [else (let ((a (+ a m)))
                                  (let* ((pascal pascal)
                                         (k (pascal-inner-k m a k)))
                                    (pascal pascal k)))]))))
               (apply-k k v)))))
      (let* ((pascal pascal)
             (k (pascal-outer-k k)))
        (pascal pascal k)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Brainteaser

(define fib
  (lambda (n k)
    (cond
      [(= n 0) (let* ((k k)
                      (v 1))
                 (apply-k k v))]
      [(= n 1) (let* ((k k)
                      (v 1))
                 (apply-k k v))]
      [else (let* ((k (fib-outer-k n k))
                   (n (sub1 (sub1 n))))
              (fib n k))]
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

#|
(let* ((m 2)
       (n 2)
       (k (empty-k)))
  (ack m n k))
;7

(let* ((ls '(1 (2 (3 (4)))))
       (k (empty-k)))
  (depth ls k))
;4

(let* ((n 5)
       (k (empty-k)))
  (fact n k))
;120

(let* ((n 10)
       (k (empty-k)))
  (pascal n k))
;(1 3 6 10 15 21 28 36 45 55)

(let* ((k (empty-k))
       (n 4))
  (fib n k))
;5
|#
