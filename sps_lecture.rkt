#lang racket
(require racket/match)
;; (let-values (((a b) (values 5 6)))
;;   (+ a b))

(define sum-ls
  (lambda (ls)
    (if (null? ls)
        0
        (+ (car ls) (sum-ls (cdr ls))))))

(define max-ls
  (lambda (ls)
    (if (null? ls)
        0
        (max (car ls) (max-ls (cdr ls))))))

(define sumXmax
  (lambda (ls)
    (if (null? ls)
        (values 0 0)
        (let-values (((sum max-val) (sumXmax (cdr ls))))
          (values (+ sum (car ls)) (max max-val (car ls)))))))

(define powerXpartials
  (lambda (x n store)
    (cond
      ((zero? n) (values 1 store))
      ((= n 1) (values x store))
      ((odd? n)
       (let-values (((pow store) (powerXpartials x (sub1 n) store)))
         (values (* x pow) `(,pow . ,store))))
      ((even? n)
       (let ((nhalf (/ n 2)))
         (let-values (((pow store) (powerXpartials x nhalf store)))
           (values (* pow pow) `(,pow . ,store))))))))

(define fib
  (lambda (n)
    (if (< n 2)
        n
        (+ (fib (sub1 n)) (fib (- n 2))))))

(define fib-sps
  (lambda (n store)
    (cond
      ((assv n store) =>
       (lambda (pr) (values (cdr pr) store)))
      ((zero? n) (values n `((,n . 0) . ,store))) ; -> (cons (cons n 0) store)
      ((= 1 n) (values n `((,n . 1) . ,store))) ; -> (cons (cons n 1) store)
      (else
       (let*-values (((u store) (fib-sps (sub1 (sub1 n)) store))
                     ((v store) (fib-sps (sub1 n) store)))
         (values (+ u v) `((,n . ,(+ u v)) . ,store)))))))

(define fib-2
  (lambda (n)
    (let-values (((ans store) (fib-sps n '())))
      ans)))

(define empty-store
  (lambda ()
    (hash)))
(define update-store
  (lambda (store loc val)
    (hash-set store loc val)))
(define ref-store
  (lambda (store loc)
    (hash-ref store loc)))

(define empty-env (lambda () '()))
(define extend-env ; (snoc)
  (lambda (x ls)
    (append ls (list x))))
(define apply-env ; (get-loc)
  (lambda (ls x)
    (if (eqv? x (car ls)) 0 (add1 (apply-env (cdr ls) x)))))

(define val-of
  (lambda (exp env store)
    (match exp
      ((? number?) (values exp store))
      ((? symbol?) (values (ref-store store (apply-env env exp)) store))
      (`(lambda (,x) ,body)
       (values (lambda (a)
                 (let* ((loc (length env))
                        (store (update-store store loc a))
                        (env (extend-env x env)))
                   (val-of body env store)))
               store))
      (`(set! ,x ,rhs)
       (let-values (((ans store) (val-of rhs env store)))
         (values (void)
                 (update-store store (apply-env env x) ans))))
      (`(begin ,e0 ,e1)
       (let*-values (((_ store) (val-of e0 env store))
                     ((ans store) (val-of e1 env store)))
         (values ans store)))
      (`(,rat ,rand)
       (let*-values (((arg store) (val-of rand env store))
                     ((clos _) (val-of rat env store)))
         (clos arg))))))





