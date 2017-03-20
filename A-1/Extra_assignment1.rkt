#lang racket

;; Question 1

(define countdown
  (lambda (n)
    (cond 
      [(zero? n) 0]
      [n (list* n (countdown (sub1 n)))])))

(define (count-down n)
  (cons n (if (zero? n) '()
              (count-down (sub1 n)))))

;Question 6

(define (zip ls1 ls2)
  (cond
     [(null? ls1) '()]
     [(null? ls2) '()]
     [else (cons (cons (car ls1) (car ls2)) (zip (cdr ls1) (cdr ls2)))]))