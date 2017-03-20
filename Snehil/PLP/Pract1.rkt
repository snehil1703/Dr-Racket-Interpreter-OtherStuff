#lang racket

(define ^
  (lambda (n1 n2)
    (cond
      ((zero? n2) 1)
      (else
       (* n1 (^ n1 (sub1 n2)))))))


\Uparrow