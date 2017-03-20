#lang racket

(define G
  (lambda (i)
    (lambda (x y)
    (cond
      [(zero? i) 
                   (cond
                     [(zero? y) x]
                     [else (add1 ((G 0) x(sub1 y)))])]
      [(zero? (sub1 i))
       (cond
         [(zero? y) 0]
         [else ((G 0) * (G 1) * (sub1 y))])]
      [else
       (cond
         [(zero? y) 1]
         [else
          ((G (sub1 1)) * ((G i) *(sub1 y)))])]))))