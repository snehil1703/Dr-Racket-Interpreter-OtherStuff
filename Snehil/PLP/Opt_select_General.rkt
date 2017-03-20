 #lang racket

(define (G i)
  (lambda (x y)
    (cond
      [(zero? y)
       (cond
         [(zero? i) x]
         [(zero? (sub1 i)) 0]
         [else 1])]
      [else
       (cond
         [(zero? i) (add1 ((G i) x (sub1 y)))]
         [(zero? (sub1 i)) ((G 0) x ((G i) x (sub1 y)))]
         [else ((G (sub1 i)) x ((G i) x (sub1 y)))])])))