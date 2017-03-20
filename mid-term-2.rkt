#lang racket

#| (define takef
  (lambda (f)
    (lambda (ls k)
      (cond
        [( null? ls) (k '())]
        [(f (car ls)) (cons (car ls) (( takef f) (cdr ls)))]
        [else (k '())]
        )))) |#

((lambda (f)
  (if (zero? 0)
      5
      f)
    ) (set-box! ((lambda (x) (x x)) (lambda (x) (x x)))))