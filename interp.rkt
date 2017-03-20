#lang racket

(define (lookup x vars vals)
  (match-let
    ([`(,y . ,vars^) vars]
     [`(,v . ,vals^) vals])
    (cond
      ((equal? y x) v)
      ((not (equal? x y)) (lookup x vars^ vals^)))))

(define (valof* exps vars vals)
  (match exps
    (`() `())
    (`(,exp . ,exps^)
     (let ((v (valof exp vars vals))
           (v^ (valof* exps^ vars vals)))
       `(,v . ,v^)))))

(define (valof exp vars vals)
  (match exp
    ((? number?) exp)
    ((? symbol?) (lookup exp vars vals))
    (`(quote ,v) v)
    (`(list . ,exps) (valof* exps vars vals))
    (`(λ (,x) ,b) `(closure ,x ,b ,vars ,vals))
    (`(,rator ,rand) 
     (match-let 
       ([`(closure ,x ,b ,vars^ ,vals^) (valof rator vars vals)]
        [a (valof rand vars vals)])
       (valof b `(,x . ,vars^) `(,a . ,vals^))))))
