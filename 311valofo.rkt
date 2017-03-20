#lang racket
(require "mk.rkt")
(require (except-in (rename-in racket (eval J-eval)) ==))

;; NB, there's no base case.
(define-relation (lookup x vars vals o)
  (fresh (y vars^ a vals^)
    (== `(,y . ,vars^) vars)
    (== `(,a . ,vals^) vals)
    (conde
      ((== x y) (== o a))
      ((=/= x y) (lookup x vars^ vals^ o)))))

(define-relation (valof* exps vars vals o)
  (conde
    ((== `() exps) (== o `()))
    ((fresh (exp exps^)
       (== exps `(,exp . ,exps^))
       (fresh (v v^)
         (== o `(,v . ,v^))
         (valof exp vars vals v)
         (valof* exps^ vars vals v^))))))

(define-relation (valof exp vars vals o)
  (conde
;;  ((numbero exp) (== o exp))
    ((symbolo exp) (lookup exp vars vals o))
    ((== exp `(quote ,o))
     (absento 'closure o)
     (absento 'quote vars))
    ((fresh (exps)
       (== exp `(list . ,exps))
       (absento 'list vars)
       (valof* exps vars vals o)))
    ((fresh (x b)
       (== exp `(λ (,x) ,b))
       (absento 'λ vars)
       (symbolo x)
       (== o `(closure ,x ,b ,vars ,vals))))
    ((fresh (rator rand)
       (== exp `(,rator ,rand))
      (fresh (x b vars^ vals^ a)
        (valof rator vars vals `(closure ,x ,b ,vars^ ,vals^))
        (valof rand vars vals a)
        (valof b `(,x . ,vars^) `(,a . ,vals^) o))))))

(define (eval exp o)
  (valof exp '() '() o))
