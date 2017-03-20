#lang racket
(require "mk.rkt")

(define lookupo
  (lambda (env x b)
    (fresh (aa da d)
           (== env `((,aa . ,da) . ,d))
           (conde
            ((== aa x) (== da b))
            ((=/= aa x) (lookupo d x b))))))