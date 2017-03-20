#lang racket

;; Code from Lab 2 (Sep 04)
;; Carl and Keyan

#|
A `Lambda-term` is one of:
- Variable    ::= Symbol
- Abstraction ::= (lambda (Symbol) Lambda-term)
- Application ::= (Lambda-term Lambda-term)

Note: The `(Symbol)` in an Abstraction is called a "Variable in Binding Position"
|#


(define oh-my-exp-1
  'foo) ;; Symbol

(define oh-my-exp-2
  '(lambda (y) bar)) ;; Abstraction

(define oh-my-exp-3
  '((lambda (y) y) cool)) ;; Application


;; Let's define a function that grabs all Variables (including ones in Binding Position)
;; Why? To keep our function really simple!
(define grab-all-vars
  (lambda (exp)
    (match exp
      [`,x #:when (symbol? x) `(,x)]
      [`(lambda (,y) ,body) `(,y . ,(grab-all-vars body))]
      [`(,rator ,rand) (append (grab-all-vars rator)
                               (grab-all-vars rand))])))

;; Some tests. 
;;(grab-all-vars oh-my-exp-1)
;; > '(foo)
;; (grab-all-vars oh-my-exp-2)
;; > '(y bar)
;; (grab-all-vars oh-my-exp-3)
;; > '(y y cool)


;; Let's say, for some reason, we don't like the variable `y` and we don't want to include it in our output. How do we do that?
;; We need only change the lines for Variables and Abstractions
(define grab-all-vars-but-y
  (lambda (exp)
    (match exp
      [`,x #:when (symbol? x) (if (eqv? x 'y) '() `(,x))]
      [`(lambda (,x) ,body) (if (eqv? x 'y)
                                (grab-all-vars-but-y body)
                                `(,x . ,(grab-all-vars-but-y body)))]
      [`(,rator ,rand) (append (grab-all-vars-but-y rator)
                               (grab-all-vars-but-y rand))])))

;; More tests.
;; (grab-all-vars-but-y oh-my-exp-1)
;; > '(foo)
;; (grab-all-vars-but-y oh-my-exp-2)
;; > '(bar)
;; (grab-all-vars-but-y oh-my-exp-3)
;; > '(cool)


;; It works! but it's terrible. We shouldn't hard-code the `y` part.
;; What if we want to manually choose what variable we don't want to see in our output?
;; (e.g. what if don't like `x`? what if we don't like `z`? what if we don't like `x` and `z`?)
;; let's pass a list as the 2nd argument to the function, which specifies the stuff we don't want to see.
;; Oh and we need to change `eqv?` to `memv`.
(define grab-all-vars-unless-in-ls
  (lambda (exp ls)
    (match exp
      [`,x #:when (symbol? x) (if (memv x ls) '() `(,x))]
      [`(lambda (,x) ,body) (if (memv x ls)
                                (grab-all-vars-unless-in-ls body ls)
                                `(,x . ,(grab-all-vars-unless-in-ls body ls)))]
      [`(,rator ,rand) (append (grab-all-vars-unless-in-ls rator ls)
                               (grab-all-vars-unless-in-ls rand ls))])))

;; (grab-all-vars-unless-in-ls oh-my-exp-1 '(y))
;; > '(foo)
;; (grab-all-vars-unless-in-ls oh-my-exp-2 '(y))
;; > '(bar)
;; (grab-all-vars-unless-in-ls oh-my-exp-3 '(y))
;; > '(cool)

#|
We can keep going! Play around with this last definition and see what you can come up with. You can do a lot of different things with simple changes and achieve different outputs (e.g. Return free variables or bound variables).
Before you do, just rename `ls` to `env` so it becomes, more or less, the formal way of writing functions like these.
- (For now it's not really an `env`, cuz it's still kinda hard-coded. it should be changed based on the context!)
Just keep your changes as simple as possible so that the function still works. "The rest is left as an exercise to the reader."
|#
