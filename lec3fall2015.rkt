#lang racket

;; let vs. letrec
;; http://docs.racket-lang.org/guide/let.html

;; this one won't work
;; (define fact-let
;;   (lambda (x)
;;     (let ([! (lambda (n)
;;                (if (zero? n)
;;                    1
;;                    (* n (! (sub1 n)))))])
;;       (! x))))

;; this one works
;; (define fact-letrec
;;   (letrec ([! (lambda (n)
;;                 (if (zero? )
;;                     1
;;                     (* n (! (sub1 n)))))])
;;     !))

;; quasiquote (`) and unquote (,)
;; we know about ' (quote), if i ' (quote) anything it turns it into data. but what if i want to create data structures dynamically? i dont know what the structure will hold until my program goes off and does some work. but what if i have some skeleton (or say template) of what that data structure is and i was able to fill it in? quoasiuote will allow us to do this!

;; (define foo 12)
;; foo
;; > 12
;; 'foo
;; > 'foo
;; `foo
;; > 'foo
;; `,foo
;; > 12
;; '`,foo
;; > '`,foo
;; `',foo
;; > ''12

(define build-a-pair
  (lambda (x y)
    (cons x y)))

(define build-with-quasi
  (lambda (x y)
    `(,x . ,y))) ;; this is nicer cuz you can **see** the structure

(define rember
  (λ (x ls)
    (cond
      [(null? ls) '()]
      [(eqv? x (car ls)) (rember x (cdr ls))]
      [else (cons (car ls) (rember x (cdr ls)))])))

(define rember-quasi
  (lambda (x ls)
    (cond
      [(null? ls) '()]
      [(eqv? x (car ls)) (rember-quasi x (cdr ls))]
      [else `(,(car ls) . ,(rember-quasi x (cdr ls)))])))

;; match
;; http://docs.racket-lang.org/guide/match.html
;; structural recursion
;; So far for most of the hw1 we have been recuring on lists. mapping, filter, and others like them. we also got away by recuring on what we called sets, but really those were the same as lists. These are only two types of data structures, and we can recur over many others. and thats good because if you just bought what i said about quasiquote and unquote you should be able to build whatever data structures you want! but if we want to recur over all these data structures and others such as trees we will need a tool. this tool is called match.

;; Basically we fill out the skeleton of the data structure and fill in the place holders with bindings. lets do an example on lists. how about: sum

;; it is important to keep in mind that `(,a . ,d) <=> (cons a d)

(define sum-ls
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [else (+ (car ls) (sum-ls (cdr ls)))])))

(define sum-ls-with-match
  (lambda (ls)
    (match ls
      ['() 0]
      [`(,a . ,d) (+ a (sum-ls-with-match d))])))

(define fact-match
  (lambda (n)
    (match n
      [0 1]
      [n #:when (number? n) (* n (fact-match (sub1 n)))] ;; guard
      [_ 'error])))

(define rember-match
  (lambda (x ls)
    (match ls
      ['() '()]
      [`(,a . ,d) #:when (eqv? a x) (rember-match x (cdr ls))]
      [`(,a . ,d) `(,a . ,(rember-match x d))])))

;; cool! but that one was easy. let try to do recursion over say binary trees. lets define our data structure a s-expressions with the delaration of either Leaf or Tree to denote a leaf, which holds a value, and a tree, which holds a left and a right subtree.

;; out skeleton looks something like:
;; (Leaf ...) or (Tree ... ...)

(define just-a-tree
  '(Tree (Tree (Leaf 1)
               (Leaf 2))
         (Tree (Tree (Leaf 3)
                     (Leaf 4))
               (Leaf 5))))

;; so, when using match lets just add a quasiquote to make it a data structure and some place holders for those values with unquote[symbol] like so:

;; `(Leaf ,value) and `(Tree ,left-sub-tree ,right-sub-tree)

;; great. now we can write functions over trees. lets right gather leaves.

(define gather
  (lambda (tr)
    (match tr
      [`(Leaf ,val) #:when (number? val) `(,val)]
      [`(Tree ,L ,R) (append (gather L) (gather R))])))

;; we could also write other functions such as incrementing every leaf by some number. if we wanted we could also store some data at each node by making the modification to the Tree declaration (a node) to store data like so:

;; (Tree ... ... ...) skeleton 

;; maps to `(Tree ,data ,left-sub-tree ,right-sub-tree) data structure

(define just-a-tree-with-val
  '(Tree 9
         (Tree 8
               (Leaf 1)
               (Leaf 2))
         (Tree 6
               (Tree 7
                     (Leaf 3)
                     (Leaf 4))
               (Leaf 5))))

(define gather-with-val
  (lambda (tr)
    (match tr
      [`(Leaf ,val) #:when (number? val) `(,val)]
      [`(Tree ,val ,L ,R) (append `(,val)
                                  (gather-with-val L)
                                  (gather-with-val R))])))

(define make-tree-with-val
  (lambda (val L R)
    `(Tree ,val ,L ,R)))

;; we can recur on any data structure as long as we match against the structures it can be and handle the cases appropriately. so now what happens when i put a  ' (quote) onto some scheme program? like for example (lambda (x) x)? i get back '(lambda (x) x), i have changed my program into data...actually a data structure so i should be able to use this nifty new structual recursion to comb over it right?

;; so lets define a simple tree data structure of a language so we can recur over it. we want it to be simple so how about we only have symbols, functions of one argument, and application of those functions? like so: 

;; lambda calculus (simplified)
;; Exp := Symbol            ;; a symbol
;;      | (lambda (x) Body) ;; a function
;;      | (Rator Rand)      ;; an application. operator and operand

;; now since we said this was a tree structure we should be able to take its height right? every tree has a height so we better be able to take it. so lets write that function.

(define just-a-program
  '(lambda (x) wowwowwow))

(define just-a-complicated-program
  '(lambda (x) ((lambda (y) wow) oops)))

(define depth
  (lambda (tr)
    (match tr
      [`,y #:when (symbol? y) 0]
      [`(lambda (,x) ,body) (add1 (depth body))]
      [`(,rator ,rand) (add1 (max (depth rator)
                                  (depth rand)))])))

;; we shoud also be able to collect all the leaves since we said it was a tree right? so how about we do that as well:

(define gather-leaves
  (lambda (tr)
    (match tr
      [`,y #:when (symbol? y) `(,y)]
      [`(lambda (,x) ,body) (gather-leaves body)]
      [`(,rator ,rand) (append (gather-leaves rator)
                               (gather-leaves rand))])))

