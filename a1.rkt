#lang racket

(print-as-expression #f)

(provide
    countdown
    insertR
    remv-1st
    count-?s
    filter
    zip
    map
    append
    reverse
    fact
    member-?*
    fib
    binary->natural
    minus
    div
    append-map
    set-difference
    powerset
    cartesian-product
    insertR-fr
    count-?s-fr
    filter-fr
    zip-fr
    map-fr
    append-fr
    reverse-fr
    binary->natural-fr
    append-map-fr
    set-difference-fr
    powerset-fr
    cartesian-product-fr
    collatz
    quine)

(define (countdown n)
    (cons n (if (zero? n) '()
        (countdown (sub1 n)))))

(define (insertR x y xs)
    (if (null? xs) '()
        (let ((head (car xs))
              (tail (insertR x y (cdr xs))))
            (if (eqv? head x)
                (cons head (cons y tail))
                (cons head tail)))))

(define (remv-1st x xs)
    (if (null? xs) '()
        (let ((head (car xs))
              (tail (cdr xs)))
            (if (eqv? head x) tail
                (cons head (remv-1st x tail))))))

(define (count x xs)
    (if (null? xs) 0
        (let ((head (car xs))
              (tail (count x (cdr xs))))
            (if (eqv? head x)
                (add1 tail)
                tail))))

(define (count-?s xs)
    (count '? xs))

(define (filter ? xs)
    (if (null? xs) '()
        (let ((head (car xs))
              (tail (filter ? (cdr xs))))
            (if (? head)
                (cons head tail)
                tail))))

(define (on f g x y)
    (g (f x) (f y)))

(define (zip xs ys)
    (if (or (null? xs) (null? ys)) '()
        (cons (on car cons xs ys)
            (on cdr zip xs ys))))

(define (map f xs)
    (if (null? xs) '()
        (cons (f (car xs))
            (map f (cdr xs)))))

(define (append xs ys)
    (if (null? xs) ys
        (cons (car xs)
            (append (cdr xs) ys))))

(define (reverse-tail xs ys)
    (if (null? xs) ys
        (reverse-tail (cdr xs)
            (cons (car xs) ys))))

(define (reverse xs)
    (reverse-tail xs '()))

(define (reverse-natural xs)
    (if (null? xs) '()
        (append
            (reverse-natural (cdr xs))
            (cons (car xs) '()))))

(define (fact n)
    (if (zero? n) 1
        (* n (fact (sub1 n)))))

(define (member* x xs)
    (if (null? xs) #f
        (let ((head (car xs))
              (tail (cdr xs)))
            (or (eqv? head x)
                (and
                    (pair? head)
                    (member* x head))
                (member* x tail)))))

(define (member-?* xs)
    (member* '? xs))

(define (update + pair)
    (let ((fst (car pair))
          (snd (cdr pair)))
        (cons (+ fst snd) fst)))

(define (fib-pair n)
    (if (zero? n) (cons 0 1)
        (update + (fib-pair (sub1 n)))))

(define (fib n)
    (car (fib-pair n)))

; (equal? '((w . (x . ())) . (y . ((z . ()) . ()))) '((w x) y (z)))

(define (binary->natural xs)
    (if (null? xs) 0
        (+ (car xs) (* 2
            (binary->natural (cdr xs))))))

(define (minus x y)
    (if (zero? y) x
        (sub1 (minus x (sub1 y)))))

(define (div x y)
    (if (zero? x) 0
        (add1 (div (minus x y) y))))

(define (append-map f xs)
    (if (null? xs) '()
        (append (f (car xs))
            (append-map f (cdr xs)))))

(define (elem? x xs)
    (if (null? xs) #f
        (or (eqv? (car xs) x)
            (elem? x (cdr xs)))))

(define (set-difference xs ys)
    (filter (lambda (x)
            (not (elem? x ys)))
        xs))

(define (combinations xs n k)
    (cond
        ((< n k)
            '())
        ((zero? k)
            '(()))
        (else
            (let ((head (car xs))
                  (tail (cdr xs))
                  (pred (sub1 n)))
                (append
                    (map
                        (lambda (tail) (cons head tail))
                        (combinations tail pred (sub1 k)))
                    (combinations tail pred k))))))

(define (length xs)
    (if (null? xs) 0
        (add1 (length (cdr xs)))))

(define (powerset xs)
    (let ((length (length xs)))
        (append-map (lambda (k)
                (combinations xs length k))
            (countdown length))))

(define (cons-list xs xss)
    (append-map (lambda (x)
            (map (lambda (xs)
                    (cons x xs))
                xss))
        xs))

(define (cartesian-product xss)
    (if (null? xss) '(())
        (cons-list (car xss)
            (cartesian-product (cdr xss)))))

(define (insertR-fr x y xs)
    (foldr (lambda (head tail)
            (if (eqv? head x)
                (cons head (cons y tail))
                (cons head tail)))
        '() xs))

(define (count-fr x xs)
    (foldr (lambda (head tail)
            (if (eqv? head x)
                (add1 tail)
                tail))
        0 xs))

(define (count-?s-fr xs)
    (count-fr '? xs))

(define (filter-fr ? xs)
    (foldr (lambda (head tail)
            (if (? head)
                (cons head tail)
                tail))
        '() xs))

(define (zip-fr xs ys)
    ((foldr (lambda (x f)
                (lambda (ys)
                    (if (null? ys) '()
                        (cons (cons x (car ys))
                            (f (cdr ys))))))
            (lambda (ys) '()) xs)
        ys))

(define (map-fr f xs)
    (foldr (lambda (head tail)
            (cons (f head) tail))
        '() xs))

(define (append-fr xs ys)
    (foldr cons ys xs))

(define (reverse-fr xs)
    (foldr (lambda (last init)
            (append-fr init (cons last '())))
        '() xs))

(define (binary->natural-fr xs)
    (foldr (lambda (head tail)
            (+ head (* 2 tail)))
        0 xs))

(define (append-map-fr f xs)
    (foldr (lambda (head tail)
            (append-fr (f head) tail))
        '() xs))

(define (set-difference-fr xs ys)
    (foldr (lambda (head tail)
            (if (not (elem? head ys))
                (cons head tail)
                tail))
        '() xs))

(define (powerset-fr xs)
    (let ((length (length xs)))
        (foldr (lambda (head tail)
                (append-fr (combinations xs length head) tail))
            '() (countdown length))))

(define (cartesian-product-fr xss)
    (foldr cons-list '(()) xss))

(define collatz
  (letrec
    ((odd-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (odd? x)) (collatz (add1 (* x 3)))) 
            (else (recur x))))))
     (even-case
       (lambda (recur)
         (lambda (x)
           (cond 
            ((and (positive? x) (even? x)) (collatz (/ x 2))) 
            (else (recur x))))))
     (one-case
       (lambda (recur)
         (lambda (x)
           (cond
            ((zero? (sub1 x)) 1)
            (else (recur x))))))
     (base
       (lambda (x)
         (error 'error "Invalid value ~s~n" x))))
    (one-case (even-case (odd-case base)))
    ))

; My quine is a universal constructor which can construct any lisp program from a
; numeric description of the program. It requires two inputs, the list of symbols
; appearing in the program and the numeric description of the program, which is a
; number in a given base, encoding the entire program as a sequence of nils, cons
; and symbols, represented as 0, 1, and so on, respectively. When given a numeric
; description of itself as an input, the universal constructor becomes a quine.

; This is 100% my own code. It can be found online here:
; https://github.com/aaditmshah/universal-constructor-quine
; The code is released under the MIT license.

(define quine
    '(letrec
        ((univcons
            (lambda (hash size num)
                (let-values (((num mod) (quotient/remainder num size)))
                    (cond
                        ((= mod 0)
                            (values num '()))
                        ((= mod 1)
                            (let*-values (((num fst) (univcons hash size num))
                                          ((num snd) (univcons hash size num)))
                                (values num (cons fst snd))))
                        (else
                            (values num (hash-ref hash mod)))))))
            (assoc
                (lambda (num xs)
                    (if (null? xs) '()
                        (cons
                            (cons num (car xs))
                            (assoc (add1 num) (cdr xs)))))))
        (let
            ((univ-cons
                (lambda (xs num)
                    (let-values
                        (((num val)
                            (univcons
                                (make-immutable-hash (assoc 2 xs))
                                (+ (length xs) 2)
                                num)))
                        val)))
                (xs
                    '(letrec univcons lambda hash size num let-values mod
                    quotient/remainder cond = 0 values 1 let*-values fst snd
                    cons else hash-ref assoc xs if null? car add1 cdr let
                    univ-cons val make-immutable-hash 2 + length quote))
                (num
                    304057600749875595149316882959599186029390834462109427740561476542887856288492027196328693436582745787405657673002043762427524716288224083595931609612657987588661390869286950768910332786399796361785622598220376667186289712998340769935386404776029505950328208152860245561751666240696901738829716228945926938367214544680393146305873541571287903919967785260427227540127920442231605042374405874404004299174732377920945314518795135469774917099353210597671754080046989763482964369959700666672581402907611917693554293797049236948494074725228351755664933406602050481541937761026210250174274371580066920895))
            (univ-cons (cons num xs) num))))

(define code
    '(1 3 1 1 1 4 1 1 5 1 1 6 1 7 1 8 0 1 1 9 1 1 1 1 8 1 10 0 1 1 11 1 8 1 7 0
    0 0 1 1 12 1 1 1 13 1 10 1 14 0 1 1 15 1 8 1 1 37 1 0 0 0 0 1 1 1 13 1 10 1
    16 0 1 1 17 1 1 1 1 8 1 18 0 1 1 4 1 6 1 7 1 8 0 0 1 1 1 8 1 19 0 1 1 4 1 6
    1 7 1 8 0 0 0 1 1 15 1 8 1 1 20 1 18 1 19 0 0 0 0 1 1 21 1 1 15 1 8 1 1 22 1
    6 1 10 0 0 0 0 0 0 0 1 1 23 1 1 5 1 1 8 1 24 0 1 1 25 1 1 26 1 24 0 1 1 37 1
    0 0 1 1 20 1 1 20 1 8 1 1 27 1 24 0 0 1 1 23 1 1 28 1 8 0 1 1 29 1 24 0 0 0
    0 0 0 0 1 1 30 1 1 1 31 1 1 5 1 1 24 1 8 0 1 1 9 1 1 1 1 8 1 32 0 1 1 4 1 1
    33 1 1 23 1 34 1 24 0 0 1 1 35 1 1 36 1 24 0 1 34 0 1 8 0 0 0 1 32 0 0 0 1 1
    24 1 1 37 1 1 3 1 4 1 5 1 6 1 7 1 8 1 9 1 10 1 11 1 12 1 13 1 14 1 15 1 16 1
    17 1 18 1 19 1 20 1 21 1 22 1 23 1 24 1 25 1 26 1 27 1 28 1 29 1 30 1 31 1 32
    1 33 1 34 1 35 1 36 1 37 0 0 0 1 1 8 1 2 0 0 1 1 31 1 1 20 1 8 1 24 0 1 8 0 0
    0 0))

; (define num (convert 38 code))

(define (convert base xs)
    (if (null? xs) 0
        (+ (car xs)
            (* base
                (convert base
                    (cdr xs))))))
