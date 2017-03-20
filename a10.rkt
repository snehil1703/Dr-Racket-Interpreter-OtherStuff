#lang racket
(require "mk.rkt")
(require "numbers.rkt")

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 

(run 2 (q) 
  (== 5 q)
  (conde
   [(conde 
     [(== 5 q)
      (== 6 q)])
    (== 5 q)]
   [(== q 5)]))

#|
The answer of the code above is '(5)

Line 1: Runs the program for at most 2 answers(variants) of q
Line 2: Associates q with 5, so one answer for q is 5 now
Line 3: Enters a 'conde' (condition check)
Line 4,5,6: Another 'conde' (condition check) which has just one association check. The evaluation part binds q to 6, which is a contradiction to the former binding and thus results in failure
Line 7: Since the inner 'conde' is the association check for outer 'conde' and since the inner one fails, the execution of Line 7 DOES NOT occur
Line 8: The outer condition check's 2nd part has no execution, just a correct association check. Even though it is success, it does nothing

So the variable q has just one answer and that is '(5)
|#

;; 2 What is the value of
(run 1 (q) 
  (fresh (a b) 
    (== `(,a ,b) q)
    (absento 'tag q)
    (symbolo a)))

#|
The answer of the code above is
'(((_.0 _.1) (=/= ((_.0 tag))) (sym _.0) (absento (tag _.1))))

Line 1: Runs the program for at most 1 answer(variant) of q
Line 2: Creating 2 new variables, 'a' and 'b'
Line 3: Creates an association of q with a pair of variables 'a' and 'b'. Whatever the values of 'a' and 'b' be, q will be a pair of them
Line 4: Restricting the presence of 'tag anywhere in q. As 'a' and 'b' are binded to q (being a part of q), either of them also CANNOT be a 'tag
Line 5: 'a' is bound to be in a symbolic form, and CANNOT be anything else

As we do not know the values for the fresh variables 'a' and 'b', they can be anything in the domain defined by the constraints in the code. Both of them cannot be or have 'tag in them and 'a' can only be a symbol. All these constraints are explicitly displayed following the probable answer of q, which is a pair of 'a' and 'b'
|#

;; 3 What do the following miniKanren constraints mean?
;; a == (Binary function which creates an association between the two things)
;; b =/= (Binary function which creates a STRICT dis-association between the two things)
;; c absento (Binary function which restricts the presence of first input in second input)
;; d numbero (Binding the input with it to a numeric form)
;; e symbolo (Binding the input with it to a symbolic form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PART II

(define assoc
  (lambda (x ls)
    (match-let* ((`(,a . ,d) ls)
                 (`(,aa . ,da) a))
      (cond
        ((equal? aa x) a)
        ((not (equal? aa x)) (assoc x d))))))

(define assoco
  (lambda (x ls ns)
    (fresh (a d aa da)
           (== `(,a . ,d) ls)
           (== `(,aa . ,da) a)
           (conde
            [(== aa x) (== a ns)]
            [(=/= aa x) (assoco x d ns)])
           )
    ))

(define reverse
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else
       (match-let* ((`(,a . ,d) ls)
                    (res (reverse d)))
         (append res `(,a)))))))

(define reverseo
  (lambda (ls ns)
    (conde
     [(== '() ls) (== '() ns)]
     [(fresh (a d res)
             (== `(,a . ,d) ls)
             (reverseo d res)
             (appendo res `(,a) ns)
             )]
     )
    ))

(define palindrome
  (lambda (ls ns)
    (conde
     [(== '() ls) (== #t ns)]
     [(fresh (q)
             (reverseo ls q)
             (conde
              [(== q ls) (== #t ns)]
              [(=/= q ls) (== #f ns)]
              )
             )]
     ;[(== (car ls) (car (run* (q) (reverseo ls q)))) (palindrome (car (run* (q) (reverseo (cdr ls) q))) ns)]
     ;[(=/= (car ls) (car (run* (q) (reverseo ls q)))) (== #f ns)]
     )
    )
  )

(define stutter
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else 
        (match-let* ((`(,a . ,d) ls)
		     (res (stutter d)))
          `(,a ,a . ,res))))))

(define stuttero
  (lambda (ns ls)
    (conde
     [(== '() ls) (== '() ns)]
     [(fresh (a d res)
             (== `(,a ,a . ,d) ls)
             (stuttero res d)
             (== `(,a . ,res) ns))]
     )
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; BRAINTEASER

(define length
  (lambda (ls)
    (cond
      [(equal? '() ls) 0]
      [else (add1 (length (cdr ls)))]
     )
    ))

(define lengtho
  (lambda (ls ns)
    (conde
     [(== '() ls) (== (build-num 0) ns)]
     [(fresh (a d res)
             (== `(,a . ,d) ls)
             (lengtho d res)
             (pluso (build-num 1) res ns)
             )]
     )
    ))

;(require "a10-student-tests.rkt")
;(test-file #:file-name "a10.rkt")