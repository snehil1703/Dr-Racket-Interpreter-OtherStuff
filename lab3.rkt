#lang racket
;; ((((lambda (x)
;;      (lambda (y)
;;        (lambda (z)
;;          ((g y) (lambda (g) (g z))))))
;;    i)
;;   h)
;;  x)

#|
List all free variables: x h i g
List all bound variables: y g z

Now write out the `lex`-ified equivalent of the
above expression.
|#

#|-- Solutions |#
;; ((((λ
;;      (λ
;;        (λ
;;          ((-1 1) (λ (0 1))))))
;;    -1)
;;   -1)
;;  -1)

#|-- Some live coding |#

;; Λ-expr -> Boolean
(define var-occurs-free?
  (λ (var exp)
    (match exp
      [(? symbol?) (eqv? var exp)]
      [`(λ (,x) ,b)
       (and (not (eqv? var x))
            (var-occurs-free? var b))]
      [`(,rat ,rand)
       (or (var-occurs-free? var rat)
           (var-occurs-free? var rand))])))

;; Λ-expr -> Boolean
(define var-occurs-bound?
  (λ (var exp)
    (match exp
      [(? symbol?) #f]
      [`(λ (,x) ,b)
       (or (and (eqv? var x)
                (var-occurs-free? var b))
           (var-occurs-bound? var b))]
      [`(,rat ,rand)
       (or (var-occurs-bound? var rat)
           (var-occurs-bound? var rand))])))

;; Λ-expr -> Lex-expr
(define var-pos
  (λ (var env)
    (if (eqv? var (car env))
        0
        (add1 (var-pos var (cdr env))))))
(define lex
  (λ (exp env)
    (match exp
      [(? symbol?)
       (if (memv exp env)
           `(var ,(var-pos exp env))
           `(var -1))]
      [`(λ (,x) ,b)
       `(λ ,(lex b (cons x env)))]
      [`(,rat ,rand)
       `(,(lex rat env)
         ,(lex rand env))])))

#|-- The Difference between fn and ds Environments

The main difference is between `apply-env-fn` and `apply-env-ds`.

One is a `functional` representation of environments and the other
is a `data-structural` representation of environments. This is
easier to see in how we define and transform `empty-env`.|#

;; Given by the assignment, the base environment is a function
;; that returns an "unbound variable" error.
;; (λ (y) (error 'valof "unbound variable ~s\n" y))

;; To make the `fn` representation, we simply drop this function
;; into the body of a `define`.
(define empty-env-fn
  (λ () ;; * what the heck is this?
    (λ (y) (error 'valof "unbound variable ~s\n" y)))) ;; same as above!

;; * Good question. It's a thunk, a function with 0 arity.
;; We use it in the same way as other functions.
(empty-env-fn) ;; => #<procedure> => (λ (y) (error 'valof "unbound variable ~s\n" y))

;; Now, how do we transform this into the `data-structural` representation?
;; Simple. We use a `tagged-list`.
(define empty-env-ds
  (λ () `(empty-env-ds))) ;; * What? That's not the same.

;; * Good observation. This is most definitely not the same as the original
;; representation of the base environment or the functional one, `fn`.

;; How do we handle this? We use something that converts the `tagged-list`
;; into the actual function/behavior we want it to do.
(define apply-env-ds
  (λ (env y)
    (match env
      ;; There it is!
      [`(empty-env) (λ (y) (error 'valof "unbound variable ~s\n" y))]
      ;;;
      ;;; More stuff here... What else do environments look like?
      ;;;
      ;;; "The rest is left as [a homework assignment] to the reader."
      )))

