#lang racket

;; this won't work for infinite loops
(define thunkify
  (lambda (x)
    (lambda ()
      x)))

(define-syntax-rule (thunkify-ma x) (lambda () x))

(define-syntax-rule (if-ma test then conseq else alt)
  (if test conseq alt))

(define-syntax if-ma2
  (syntax-rules (then else)
    ((_ e0 then e1 else e2)
     (if e0 e1 e2))))

(define-syntax-rule (for ((var start) test incr) body)
  (let loop ((var start))
    (when test ; -> (if test conseq (void))
      (begin
        body
        (loop (incr var))))))

;; define-syntax-rule and call to macro are same:
;; (for ((var start) test     incr) body)
;; (for ((i   0)     (< i 10) add1) (printf "~a\n" i))

(define-syntax my-let*
  (syntax-rules ()
    ((_ ((var1 val1)) b0 b1 ...)
     (let ((var1 val1)) b0 b1 ...))
    ((_ ((var1 val1) (var2 val2) ...)
        b0 b1 ...)
     (let ((var1 val1))
       (my-let* ((var2 val2) ...) b0 b1 ...)))))

;; left-left-lambda is essentially equal to let
;; (lambda shortcut for let from hw)
;; ((lambda (x) x) 5) <-> (let ((x 5)) x)

(define-syntax let->lll
  (syntax-rules ()
    ((_ ((e0 v0)) b0 b1 ...)
     ((lambda (e0) b0 b1 ...) v0))
    ((_ ((e0 v0) (e1 v1) ...) b0 b1 ...)
     ((lambda (e0) (let->lll ((e1 v1) ...) b0 b1 ...)) v0))))

(define-syntax my-or
  (syntax-rules ()
    ((_) #f)
    ((_ e0 e1 ...)
     (let ((var e0))
       (if var
           var
           (my-or e1 ...))))))

(define-syntax-rule (swap x y)
  (let ((tmp x))
    (set! x y)
    (set! y tmp)))

;; macro expand by hand
;; (let ((tmp 5)
;;       (something-else 6))
;;   (let ((tmp tmp))
;;     (set! tmp something-else)
;;     (set! something-else tmp))
;;   (list tmp something-else))

;; actual macro expand
;; (let ((tmp 5)
;;       (something-else 6))
;;   (let (((gensym tmp) tmp))
;;     (set! tmp something-else)
;;     (set! something-else tmp))
;;   (list tmp something-else))

;; extend more from the language
;; uses rkt's curry function to
;; give us "curried-by-default" functions
(define-syntax define-curry
  (syntax-rules ()
    ((_ (f . a) b ...)
     (define f (curry (lambda a b ...))))
    ((_ f (lambda a b ...))
     (define f (curry (lambda a b ...))))))
