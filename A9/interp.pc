;; #lang racket
;; (require "parenthec.rkt")

(define-registers expression env k y k^ cl a v)
(define-program-counter pc)

(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (capture body)
  (return kexp vexp)
  (let e body)              
  (lambda body)
  (app rator rand))

(define-union clos
  (closure body env))

(define-union envr
  (empty-env)
  (extend-env a^ env^))

(define-union kt
  (*-inner-k k^ k^^)
  (*-outer-k x1^ env^ k^)
  (sub1-k k^)
  (zero-k k^)
  (if-k conseq^ alt^ env^ k^)
  (return-k v-exp^ env^)
  (let-k body^ env^ sk^)
  (app-inner-k v^ k^)
  (app-outer-k rand^ env^ k^)
  (empty-k jumpout))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Comment out the lines #lang racket, (require “parentheC.rkt”), and your invocation of main if you added it to your file. And save a copy of this file named interp.pc.

(define-label value-of-cps
    (union-case expression expr
      [(const cexp^) (begin (set! v cexp^)
                           (set! pc apply-k))]
      [(mult nexp1^ nexp2^) (begin (set! expression nexp2^)
                                 (set! k (kt_*-outer-k nexp1^ env k))
                                 (set! pc value-of-cps))]
      [(sub1 nexp^) (begin (set! expression nexp^)
                          (set! k (kt_sub1-k k))
                          (set! pc value-of-cps))]
      [(zero nexp^) (begin (set! expression nexp^)
                          (set! k (kt_zero-k k))
                          (set! pc value-of-cps))]
      [(if test^ conseq^ alt^) (begin (set! expression test^)
                                   (set! k (kt_if-k conseq^ alt^ env k))
                                   (set! pc value-of-cps))]
      [(capture body^) (begin (set! expression body^)
                             (set! env (envr_extend-env k env))
                             (set! pc value-of-cps))]
      [(return kexp vexp) (begin (set! expression kexp)
                                 (set! k (kt_return-k vexp env))
                                 (set! pc value-of-cps))]
      [(let e^ body^) (begin
                         (set! expression e^)
                         (set! k (kt_let-k body^ env k))
                             (set! pc value-of-cps))]
      [(var n^) (begin (set! k^ k)
                      (set! y n^)
                      (set! pc apply-env))]
      [(lambda body^) (begin (set! v (clos_closure body^ env))
                            (set! pc apply-k))]
      [(app rator^ rand^) (begin (set! expression rator^)
                               (set! k (kt_app-outer-k rand^ env k))
                               (set! pc value-of-cps))]
      ))

(define-label apply-env
    (union-case env envr
      [(extend-env a^ env^) (if (zero? y)
                                (begin (set! k k^)
                                       (set! v a^)
                                       (set! pc apply-k))
                                (begin (set! env env^)
                                       (set! y (sub1 y))
                                       (set! pc apply-env)))]
      [(empty-env) (error 'value-of "unbound identifier")]
      ))

(define-label apply-closure
    (union-case cl clos
      [(closure body^ env^) (begin (set! expression body^)
                                 (set! env (envr_extend-env a env^))
                                 (set! k k^)
                                 (set! pc value-of-cps))]
      ))

(define-label apply-k
    (union-case k kt
      [(*-inner-k k^ k^^) (begin (set! k k^^)
                                 (set! v (* k^ v))
                                 (set! pc apply-k))]
      [(*-outer-k x1^ env^ k^) (begin (set! expression x1^)
                                      (set! env env^)
                                      (set! k (kt_*-inner-k v k^))
                                      (set! pc value-of-cps))]
      [(sub1-k k^) (begin (set! k k^)
                          (set! v (sub1 v))
                          (set! pc apply-k))]
      [(zero-k k^) (begin (set! k k^)
                          (set! v (zero? v))
                          (set! pc apply-k))]
      [(if-k conseq^ alt^ env^ k^) (if v
                                       (begin (set! expression conseq^)
                                              (set! env env^)
                                              (set! k k^)
                                              (set! pc value-of-cps))
                                       (begin (set! expression alt^)
                                              (set! env env^)
                                              (set! k k^)
                                              (set! pc value-of-cps)))]
      [(return-k v-exp^ env^) (begin (set! expression v-exp^)
                                     (set! env env^)
                                     (set! k v)
                                     (set! pc value-of-cps))]
      [(let-k body^ env^ k^) (begin (set! expression body^)
                                     (set! env (envr_extend-env v env^))
                                     (set! k k^)
                                     (set! pc value-of-cps))]
      [(app-inner-k v^ k^^) (begin (set! cl v^)
                                  (set! a v)
                                  (set! k^ k^^)
                                  (set! pc apply-closure))]
      [(app-outer-k rand^ env^ k^) (begin (set! expression rand^)
                                          (set! env env^)
                                          (set! k (kt_app-inner-k v k^))
                                          (set! pc value-of-cps))]
      [(empty-k jumpout) (dismount-trampoline jumpout)]
      ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-label main 
    (begin (set! expression (expr_let
                 (expr_lambda
                  (expr_lambda
                   (expr_if
                    (expr_zero (expr_var 0))
                    (expr_const 1)
                    (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
                 (expr_mult
                  (expr_capture
                   (expr_app
                    (expr_app (expr_var 1) (expr_var 1))
                    (expr_return (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
                  (expr_const 5))))
           (set! env (envr_empty-env))
           (set! pc value-of-cps)
           (mount-trampoline kt_empty-k k pc)
           (printf "Fact 5: ~s\n" v))
    )

