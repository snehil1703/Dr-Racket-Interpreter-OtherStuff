#lang racket



(define pie 13)

(define billu "where are u now")

(define (piece +)      
  (string-append (string-append + +) "hellno"))

;(piece "marcello")


(define (reply s)
  (if (string? s)
      (if (equal? "hello" (substring s 0 5))
          "hi!"
          "huh?")
      "huh?"))

;(reply "hello nigga")
;(reply "  hello nigga")

(define (reply: s)
  (if (and (string? s)
           (>= (string-length s) 5)
           (equal? "hello" (substring s 0 5)))
      "hi!"
      "huh?"))

;(reply: "hello nigga")
;(reply: "  hello nigga")
;(reply: "hel")

(define (reply-more s)
  (if (equal? "hello" (substring s 0 5))
      "hi!"
      (if (equal? "goodbye" (substring s 0 7))
          "bye!"
          (if (equal? "?" (substring s (- (string-length s) 1)))
              "I don't know"
              "huh?"))))

;(reply-more "papakancha")
;(reply-more "goodbye adios mofo")

(define (double v)
  (
   (if (string? v)
       string-append
       +)
   v v)
  )

;(double "-3.512+i")

(define
  (twice f v)
  (f (f v))
  )

;(twice
 ;(λ(s)
  ; (string-append s "!"))
 ;"hello!!baby")

 ;((λ(s)
  ; (string-append s "!"))
 ;"hello!!baby")

(define (make-add-suffix s2)
  (lambda (s) (string-append s s2)))

;((make-add-suffix "!") "hello")

(define (converse s)
  (define (starts? s2) ; local to converse
    (define len2 (string-length s2))  ; local to starts?
    (and (>= (string-length s) len2)
         (equal? s2 (substring s 0 len2)))
    )
  (cond
   [(starts? "hello") "hi!"]
   [(starts? "goodbye") "bye!"]
   [else "huh?"])
  )

;(converse "bitch")
;(converse "hellomiya")
;(converse " goodbyeluck")


;(let ([x 5])
 ; (let ((y 7))
  ;  (let ((f (λ (x) (+ x y))))
   ;   (let ((x 2))
    ;    (f x))))) 

;for lexical scope: 7, for dynamic scope: 12

(define
  (remove-dups l)
  (cond
    [(empty? l) '()]
    [(empty? (cdr l)) l]
    [else
     (let ([n (car l)])
       (if (eqv? n (car(cdr l)))
           (remove-dups (cdr l))
           (cons n (remove-dups (cdr l)))))]))

;(symbol? (symbol->string (car (quote (road map)))))

(define b (box "apple"))
b

