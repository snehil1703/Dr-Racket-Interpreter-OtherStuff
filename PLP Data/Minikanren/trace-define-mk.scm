;; Developed by Claire Alvis

(define-syntax trace-define-mk
  (syntax-rules ()
    ((_ name (lambda (a* ...) body))
     (define name
       (lambda (a* ...)
         (fresh ()
           (project (a* ...)
             (begin
               (printf "~s\n" (list 'name a* ...))
               succeed))
           body))))
    ((_ (name a* ...) body)
     (define (name a* ...)
       (fresh ()
         (project (a* ...)
           (begin
             (printf "~s\n" (list 'name a* ...))
             succeed))
         body)))))