(define group "")
(define-syntax test-group
  (syntax-rules ()
    ((_ name . body)
     (let () (set! group name) . body))))
(define (abort form)
  (display "Assertion fail in group " (current-error-port))
  (display group (current-error-port))
  (display ": " (current-error-port))
  (write form (current-error-port))
  (newline (current-error-port))
  (exit 1))
(define-syntax test-assert
  (syntax-rules ()
    ((_ form)
     (if form
       #t
       (abort 'form)))))

(define-syntax test-assert-proc
  (syntax-rules ()
    ((_ form) (test-assert (procedure? form)))))
