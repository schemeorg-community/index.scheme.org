(library (tester-r6rs (1))
         (export test-group test-assert test-assert-proc)
         (import (rnrs base (6))
                 (rnrs mutable-pairs (6))
                 (rnrs io simple (6))
                 (rnrs programs (6)))
         (define group (cons "" #f))
         (define-syntax test-group
           (syntax-rules ()
             ((_ name . body)
              (let () (set-car! group name) . body))))
         (define (abort form)
           (display "Assertion fail in group " (current-error-port))
           (display (car group) (current-error-port))
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

         )
