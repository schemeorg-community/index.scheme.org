(define-library
  (scmindex util)
  (import (scheme base)
          (scheme write))
  (export ->string)
  (begin
    (define (->string obj)
      (define port (open-output-string))
      (write obj port)
      (get-output-string port))))
