#|
    Misc util methods
|#
(define-library
  (scmindex util)
  (import (scheme base)
          (scheme read)
          (scheme write))
  (export write* read*)
  (begin

    (define (write* obj)
      (define port (open-output-string))
      (write obj port)
      (get-output-string port))
    
    (define (read* str)
      (define port (open-input-string str))
      (read port))))
