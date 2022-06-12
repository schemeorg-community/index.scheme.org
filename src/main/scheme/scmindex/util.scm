#|
    Misc util methods
|#
(define-library
  (scmindex util)
  (import (scheme base)
          (scheme read)
          (scheme write))
  (export ->string read*)
  (begin

    ;;TODO rename to write*
    (define (->string obj)
      (define port (open-output-string))
      (write obj port)
      (get-output-string port))
    
    (define (read* str)
      (define port (open-input-string str))
      (read port))))
