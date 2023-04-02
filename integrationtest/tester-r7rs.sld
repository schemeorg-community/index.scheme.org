(define-library 
  (tester-r7rs)
  (import (scheme base)
          (scheme process-context)
          (scheme write))
  (export test-group test-assert test-assert-proc)
  (include "tester-impl.scm"))
