(define-library
  (main-test)
  (import (scheme base)
          (srfi 64)
          (test-mustache)
          (test-types-parser)
          (test-json))
  
  (export run-tests)
  
  (begin
    (define (run-tests)
      (test-begin "SCM index test")
      (run-parser-tests)
      (run-json-tests)
      (run-mustache-tests)
      (test-end))))
