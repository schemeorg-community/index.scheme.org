(define-library
  (main-test)
  (import (scheme base)
          (scheme write)
          (scheme read)
          (scmindex domain)
          (scmindex types-parser)
          (scmindex mustache)
          (only (srfi 1) lset=)
          (srfi 64)
          (srfi 180))
  
  (export run-tests)
  
  (begin
    
    (define (run-tests)
      (test-begin "SCM index test")
      (test-group "types-parser"
                  (include "test-types-parser.scm"))
      (test-group "mustache"
                  (include "test-mustache.scm"))
      (test-end))))
