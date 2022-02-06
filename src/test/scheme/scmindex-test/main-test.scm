(define-library
  (scmindex-test main-test)
  (import (scheme base)
          (scheme write)
          (scheme read)
          (scmindex types-parser) 
          (srfi 64)
          (srfi 180))
  
  (export do-scmindex-test)
  
  (begin
    
    (define (do-scmindex-test)
      (test-begin "SCM index test")
      (test-group "types-parser"
                  (include "test-types-parser.scm"))
      (test-end))))
