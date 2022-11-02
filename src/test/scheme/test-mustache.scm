(define-library
  (test-mustache)
  (import (scheme base)
          (scheme write)
          (scheme read)
          (scmindex domain)
          (scmindex types-parser)
          (scmindex mustache)
          (only (srfi 1) lset=)
          (srfi 64)
          (srfi 180))
  
  (export run-mustache-tests)
  
  (begin
    (define (run-mustache-tests)
      (test-group
        "Read and mustache-render everything"
        (define specs (read-specs "types/index.scm"))
        (vector-for-each
          (lambda (doc)
            (test-assert (not (null? (render-index-entry doc)))))
          specs)))))
