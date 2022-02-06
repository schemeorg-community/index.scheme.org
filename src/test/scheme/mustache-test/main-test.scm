(define-library
  (mustache-test main-test)
  (import (scheme base)
          (scheme write)
          (arvyy mustache)
          (srfi 41)
          (srfi 64))
  (export do-mustache-test)
  (begin
    (define (do-mustache-test)

      (define-syntax test-mustache
        (syntax-rules ()
          ((_ name data template expected)
           (test-equal name expected (execute (compile "foo" (lambda args template)) data)))
          ((_ name data partials template expected)
           (let* ((partials* (cons (cons "root" template) partials))
                  (fn (lambda (n) 
                        (cond
                          ((assoc n partials*) => cdr)
                          (else #f)))))
             (test-equal name expected (execute (compile "root" fn) data))))))

      (test-begin "mustache")

      (test-group 
        "comments"
        (include "mustache-test-comments.scm"))

      (test-group
        "delimiters"
        (include "mustache-test-delimiters.scm"))

      (test-group
        "interpolation"
        (include "mustache-test-interpolation.scm"))

      (test-group
        "inverted"
        (include "mustache-test-inverted.scm"))

      (test-group
        "partials"
        (include "mustache-test-partials.scm"))

      (test-group
        "sections"
        (include "mustache-test-sections.scm"))

      (test-group
        "implementation-specific"
        (include "mustache-test-implementation-specific.scm"))

      (test-end))))
