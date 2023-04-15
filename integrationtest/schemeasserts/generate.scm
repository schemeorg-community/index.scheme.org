;; Util batch program, that takes definitions under ../filters and ../types,
;; and generates appropriate testing files to be ran with specific implementations.
;; The test is oriented towards some tentative validation that these definitions actually exist
;; and are exported / guard against typos in names.

;; This batch file itself can be ran with any r7rs compliant implementation.
;; Must be run from project root directory as working dir

(import (scheme base)
        (scheme write)
        (scheme read)
        (scheme file)
        (scheme process-context)
        (scheme list))

(define testdir "integrationtest/schemeasserts/")

(define (->string datum)
  (define port (open-output-string))
  (display datum port)
  (get-output-string port))

(define (lib->string lib)
  (define chars (string->list (->string lib)))
  (define new-chars
    (apply append (map
                    (lambda (c)
                      (cond
                        ((or (char=? c #\()
                             (char=? c #\))) '())
                        ((char=? c #\space) '(#\-))
                        (else (list c))))
                    chars)))
  (list->string new-chars))

(define (make-library-prefix lib)
  (string-append (lib->string lib) "/"))

;; given library name (as import list) and content
;; returns a test group for the entries.
;; Returns #f if it's empty
(define (build-test-group lib entries use-prefix?)
  (define prefix (if use-prefix? (make-library-prefix lib) ""))
  (define asserts (apply append (map (lambda (e) (entry->assertions e prefix )) entries)))
  (if (null? asserts)
    #f
    `(test-group ,(string-append "Library " (->string lib)) 
                 ,@asserts)))

(define (entry->assertions e prefix )
  (cond
    ((assoc 'group e) => (lambda (group*)
                           (define group (cdr group*))
                           (apply append (map (lambda (e*) (entry->assertions/single e* prefix )) group))))
    (else (entry->assertions/single e prefix ))))

(define (entry->assertions/single e prefix )
  (define signature (cdr (assoc 'signature e)))
  (define kind (car signature))
  (define name-str (cdr (assoc 'name e)))
  (define name (string->symbol (string-append prefix name-str)))
  (cond
    ((or (symbol=? kind 'lambda)
         (symbol=? kind 'case-lambda))
     `((test-assert-proc ,name)))
    ;; TODO this fails with eg. &i/o (since it's actually a syntax?)
    ;;((symbol=? kind 'value) `((test-assert ,name)))
    (else '())))

(define (load-library-list name)
  (with-input-from-file (string-append "filters/" name ".scm")
                        (lambda ()
                          (define entries (read))
                          (map car entries))))

(define (load-entries-file descriptor)
  (cond
    ((string? descriptor)
     (with-input-from-file descriptor
                           (lambda ()
                             (read))))
    (else '())))

(define (load-content)
  (define types-index (with-input-from-file "types/index.scm" read))
  (define content-map '())
  (for-each
    (lambda (pair)
      (let ((entries (load-entries-file (cdr pair))))
        (cond
          ((assoc (car pair) content-map) => (lambda (p) (set-cdr! p (append entries (cdr p)))))
          (else (set! content-map (cons (cons (car pair) entries) content-map))))))
    types-index)
  content-map)

(define (make-imports libs testerlib)
  (define (import-lib lib)
    `(prefix ,lib ,(string->symbol (make-library-prefix lib))))
  `(import (,testerlib) ,@(map import-lib libs)))

(define (build-testsuite)
  (define content (load-content))
  #|
  (define lib+testgroups
    (let loop ((content content)
               (results '()))
      (if (null? content)
          results
          (let* ((c (car content))
                 (test-group (build-test-group (car c) (cdr c))))
            (if test-group
                (loop (cdr content)
                      (cons (cons (car c) test-group) results))
                (loop (cdr content)
                      results))))))
  |#
  (define testlist (with-input-from-file (string-append testdir "testlist.scm") read))
  (for-each
    (lambda (content-entry)
      (define lib (car content-entry))
      (define lib-entries (cdr content-entry))
      (cond
        ((assoc lib testlist) => (lambda (e)
                                    (define impl (cdr e))
                                    (generate-test-file impl lib lib-entries)))
        (else (begin
                (display (string-append "No test case for library " (->string lib) "\n") (current-error-port))
                (exit 1)))))
    content))

(define (generate-test-file impl lib lib-entries)
  (case impl
    ((chibi gauche)
     (with-output-to-file (string-append testdir (symbol->string impl) "-tests/" (lib->string lib) ".scm")
                          (lambda ()
                            (define testgroup (build-test-group lib lib-entries #t))
                            (when testgroup
                              (write
                                `(import (tester-r7rs)
                                         (prefix ,lib ,(string->symbol (make-library-prefix lib)))))
                              (newline)
                              (write testgroup)))))
    ((chez)
     (with-output-to-file (string-append testdir "chez-tests/" (lib->string lib) ".scm")
                          (lambda ()
                            (define testgroup (build-test-group lib lib-entries #t))
                            (when testgroup
                              (write
                                `(import (tester-r6rs (1))
                                         (prefix ,lib ,(string->symbol (make-library-prefix lib)))))
                              (newline)
                              (write testgroup)))))
    ((guile)
     (with-output-to-file (string-append testdir "guile-tests/" (lib->string lib) ".scm")
                          (lambda ()
                            (define testgroup (build-test-group lib lib-entries #t))
                            (define lib*
                              (if (and 
                                    (= (length lib) 2)
                                    (symbol=? 'srfi (car lib))
                                    (number? (cadr lib)))
                                `(srfi ,(string->symbol (string-append "srfi-" (number->string (cadr lib)))))
                                lib))
                            (when testgroup
                              (write
                                `(import (tester-r7rs)
                                         (prefix ,lib* ,(string->symbol (make-library-prefix lib)))))
                              (newline)
                              (write testgroup)))))
    ((bigloo)
     (with-output-to-file (string-append testdir "bigloo-tests/" (lib->string lib) ".scm")
                          (lambda ()
                            (define testgroup (build-test-group lib lib-entries #f))
                            (when testgroup
                              (write '(module foo (include "tester-impl.scm")))
                              (newline)
                              (write testgroup)))))
    ((gambit)
     ;;TODO
     #f
     )
    ((ignore) #f)
    (else (begin
            (display (string-append "Unrecognized implementation " (symbol->string impl) "\n") (current-error-port))
            (exit 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(build-testsuite)
