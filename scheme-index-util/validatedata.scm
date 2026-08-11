(import (chicken file)
        (chicken process-context)
        json
        matchable
        srfi-1)

(define (main)
  (define filters-index-file-name (list-ref (command-line-arguments) 0))
  (define types-index-file-name (list-ref (command-line-arguments) 1))
  (process-filters-index filters-index-file-name)
  (process-types-index types-index-file-name))

(define (->string obj)
  (let ((out (open-output-string)))
    (write obj out)
    (get-output-string out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (process-filters-index index-file-name)
  (define (assoc/assert-string key alist)
    (cond
      ((assoc key alist) => (lambda (p)
                               (define v (cdr p))
                               (unless (string? v)
                                 (error (string-append "Key " (->string key) " doesn't map to string value; was " (->string v))))
                               v))
      (else (error (string-append "Missing key " (->string key))))))
  (define (process-filter filter)
    (unless (list? filter)
      (error (string-append "Filter entry not a list; was " (->string filter))))
    (assoc/assert-string 'code filter)
    (assoc/assert-string 'name filter)
    (let* ((file (assoc/assert-string 'file filter))
           (content (with-input-from-file file read)))
      (unless (list? content)
        (error (string-append "Filter file " file " doesn't contain a list")))
      (for-each
        (lambda (e)
          (unless (pair? e)
            (error (string-append "Entry in filter file " file " is not a pair; was " (->string e)))))
        content)))

  (define filters (with-input-from-file index-file-name read))

  (unless (list? filters)
    (error (string-append "Filter index file " index-file-name " consists of not a list; was " (->string filters))))

  (for-each process-filter filters))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (process-types-index index-file-name)

  (define (process-library lib)
    (define lib-name
      (let ((str-out (open-output-string)))
        (write (car lib) str-out)
        (get-output-string str-out)))
    (define-values
      (file exclude)
      (match (cdr lib)
        ((? string? v) (values v '()))
        (alist (values (cdr (assoc 'file alist))
                       (cdr (assoc 'exclude alist)) ))))
    (let ((definitions (with-input-from-file file read)))
      (for-each
        (lambda (e)
          (validate-entry-definition exclude lib-name e))
        definitions)))

  (define libraries (with-input-from-file index-file-name read))
  (unless (list? libraries)
    (error (string-append "Types index file " index-file-name " consists of not a list; was " (->string libraries))))

  (for-each process-library libraries))

(define (validate-entry-definition exclude lib-name e)
  (unless (string? lib-name)
    (error "lib-name must be string"))
  (cond
    ((assoc 'group e) => (lambda (group)
                           (for-each (lambda (e) (validate-single-entry-definition exclude lib-name e)) (cdr group))))
    (else (validate-single-entry-definition exclude lib-name e))))

(define (validate-single-entry-definition exclude lib-name e)
  (display "\nParsing " (current-error-port) )
  (display lib-name (current-error-port) )
  (display " " (current-error-port) )
  (display (cdr (assoc 'name e)) (current-error-port))
  (define name
    (cond
      ((assoc 'name e) => cdr)
      (else (error "name field missing"))))
  (define desc
    (cond
      ((assoc 'desc e) => cdr)
      (else "")))
  (cond
    ((assoc 'tags e) => (lambda (t) (for-each symbol->string (cdr t))))
    (else '()))
  (cond
    ((assoc 'subsigs e) => (lambda (subsigs)
                             (for-each validate-subsig (cdr subsigs))))
    (else '()))
  (cond
    ((assoc 'signature e) => (lambda (sig)
                               (validate-sig (cdr sig))))
    (else (error "signature field missing")))
  (unless (string? name)
    (error "name must be string"))
  (unless (string? desc)
    (error "desc must be string"))
  (unless (string? lib-name)
    (error "lib-name must be string")))

(define (validate-subsig s)
  (match s
    (((? symbol?) signature)
     (validate-sig signature))
    (_ (error "Unknown subsignature shape"))))

(define (validate-sig s)
  (match s
    (('lambda (param ...) return-type)
     (begin
       (for-each validate-function-param param)
       (validate-function-return return-type)))
    (('case-lambda cases ...)
     (let ((validate-case (lambda (c)
                         (match c
                            (((param ...) return-type)
                             (begin
                               (for-each validate-function-param param)
                               (validate-function-return return-type)))))))
       (for-each validate-case cases)))
    (('value return)
     (validate-function-return return))
    (('syntax-rules ((? symbol?) ...) cases ...)
     (for-each validate-syntax-rule-pattern cases))
    (('pattern pattern ...)
     #t)
    (('list type)
     (validate-function-param type))
    (('vector type)
     (validate-function-param type))
    (('alist key-type value-type)
     (begin
       (validate-function-param key-type)
       (validate-function-param value-type)))
    (_ (error (string-append "unrecognized signature; was " (->string s))))))

(define (validate-function-param param)
  (define (validate-type t)
    (match t
      (#f #t)
      ((? symbol?) #t)
      (_ (error "Unknown type"))))
  (match param
    ((('or types ...) (? symbol? name))
     (for-each validate-type types))
    ((type (? symbol? name))
     (validate-type type))
    ((? symbol? name)
     #t)
    (_ (error "Unknown param shape"))))

(define (validate-function-return return)
  (match return
    (('values returns ...)
     (for-each validate-function-return returns))
    (('or returns ...)
     (for-each validate-function-return returns))
    ((? symbol?)
     #t)
    (#f
     #t)
    (_ (error "Unknown return shape"))))

(define (validate-syntax-rule-pattern pattern)
  (match pattern
    ((p) #t)
    ((p return-type) (validate-function-return return-type))))

(main)
