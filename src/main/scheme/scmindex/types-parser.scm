#|
    Module responsible for parsing index entries encoded in sexpr format
|#
(define-library
  (scmindex types-parser)
  (import (scheme base)
          (scheme cxr)
          (scheme file)
          (scheme read)
          (scheme write)
          (only (srfi 1) lset-adjoin lset-difference alist-cons alist-delete delete-duplicates filter fold)
          (scmindex util)
          (scmindex domain)
          (arvyy slf4j))

  (export read-specs 
          read-spec
          make-type-maps
          flatten-type
          )
  (begin

    (define logger (get-logger "types-parser"))

    (define (parse-spec-entry-data val)
      (cond
        ((string? val) (values val '()))
        ((list? val)
         (let* ((file (cond
                        ((assoc 'file val) => cdr)
                        (else (error "Missing 'file' attribute in spec entry data" val))))
                (exclude (cond
                           ((assoc 'exclude val) => cdr)
                           (else '()))))
           (values file exclude)))
        (else (error "Unrecognized spec entry data" val))))

    (define (read-specs index-file)
      (log-info logger "Reading specs from index file {}" index-file)
      (with-input-from-file 
        index-file
        (lambda ()
          (define lst (read))
          (define specs*
            (map
              (lambda (entry)
                (define-values (file excluded) (parse-spec-entry-data (cdr entry)))
                (log-info logger "Reading specs from data file {} for lib {}" file (car entry))
                (with-input-from-file
                  file
                  (lambda ()
                    (read-spec (car entry)
                               excluded
                               (read)))))
              lst))
          (apply append specs*))))

    (define (assoc* key alist default)
      (cond
        ((assoc key alist) => cdr)
        (else default)))

    (define (read-spec lib excluded input)
      (define lib-string
        (if (string? lib)
            lib
            (let ((port (open-output-string)))
              (display lib port)
              (get-output-string port))))
      (fold
        (lambda (entry entries)
          (define name (let ((n (assoc* 'name entry #f)))
                         (unless n
                           (error "Missing name attribute"))
                         (log-debug logger "Reading spec {}" n)
                         (cond
                           ((string? n) (string->symbol n))
                           ((symbol? n) n)
                           (else (raise "entry name not a string or a symbol")))))
          (define signature (let ((s (assoc* 'signature entry #f)))
                              (unless s
                                (error "Missing signature attribute"))
                              s))
          (define supertypes
            (case (car signature)
              ((lambda) (assoc* 'supertypes entry '()))
              (else '())))
          (define param-names
            (case (car signature)
              ((lambda) (extract-lambda-param-names signature))
              ((case-lambda) (extract-case-lambda-param-names signature))
              ((syntax-rules) (extract-syntax-names signature))
              (else '())))
          (define param-types
            (case (car signature)
              ((lambda) (append (extract-lambda-param-types signature)
                                (extract-procedure-subsig-param-types (assoc* 'subsigs entry '()))))
              ((case-lambda) (append (extract-case-lambda-param-types signature)
                                     (extract-procedure-subsig-param-types (assoc* 'subsigs entry '()))))
              ((syntax-rules) (extract-procedure-subsig-param-types (assoc* 'subsigs entry '())))
              (else '())))
          (define return-types
            (case (car signature)
              ((lambda) (extract-lambda-return-types signature))
              ((case-lambda) (extract-case-lambda-return-types signature))
              ((syntax-rules) (extract-syntax-return-types signature))
              ((value) (list (cadr signature)))
              (else '())))
          (define tags (assoc* 'tags entry '()))
          (define parameterized-by (assoc* 'parameterized-by entry '()))
          (define param-signatures (assoc* 'subsigs entry '()))
          (define spec-values (assoc* 'spec-values entry '()))
          (cond
            ;; name found in excluded list -- ignore
            ((member name excluded) 
             entries)
            (else
              (begin
                (log-debug logger "Parsed spec {}" name)
                (cons
                  (make-index-entry 
                    lib-string 
                    name
                    param-names
                    signature
                    param-signatures
                    tags
                    param-types
                    return-types
                    parameterized-by
                    spec-values
                    supertypes)
                  entries)))))
        '()
        (reverse input)))

    (define (extract-case-lambda-param-names signature)
      (define params-list (apply append (map 
                                          (lambda (case)
                                            (car case))
                                          (cdr signature))))
      (extract-param-names params-list))

    (define (extract-lambda-param-names signature)
      (define params-list (cadr signature))
      (extract-param-names params-list))

    (define (extract-param-names params-list)
      (define lst* (map
                     (lambda (param)
                       (cond
                         ((list? param) (list (cadr param)))
                         ((or (equal? '... param)
                              (equal? #f param)
                              (equal? #t param)) (list))
                         (else (list param))))
                     params-list))
      (apply append lst*))

    (define (extract-syntax-names signature)
      (define literals (cadr signature))
      (define (do-extract fragment)
        (cond
          ((or (equal? '... fragment)
               (null? fragment)) 
           (list))
          ((symbol? fragment) 
           (lset-difference equal? (list fragment) literals))
          ((pair? fragment)
           (lset-difference equal? 
                            (append (do-extract (car fragment))
                                    (do-extract (cdr fragment)))
                            literals))))
      (let* ((result (map
                       (lambda (rule)
                         (do-extract (cdar rule)))
                       (cddr signature)))
             (result (apply append result))
             (result (delete-duplicates result equal?)))
        result))

    (define (extract-syntax-return-types signature)
      (let* ((rules (cddr signature))
             (rules (filter (lambda (rule) (> (length rule) 1)) rules))
             (returns (map cadr rules))
             (types (map parse-type-from-return returns)))
        (delete-duplicates (apply append types))))

    (define (extract-procedure-subsig-param-types subsigs)
      (define (parse-type-if-present value)
        (cond
          ((list? value) (parse-type-from-param (car value)))
          (else (list))))
      (define types (map
                      (lambda (sig)
                        (define value (cadr sig))
                        (case (car value)
                          ((value) (list (cadr value)))
                          ((list vector) (parse-type-if-present (cadr value)))
                          ((alist) (append (parse-type-if-present (cadr value))
                                           (parse-type-if-present (caddr value))))
                          ;; ignore lambda and synax-rules
                          (else '())))
                      subsigs))
      (apply append types))

    (define (extract-lambda-param-types signature)
      (define params-list (cadr signature))
      (extract-param-types params-list))

    (define (extract-case-lambda-param-types signature)
      (define params-list (apply append (map 
                                          (lambda (case)
                                            (car case))
                                          (cdr signature))))
      (extract-param-types params-list))

    (define (extract-param-types params-list)
      (define lst* (map
                     (lambda (param)
                       (cond
                         ((list? param)
                          (let ((value (car param)))
                            (parse-type-from-param value)))
                         (else (list))))
                     params-list))
      (apply append lst*))

    (define (parse-type-from-param value)
      (define types
        (cond
          ((and (list? value) (equal? 'or (car value)))
           (cdr value))
          ((symbol? value) (list value))
          ((equal? #f value) (list))
          (else (error (string-append "Bad signature: " (write* value))))))
      (filter symbol? types))

    (define (parse-type-from-return value)
      (cond
        ((or (equal? '* value)
             (equal? '... value)
             (equal? 'undefined value)
             (equal? #f value))
         (list))
        ((symbol? value) (list value))
        ((and (list? value)
              (or (equal? 'values (car value))
                  (equal? 'or (car value))))
         (apply append (map parse-type-from-return (cdr value))))
        (else (list))))

    (define (extract-lambda-return-types signature)
      (parse-type-from-return (caddr signature)))

    (define (extract-case-lambda-return-types signature)
      (apply append (map
                      (lambda (case)
                        (parse-type-from-return (cadr case)))
                      (cdr signature))))

    (define (make-type-maps specs)
      (define (make-entries strict)
        (define entries*
          (map
            (lambda (spec)
              (define supertypes (index-entry-supertypes spec))
              (define type (index-entry-name spec))
              (cond
                ((or (not supertypes) (null? supertypes)) '())
                ((and strict (not (null? (cdr supertypes)))) '())
                (else (map
                        (lambda (supertype)
                          (cons type supertype))
                        supertypes))))
            specs))
        (apply append entries*))
      (define entries-strict (make-entries #t))
      (define entries-loose (make-entries #f))
      (define supertype-map (make-multivalue-alist entries-loose car cdr))
      (define subtype-loose-map (make-multivalue-alist entries-loose cdr car))
      (define subtype-strict-map (make-multivalue-alist entries-strict cdr car))
      (values supertype-map subtype-strict-map subtype-loose-map))

    (define (make-multivalue-alist entries key-proc value-proc)
      (let loop ((alist '())
                 (entries entries))
        (cond
          ((null? entries) alist)
          (else (let* ((e (car entries))
                       (key (key-proc e))
                       (value (value-proc e)))
                  (cond
                    ((assoc key alist) 
                     => (lambda (alist-entry)
                          (define new-list (lset-adjoin equal? (cdr alist-entry) value))
                          (define new-alist
                            (if (eq? new-list (cdr alist-entry))
                                alist
                                (alist-cons key new-list (alist-delete key alist)))  )
                          (loop new-alist (cdr entries))))
                    (else (loop (alist-cons key (list value) alist)
                                (cdr entries)))))))))

    (define (flatten-type type-map types)
      (let loop ((result '())
                 (q types))
        (define to-add (lset-difference equal? q result))
        (define new-q* (map 
                         (lambda (type)
                           (cond
                             ((assoc type type-map) => cdr)
                             (else '())))
                         to-add))
        (define new-q
          (delete-duplicates
            (apply append new-q*)))

        (if (null? to-add)
            result
            (loop (append to-add result)
                  new-q))))

))
