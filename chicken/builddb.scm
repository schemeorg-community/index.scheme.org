(import (chicken file)
        (chicken process-context)
        json
        matchable
        srfi-1)

(define (main)
  (define index-file-name (list-ref (command-line-arguments) 0))
  (define output-file-name (list-ref (command-line-arguments) 1))
  (process-types-index index-file-name output-file-name))

(define (make-json-sink out)
  (define empty #t)
  (display "[" out)
  (lambda (obj)
    (cond
      ((eq? obj 'eof)
       (display "]\n" out))
      (else
        (begin
          (unless empty
            (display "\n, " out))
          (when empty
            (set! empty #f))
          (json-write obj out))))))

(define (->string obj)
  (let ((out (open-output-string)))
    (write obj out)
    (get-output-string out)))

(define (process-types-index index-file-name output-file-name)

  (define (process-library lib json-sink)
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
          (define json-value (entry-definition->json exclude lib-name e))
          (when json-value
            (json-sink json-value)))
        definitions)))

  (define libraries (with-input-from-file index-file-name read))

  (with-output-to-file output-file-name
    (lambda ()
      (let ((json-sink (make-json-sink (current-output-port))))
        (for-each
          (lambda (lib)
            (process-library lib json-sink))
          libraries)
        (json-sink 'eof)))))

(define (entry-definition->json exclude lib-name e)
  (unless (string? lib-name)
    (error "lib-name must be string"))
  (cond
    ((assoc 'group e) => (lambda (group)
                           `#((entries . ,(filter values (map (lambda (e) (single-entry-definition->json exclude lib-name e)) (cdr group))))
                              (kind . "group")
                              (lib . ,lib-name)
                              (description . ,(cdr (assoc 'desc e))))))
    (else (single-entry-definition->json exclude lib-name e))))

(define (single-entry-definition->json exclude lib-name e)
  (display "\nParsing " (current-error-port) )
  (display lib-name (current-error-port) )
  (display " " (current-error-port) )
  (display (cdr (assoc 'name e)) (current-error-port))
  (define tags
    (cond
      ((assoc 'tags e) => (lambda (t) (map symbol->string (cdr t))))
      (else '())))
  (define name
    (cond
      ((assoc 'name e) => cdr)
      (else (error "name field missing"))))
  (define desc
    (cond
      ((assoc 'desc e) => cdr)
      (else "")))
  (define subsigs
    (cond
      ((assoc 'subsigs e) => (lambda (subsigs)
                               (map subsig->json (cdr subsigs))))
      (else '())))
  (define sig
    (cond
      ((assoc 'signature e) => (lambda (sig)
                                 (sig->json (cdr sig))))
      (else (error "signature field missing"))))
  (unless (string? name)
    (error "name must be string"))
  (unless (string? desc)
    (error "desc must be string"))
  (unless (string? lib-name)
    (error "lib-name must be string"))
  (if (member name exclude)
      #f
      `#((kind . "single")
         (name . ,name)
         (signature . ,sig)
         (subsignatures . ,subsigs)
         (tags . ,tags)
         (lib . ,lib-name)
         (description . ,desc))))

(define (subsig->json s)
  (define name (symbol->string (car s)))
  (define signature (cadr s))
  `#((name . ,name)
     (signature . ,(sig->json signature))))

(define (sig->json s)
  (match s
    (('lambda (param ...) return-type)
     `#((type . "function")
        (variants . (#((params . ,(map function-param->json param))
                       (return . ,(function-return->json return-type)))))))
    (('case-lambda cases ...)
     (let ((case->json (lambda (c)
                         (match c
                            (((param ...) return-type)
                             `#((params . ,(map function-param->json param))
                                (return . ,(function-return->json return-type))))))))
       `#((type . "function")
          (variants . ,(map case->json cases)))))
    (('value return)
     `#((type . "value")
        (value . ,(function-return->json return)))) 
    (('syntax-rules (keyword ...) cases ...)
     `#((type . "syntax")
        (literals . ,(map symbol->string keyword))
        (patterns . ,(map syntax-rule-pattern->json cases))))
    (('pattern pattern ...)
     (let ((pattern->string (lambda (p)
                              (let ((out (open-output-string)))
                                (write p out)
                                (get-output-string out)))))
       `#((type . "pattern")
          (patterns . ,(map pattern->string pattern)))))
    (('list type)
     `#((type . "list")
        (element . ,(function-param->json type)))) 
    (('vector type)
     `#((type . "vector")
        (element . ,(function-param->json type)))) 
    (('alist key-type value-type)
     `#((type . "alist")
        (car . ,(function-param->json key-type))
        (cdr . ,(function-param->json value-type))))
    (_ (error "unrecognized signature"))))

(define (function-param->json param)
  (define (type->string t)
    (match t
      (#f "#f")
      ((? symbol?) (symbol->string t))
      (_ (error "Unknown type"))))
  (match param
    ((('or types ...) (? symbol? name))
     `#((name . ,(symbol->string name))
        (types . ,(map type->string types))))
    ((type (? symbol? name))
     `#((name . ,(symbol->string name))
        (types . (,(type->string type)))))
    ((? symbol? name)
     `#((name . ,(symbol->string name))
        (types . ())))
    (_ (error "Unknown param shape"))))

(define (function-return->json return)
  (match return
    (('values returns ...)
     `#((kind . "values")
        (items . ,(map function-return->json returns))))
    (('or returns ...)
     `#((kind . "or")
        (items . ,(map function-return->json returns))))
    ((? symbol?)
     `#((kind . "return")
        (type . ,(symbol->string return))))
    (#f
     `#((kind . "return")
        (type . "#f")))
    (_ (error "Unknown return shape"))))

(define (syntax-rule-pattern->json pattern)
  (define-values
    (pattern-string type)
    (match pattern
      ((p) (values (->string p) (void)))
      ((p return-type) (values (->string p) (function-return->json return-type)))))
  `#((pattern . ,pattern-string)
     (type . ,type)))

(main)
