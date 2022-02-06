(define (default-writer obj out)
  (when obj
    (display obj out)))

(define default-lookup
  (compose-lookups 
    alist-lookup))

(define default-collection
  (compose-collections 
    vector-collection
    stream-collection))

(define (port->string port)
  (define str
    (let loop ((chunks '())
               (chunk (read-string 2000 port)))
      (if (eof-object? chunk)
          (apply string-append (reverse chunks))
          (loop (cons chunk chunks)
                (read-string 2000 port)))))
  (close-input-port port)
  str)

(define (template-get-partials template)
  (define partials
    (let loop ((template template)
               (parts '()))
     (cond
       ((null? template) parts)
       (else (let ((t (car template))
                   (rest (cdr template)))
              (cond
                ((partial? t) (loop rest
                                    (cons (partial-name t) parts)))
                ((section? t) (loop rest
                                    (append (template-get-partials (section-content t))
                                            parts)))
                (else (loop rest
                            parts))))))))
  (delete-duplicates! partials))

(define compile
  (case-lambda
    ((template) (compile/without-partials template))
    ((root partial-locator) (compile/with-partials root partial-locator))))

(define (compile/without-partials template)
  (compile/with-partials #f (lambda (partial)
                              (if partial
                                  #f
                                  template))))

(define (compile/with-partials root partial-locator)
  
  ;; returns 2 values: missing partials (found in part) and compiled part template
  (define (compile-part part resolved-partials)
    (define source (partial-locator part))
    (define in (cond
                 ((not source) "")
                 ((string? source) source)
                 ((port? source) (port->string source))
                 (else (error "Partial locator returned unrecognized type"))))
    (define template (parse (read-tokens in)))
    (define partials (template-get-partials template))
    (define missing-partials (lset-difference string=? partials resolved-partials))
    (values missing-partials template))
  
  (let loop ((unresolved (list root))
             (resolved-map '())
             (resolved-lst '()))
    (cond
      ((null? unresolved) (cons root resolved-map))
      (else (let ((part (car unresolved)))
             (define-values (unresolved* template)
                            (compile-part part resolved-lst))
             (loop (append unresolved* (cdr unresolved))
                   (cons (cons part template) resolved-map)
                   (cons part resolved-lst)))))))

(define current-lookup (make-parameter default-lookup))
(define current-collection (make-parameter default-collection))
(define current-writer (make-parameter default-writer))

(define execute
  (case-lambda
    ((compilation data)
     (let ((out (open-output-string)))
      (execute compilation data out)
      (get-output-string out)))
    ((compilation data out)
     (define root (car compilation))
     (define partials (cdr compilation))
     (define template (cdr (assoc root partials)))
     (define lookup (current-lookup))
     (define collection* (current-collection))
     (define writer (current-writer))
     (executor-execute template 
                       (list data) 
                       partials 
                       out 
                       lookup 
                       (collection-pred-proc collection*)
                       (collection-empty?-proc collection*)
                       (collection-for-each-proc collection*)
                       writer))))
