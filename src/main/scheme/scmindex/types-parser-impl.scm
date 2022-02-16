(define-record-type <scmindex-function>
                    (make-func 
                      lib 
                      name 
                      param-names 
                      signature
                      param-signatures
                      tags 
                      param-types 
                      return-types
                      supertypes)

                    func?

                    (lib func-lib)
                    (name func-name)
                    (param-names func-param-names)
                    (signature func-signature)
                    (param-signatures func-param-signatures)
                    (tags func-tags)
                    (param-types func-param-types)
                    (return-types func-return-types)
                    (supertypes func-supertypes)
                    
                    )


(define (read-specs index-file)
  (with-input-from-file 
    index-file
    (lambda ()
      (define lst (read))
      (define funcs*
        (map
          (lambda (entry)
            (with-input-from-file
              (cdr entry)
              (lambda ()
                (read-spec (car entry)
                           (read)))))
          lst))
      (apply append funcs*))))

(define (list-ref/f lst index)
  (cond
    ((null? lst) '())
    ((= index 0) (car lst))
    (else (list-ref/f (cdr lst) (- index 1)))))

(define (read-spec lib input)
  (map
    (lambda (entry)
      (define name (list-ref entry 0))
      (define signature (list-ref entry 1))
      (define proc?
        (cond
          ((equal? (car signature) 'lambda) #t)
          ((equal? (car signature) 'syntax-rules) #f)
          (else (error (string-append "Unrecognized signature for " (->string name))))))
      (define tags (list-ref/f entry 2))
      (define param-signatures (list-ref/f entry 3))
      (define supertypes (if proc? (list-ref/f entry 4) '()))
      (define param-names (if proc? (extract-param-names signature) (extract-syntax-names signature)))
      (define param-types (if proc? (extract-param-types signature) '()))
      (define return-types (if proc? (extract-return-types signature) '()))
      (make-func lib name param-names signature param-signatures tags param-types return-types supertypes))
    input))

(define (extract-param-names signature)
  (define params-list (cadr signature))
  (define lst* (map
                 (lambda (param)
                   (cond
                     ((list? param) (list (cadr param)))
                     ((equal? '... param) (list))
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

(define (extract-param-types signature)
  (define params-list (cadr signature))
  (define lst* (map
                 (lambda (param)
                   (cond
                     ((list? param) (list (car param)))
                     (else (list))))
                 params-list))
  (apply append lst*))

(define (extract-return-types signature)
  (define (extract value)
    (cond
      ((equal? '* value) (list))
      ((equal? '... value) (list))
      ((equal? 'undefined value) (list))
      ((symbol? value) (list value))
      ((and (list? value)
            (or (equal? 'values (car value))
                (equal? 'or (car value))))
       (apply append (map extract (cdr value))))
      (else (list))))
  (when (< (length signature) 3)
    (error (string-append "Bad signature: " (->string signature))))
  (extract (caddr signature)))

(define (make-type-maps funcs)
  (define entries*
    (map
      (lambda (func)
        (define supertypes (func-supertypes func))
        (define type (func-name func))
        (if (not supertypes)
            '()
            (map
              (lambda (supertype)
                (cons type supertype))
              supertypes)))
      funcs))
  (define entries (apply append entries*))
  (define supertype-map (make-multivalue-alist entries car cdr))
  (define subtype-map (make-multivalue-alist entries cdr car))
  (values supertype-map subtype-map))

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

(define (->string obj)
  (define port (open-output-string))
  (write obj port)
  (get-output-string port))

(define (func->json func)
  `((lib . ,(->string (func-lib func)))
    (name . ,(->string (func-name func)))
    (param_names . ,(list->vector (map ->string (func-param-names func))))
    (signature . ,(->string (func-signature func)))
    (param_signatures . ,(->string (func-param-signatures func)))
    (tags . ,(list->vector (map ->string (func-tags func))))
    (param_types . ,(list->vector (map ->string (func-param-types func))))
    (return_types . ,(list->vector (map ->string (func-return-types func))))
    (super_types . ,(list->vector (map ->string (func-supertypes func))))))
