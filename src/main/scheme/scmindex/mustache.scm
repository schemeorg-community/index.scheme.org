(define-library
  (scmindex mustache)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme cxr)
          (only (srfi 1) iota filter)
          (arvyy httpclient))
  
  (export make-mustache-data)
  (begin


    (define (make-mustache-data 
              page
              page-size
              query
              libs
              param-types
              return-types
              tags
              data)
      
      (define current-query
        (append
          `((page . ,(number->string page)))
          (if query
              `((query . ,query))
              '())
          (map
            (lambda (lib)
              (cons 'lib lib)) 
            libs)
          (map
            (lambda (param)
              (cons 'param param)) 
            param-types)
          (map
            (lambda (return)
              (cons 'return return)) 
            return-types)
          (map
            (lambda (tag)
              (cons 'tag tag)) 
            tags)))
      
      `((query . ,query)
        (facets . #(((name . "lib")
                     (title . "Library")
                     (options . ,(make-mustache-facet (cdr (assoc 'lib data)) libs)))
                    ((name . "tag")
                     (title . "Tag")
                     (options . ,(make-mustache-facet (cdr (assoc 'tag data)) tags)))
                    ((name . "param")
                     (title . "Parameter type")
                     (options . ,(make-mustache-facet (cdr (assoc 'param data)) param-types)))
                    ((name . "return")
                     (title . "Return type")
                     (options . ,(make-mustache-facet (cdr (assoc 'return data)) return-types)))))
        (pages . ,(make-pager-data page (ceiling (/ (cdr (assoc 'total data)) page-size)) current-query))
        (procedures . ,(vector-map make-doc-data (cdr (assoc 'procedures data))))))

    (define (make-mustache-facet facet-result selected-values)
      (vector-map
        (lambda (f)
          (cons
            (cons 'selected (and (member (cdr (assoc 'value f)) selected-values) #t))
            f))
        facet-result))
    
    (define (make-pager-data page total-pages query)
      (define query-without-page
        (filter
          (lambda (e)
            (not (equal? 'page (car e))))
          query))
      (list->vector
        (map
          (lambda (p)
            (define link-query
              (cons
                (cons 'page (number->string p))
                query-without-page))
            (define link
              (if (= p page)
                  #f
                  (string-append "?" (encode-query link-query))))
            `((number . ,p)
              (link . ,link)))
          (iota total-pages 1))))
    
    (define (make-doc-data doc)
      (define (get key)
        (cond
          ((assoc key doc) => cdr)
          (else #f)))
      (define (make-link type param?)
        (if param?
            (string-append "?" (encode-query `((return . ,(symbol->string type)))))
            (string-append "?" (encode-query `((param . ,(symbol->string type)))))))
      (define param-signatures
        (map
          (lambda (param-sig)
            (define data (make-signature-data (symbol->string (car param-sig))
                                              (cadr param-sig)
                                              (lambda args #f)
                                              #t))
            `((signature . ,data)))
          (read (open-input-string (cdr (assoc 'param_signatures doc))))))
      `((signature . ,(make-signature-data (cdr (assoc 'name doc)) 
                                           (read (open-input-string (cdr (assoc 'signature doc)))) 
                                           make-link 
                                           #f))
        (param_signatures . ,(list->vector param-signatures))
        (has_param_signatures . ,(not (null? param-signatures)))
        (tags . ,(get 'tags))
        (lib . ,(get 'lib))))
    
    
    (define (make-signature-data name sig link-maker sub?)

      (define (make-return-parts return)
        (cond
          ((or (equal? '* return)
               (equal? 'undefined return)
               (equal? '... return))
           `(((plain . ((text . ,(symbol->string return))
                        (class . "sig-right-margin"))))))
          ((symbol? return)
           `(((important . ((text . ,(symbol->string return))
                            (link . ,(link-maker return #f))
                            (class . ,(if sub? "sig-muted sig-right-margin" "sig-type sig-right-margin")))))))
          ((list? return)
           (append
             `(((plain . ((text . "("))))
               ((plain . ((text . ,(symbol->string (car return)))
                          (class . "sig-right-margin")))))
             (apply append (map make-return-parts (cdr return)))
             `(((plain . ((text . ")")))))))))
      
      (define start-parts
        `#(((plain . ((text . "("))))
           ((important . ((text . ,name)
                          (link . #f)
                          (class . ,(if sub? "sig-muted" "sig-name")))))))
      (define param-parts*
        (map 
          (lambda (param)
            (cond
              ((symbol? param)
               `#(
                 ((plain . ((text . ,(symbol->string param)))))))
              ((list? param)
               `#(
                 ((plain . ((text . "("))))
                 ((important . ((text . ,(symbol->string (car param)))
                                (link . ,(link-maker (car param) #t))
                                (class . ,(if sub? "sig-muted" "sig-type")))))
                 ((plain . ((text . ,(symbol->string (cadr param))))))
                 ((plain . ((text . ")"))))
                 
                 ))))
          (cadr sig)))
      
      (define return-parts (list->vector (append 
                                           `(((plain . ((text . ")")
                                                        (class . "sig-right-margin"))))
                                             ((plain . ((text . "=>")
                                                        (class . "sig-right-margin")))))
                                           (make-return-parts (caddr sig)))))
      
      (define chunks* (append (list start-parts)
                              param-parts*
                              (list return-parts)))
      (define chunks (apply vector chunks*))
      
      `((chunks . ,chunks)))))
