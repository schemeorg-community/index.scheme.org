(define-library
  (scmindex mustache)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scheme cxr)
          (only (srfi 1) iota filter find)
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
      (define near/start (max 1 (- page 3)))
      (define near/end (min total-pages (+ page 3)))
      (define near-range (iota (+ 1 near/end (- near/start)) near/start))
      (define begining
        (case near/start
          ((1) '())
          ((2) '(1))
          (else '(1 #f))))
      (define ending
        (case (- total-pages near/end)
          ((0) '())
          ((1) (list total-pages))
          (else (list #f total-pages))))
      (define shown-pages
        (append begining near-range ending))
      
      (list->vector
        (map
          (lambda (p)
            (if (not p)
                `((pager-gap . #t))
                (let* ((link-query
                        (cons
                          (cons 'page (number->string p))
                          query-without-page))
                       (link
                         (if (= p page)
                             #f
                             (string-append "?" (encode-query link-query)))))
                  `((number . ,p)
                    (link . ,link)
                    (pager-gap . #f)))))
          shown-pages)))
    
    (define (make-doc-data doc)
      (define (get key)
        (cond
          ((assoc key doc) => cdr)
          (else #f)))
      (define signature (read (open-input-string (get 'signature))))
      (define (make-link type param?)
        (if param?
            (string-append "?" (encode-query `((return . ,(symbol->string type)))))
            (string-append "?" (encode-query `((param . ,(symbol->string type)))))))
      (define-values
        (param-signatures subsyntax-signatures signature-sd)
        (case (car signature)
          ((lambda)
           (values (map
                     (lambda (param-sig)
                       (make-signature-sexpr-data (symbol->string (car param-sig))
                                                  (cadr param-sig)
                                                  (lambda args #f)
                                                  #t))
                     (read (open-input-string (get 'param_signatures))))
                   '()
                   (make-signature-sexpr-data (get 'name) 
                                       signature 
                                       make-link 
                                       #f)))
          ((syntax-rules)
           (values '()
                   (let ((literals (cadr signature)))
                    (map
                      (lambda (param)
                        `((name . ,(symbol->string (car param)))
                          (rules . #(,@(map
                                         (lambda (rule)
                                           (make-subsyntax-signature-sexpr-data literals rule))
                                         (cdr param))))))
                      (read (open-input-string (get 'param_signatures)))))
                   (make-syntax-signature-sexpr-data (get 'name)
                                                     signature)))
          ((value)
           (values '()
                   '()
                   (make-value-signature-sexpr-data (get 'name)
                                                    signature
                                                    make-link)))))
      `((signature . ,signature-sd)
        (param_signatures . ,(list->vector param-signatures))
        (has_param_signatures . ,(not (null? param-signatures)))
        (subsyntax_signatures . ,(list->vector subsyntax-signatures))
        (has_subsyntax_signatures . ,(not (null? subsyntax-signatures)))
        (tags . ,(get 'tags))
        (lib . ,(get 'lib))))
    
    (define (make-subsyntax-signature-sexpr-data literals rule)
      (define (term-handler term)
        (cond
          ((find (lambda (el) (equal? term el)) literals)
           `((text . ,(symbol->string term))
             (sub-exprs . #f)
             (class . "bright-syntax")))
          ((or (equal? '... term)
               (equal? '|#| term))
           `((text . ,(symbol->string term))
             (sub-exprs . #f)
             (class . "muted")))
          (else 
            `((class . "sexpr-flex muted")
              (sub-exprs . #(((html . "&#x27E8")
                              (sub-exprs . #f))
                             ((text . ,(symbol->string term))
                              (sub-exprs . #f))
                             ((html . "&#x27E9")
                              (sub-exprs . #f))))))))
      `((class . "sexpr-flex")
        (sub-exprs . #(,@(make-sexpr-data (cons rule '()) term-handler 1)))))
    
    (define (make-syntax-signature-sexpr-data name signature)
      (define name-symbol (string->symbol name))
      (define rules
        (map 
          (lambda (r)
            (cons name-symbol (cdar r)))
          (cddr signature)))
      (define literals (cadr signature))
      (define (term-handler term)
        (cond
          ((or (equal? name-symbol term)
               (find (lambda (el) (equal? term el)) literals))
           `((text . ,(symbol->string term))
             (sub-exprs . #f)
             (class . "bright-syntax")))
          ((equal? '... term)
           `((text . "...")
             (sub-exprs . #f)
             (class . "muted")))
          (else 
            `((class . "sexpr-flex muted")
              (sub-exprs . #(((html . "&#x27E8")
                              (sub-exprs . #f))
                             ((text . ,(symbol->string term))
                              (sub-exprs . #f))
                             ((html . "&#x27E9")
                              (sub-exprs . #f))))))))
      (define rules-sds
        (map
          (lambda (rule)
            `((class . "sexpr-flex")
              (sub-exprs . #(,@(make-sexpr-data (cons rule '()) term-handler 0)))))
          rules))
      `((class . "sexpr-flex-col")
        (sub-exprs . #(,@rules-sds))))
    
    (define (make-sexpr-data sexpr term-handler depth)
      (define (wrap-list sexpr)
        (define new-depth
          (if (pair? sexpr)
              (+ 1 depth)
              depth))
        (define processed-lst 
          (make-sexpr-data sexpr term-handler new-depth))
        (if (pair? sexpr)
            `(((class . "sexpr-flex")
               (sub-exprs . #(((class . ,(string-append "syntaxbracket-" (number->string depth)))
                               (text . "(")
                               (sub-exprs . #f))
                              ,@processed-lst
                              ((class . ,(string-append "syntaxbracket-" (number->string depth)))
                               (text . ")")
                               (sub-exprs . #f))))))
            processed-lst))
      
      (cond
        ((and (pair? sexpr)
              (pair? (car sexpr))
              (equal? '_append (caar sexpr)))
         (apply append (map
                         (lambda (el)
                           (make-sexpr-data (cons el '()) term-handler depth))
                         (cdar sexpr))))
        ((symbol? sexpr) 
         (list (term-handler sexpr)))
        ((and (pair? sexpr) (symbol? (cdr sexpr)))
         `(,@(wrap-list (car sexpr))
           ((class . "spacer")
            (sub-exprs . #f))
           ((class . "muted")
            (sub-exprs . #f)
            (html . "."))
           ((class . "spacer")
            (sub-exprs . #f))
           ,@(make-sexpr-data (cdr sexpr) term-handler depth)))
        ((pair? sexpr)
         `(,@(wrap-list (car sexpr))
            ((class . "spacer")
             (sub-exprs . #f))
            ,@(make-sexpr-data (cdr sexpr) term-handler depth)))
        ((null? sexpr)
         (list))
        (else (error sexpr))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (make-signature-sexpr-data name sig link-maker sub?)
      (define paren-open-sd
        `((class . "muted")
          (text . "(")
          (sub-exprs . #f)))
      (define paren-close-sd
        `((class . "muted")
          (text . ")")
          (sub-exprs . #f)))
      (define spacer-sd
        `((class . "spacer")
          (text . "")
          (sub-exprs . #f)))
      (define name-sd
        `((class . ,(if sub? "muted-name" "bright-name"))
          (text . ,name)
          (sub-exprs . #f)))
      
      (define (make-return-sd returns)
        (define (return-value->sd value)
          (cond
            ((or (equal? value '...)
                 (equal? value 'undefined)
                 (equal? value '*))
             `((class . "muted")
               (text . ,(symbol->string value))
               (sub-exprs . #f)))
            ((symbol? value)
             `((class . ,(if sub? "muted-name" "bright-type"))
               (text . ,(symbol->string value))
               (link . ,(link-maker value #f))
               (sub-exprs . #f)))
            ((list? value)
             `((class . "sexpr-flex")
               (sub-exprs . #(,paren-open-sd
                              ((class . "muted")
                               (text . ,(symbol->string (car value)))
                               (sub-exprs . #f))
                              ,@(map 
                                  (lambda (e)
                                    `((class . "sexpr-flex")
                                      (sub-exprs . #(,spacer-sd ,(return-value->sd e)))))
                                  (cdr value))
                              ,paren-close-sd))))))
        `((class . "sexpr-flex")
          (link . #f)
          (sub-exprs . #(,spacer-sd
                         ((class . "muted")
                          (link . #f)
                          (html . "&DoubleLongRightArrow;")
                          (sub-exprs . #f))
                         ,spacer-sd
                         ,(return-value->sd returns)))))
      
      (define (make-param-sds params)
        (let loop ((params params)
                   (last (null? (cdr (cadr sig))))
                   (result '()))
          (define param (car params))
          (define sd
            (cond
              ((list? param) 
               `((class . "sexpr-flex")
                  (link . #f)
                  (sub-exprs . #(,spacer-sd
                                 ,paren-open-sd
                                  ((class . ,(if sub? "muted-type" "bright-type"))
                                   (link . ,(link-maker (car param) #t))
                                   (text . ,(car param))
                                   (sub-exprs . #f))
                                  ,spacer-sd
                                  ((class . "muted")
                                   (link . #f)
                                   (text . ,(cadr param))
                                   (sub-exprs . #f))
                                  ,paren-close-sd
                                  ,@(if last (list paren-close-sd) (list))))))
              (else
                `((class . "sexpr-flex")
                  (link . #f)
                  (sub-exprs . #(,spacer-sd
                                 ((class . "muted")
                                  (link . #f)
                                  (text . ,param)
                                  (sub-exprs . #f))
                                 ,@(if last (list paren-close-sd) (list))))))))
          
          (if last
              (reverse (cons sd result))
              (loop (cdr params)
                    (null? (cddr params))
                    (cons sd result)))))
      
      (define params (cadr sig))
      (define return-sd (make-return-sd (caddr sig)))
      
      (if (null? params)
          `((class . "sexpr-flex")
            (link . #f)
            (sub-exprs . #(,paren-open-sd
                            ,name-sd
                            ,paren-close-sd
                            ,return-sd)))    
          
          `((class . "sexpr-flex")
            (link . #f)
            (sub-exprs . #(,paren-open-sd
                           ,name-sd
                           ((class . "sexpr-flex sexpr-shrink")
                            (link . #f)
                            (sub-exprs . #(,@(make-param-sds params))))
                           ,return-sd)))))
    
    (define (make-value-signature-sexpr-data name sig link-maker)
      `((class . "sexpr-flex")
        (sub-exprs . #(((class . "bright-name")
                        (text . ,name)
                        (sub-exprs . #f))
                       ((class . "spacer")
                        (sub-exprs . #f))
                       ((class . "muted")
                        (html . "&DoubleLongRightArrow;")
                        (sub-exprs . #f))
                       ((class . "spacer")
                        (sub-exprs . #f))
                       ((class . "bright-type")
                        (text . ,(symbol->string (cadr sig)))
                        (link . ,(link-maker (cadr sig) #f))
                        (sub-exprs . #f))))))))
