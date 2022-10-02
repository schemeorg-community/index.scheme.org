#|
    This module contains core domain entities / records,
    and misc conversion methods to and from json.
|#
(define-library
  (scmindex domain)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (only (srfi 1) filter)
          (srfi 95)
          (arvyy interface)
          (scheme write)
          (scmindex util))
  (export

    make-search-result
    search-result?
    search-result-items
    search-result-total
    search-result-libs
    search-result-params
    search-result-tags
    search-result-returns
    search-result-parameterized-by

    make-search-result-facet
    search-result-facet?
    search-result-facet-value
    search-result-facet-count

    make-index-entry
    index-entry?
    index-entry-lib
    index-entry-name
    index-entry-param-names
    index-entry-signature
    index-entry-param-signatures
    index-entry-syntax-param-signatures
    index-entry-tags
    index-entry-param-types
    index-entry-return-types
    index-entry-parameterized-by
    index-entry-supertypes
    index-entry-spec-values

    make-filter-entry
    filter-entry?
    filter-entry-filter
    filter-entry-source
    filter-entry-target

    index-entry->json
    search-result->json
    index-entry->alist
    alist->index-entry
    
    make-searcher
    save-index-entries
    query-index
    facet-values

    make-settings
    deploy-setting/port
    deploy-setting/spec-index
    deploy-setting/solr-embed
    deploy-setting/solr-home
    deploy-setting/solr-url
    deploy-setting/solr-core
    deploy-setting/cache-templates
    deploy-setting/page-size
    deploy-setting/serve-static
    deploy-setting/filterset-index
    deploy-setting/sqlite-location
    deploy-setting/enable-user-settings
    user-setting/page-size
    user-setting/param-filter-loose
    user-setting/ctrl-f-override

    make-filterset-store
    save-filterset-entries
    save-filterset-names
    get-name
    code-list
    source-list
    get-target
    get-source
    
    transform-request-libraries
    transform-result-libraries)

  (begin

    (define-record-type <scmindex-function>
      (make-index-entry
        lib
        name
        param-names
        signature
        param-signatures
        syntax-param-signatures
        tags
        param-types
        return-types
        parameterized-by
        spec-values
        supertypes)

      index-entry?

      (lib index-entry-lib)
      (name index-entry-name)
      (param-names index-entry-param-names)
      (signature index-entry-signature)
      (param-signatures index-entry-param-signatures)
      (syntax-param-signatures index-entry-syntax-param-signatures)
      (tags index-entry-tags)
      (param-types index-entry-param-types)
      (return-types index-entry-return-types)
      (parameterized-by index-entry-parameterized-by)
      (spec-values index-entry-spec-values)
      (supertypes index-entry-supertypes))

    (define (index-entry->json func)
      (define type
        (case (car (index-entry-signature func))
          ((lambda) 'function)
          ((syntax-rules) 'syntax)
          (else 'value)))
      `((lib . ,(index-entry-lib func))
        (name . ,(symbol->string (index-entry-name func)))
        (type . ,(symbol->string type))
        (func_signature . ,(if (equal? 'function type)
                             (jsonify-signature (index-entry-signature func))
                             'null))
        (syntax_signature . ,(if (equal? 'syntax type)
                             (jsonify-signature (index-entry-signature func))
                             'null))
        (func_param_signatures . ,(if (equal? 'function type)
                                     (jsonify-function-subsigs (index-entry-param-signatures func))
                                     #()))
        (syntax_subsyntax_signatures . ,(if (equal? 'syntax type)
                                           (jsonify-syntax-subsigs (index-entry-param-signatures func))
                                           #()))
        (syntax_param_signatures . ,(if (equal? 'syntax type)
                                        (jsonify-syntax-param-signatures (index-entry-syntax-param-signatures func))
                                        #()))
        (tags . ,(list->vector (map symbol->string (index-entry-tags func))))
        (param_types . ,(list->vector (map symbol->string (index-entry-param-types func))))
        (return_types . ,(list->vector (map symbol->string (index-entry-return-types func))))
        (parameterized_by . ,(list->vector (index-entry-parameterized-by func)))
        (spec_values . ,(list->vector (map spec-value->json (index-entry-spec-values func))))
        (super_types . ,(list->vector (map symbol->string (index-entry-supertypes func))))))

    (define (index-entry->alist f)
      `((lib . ,(index-entry-lib f))
        (name . ,(index-entry-name f))
        (param-names . ,(index-entry-param-names f))
        (signature . ,(index-entry-signature f))
        (param-signatures . ,(index-entry-param-signatures f))
        (syntax-param-signatures . ,(index-entry-syntax-param-signatures f))
        (tags . ,(index-entry-tags f))
        (param-types . ,(index-entry-param-types f))
        (return-types . ,(index-entry-return-types f))
        (parameterized-by . ,(index-entry-parameterized-by f))
        (spec-values . ,(index-entry-spec-values f))
        (supertypes . ,(index-entry-supertypes f))))

    (define (alist->index-entry a)
      (make-index-entry
        (cdr (assoc 'lib a))
        (cdr (assoc 'name a))
        (cdr (assoc 'param-names a))
        (cdr (assoc 'signature a))
        (cdr (assoc 'param-signatures a))
        (cdr (assoc 'syntax-param-signatures a))
        (cdr (assoc 'tags a))
        (cdr (assoc 'param-types a))
        (cdr (assoc 'return-types a))
        (cdr (assoc 'parameterized-by a))
        (cdr (assoc 'spec-values a))
        (cdr (assoc 'supertypes a))))

    (define (jsonify-lambda-param p)
      (cond
        ((list? p)
         (let* ((type (car p))
                (name (symbol->string (cadr p)))
                (is-or? (and (list? type) (equal? 'or (car type)))))
           (if is-or?
               `((name . ,name) (types . ,(list->vector (map write* (cdr type)))))
               `((name . ,name) (types . #(,(write* type)))))))
        (else `((name . ,(symbol->string p)) (types . #())))))

    (define (jsonify-lambda-return r)
      (cond
        ((list? r)
         `((kind . ,(symbol->string (car r)))
           (items . ,(list->vector (map jsonify-lambda-return (cdr r))))
           (type . "")))
        (else `((kind . "return") (items . #()) (type . ,(write* r))))))

    (define (jsonify-signature sig)
      (define (jsonify-lambda l)
        `((params . ,(list->vector (map jsonify-lambda-param (car l))))
          (return . ,(jsonify-lambda-return (cadr l)))))
      (define (jsonify-syntax s)
        (define literals (list->vector (map symbol->string (car s))))
        (define patterns (list->vector (map (lambda (pattern)
                                              `((pattern . ,(write* (car pattern)))
                                                (type . ,(if (> (length pattern) 1)
                                                             (write* (cadr pattern))
                                                             'null))))
                                            (cdr s))))
        `((literals . ,literals)
          (patterns . ,patterns)))
      (case (car sig)
        ((lambda) (jsonify-lambda (cdr sig)))
        ((syntax-rules) (jsonify-syntax (cdr sig)))
        (else #f)))

    (define (jsonify-function-subsigs subsigs)
      (list->vector (map (lambda (e)
                           (define name (symbol->string (car e)))
                           (define sig (jsonify-signature (cadr e)))
                           `((name . ,name)
                             (signature . ,sig))) 
                         subsigs)))

    (define (pattern->string pattern)
      (define p (open-output-string))
      (if (and (list? pattern)
               (not (null? pattern))
               (equal? '_append (car pattern)))
          (for-each
            (lambda (fragment)
              (display fragment p)
              (display " " p))
            (cdr pattern))
          (display pattern p))
      (get-output-string p))

    (define (jsonify-syntax-subsigs subsigs)
      (list->vector (map (lambda (e)
                           (define name (symbol->string (car e)))
                           (define patterns (list->vector (map pattern->string (cdr e))))
                           `((name . ,name)
                             (patterns . ,patterns))) 
                         subsigs)))

    (define (jsonify-syntax-param-signatures syntax-sigs)
      (list->vector (map (lambda (e)
                           `((name . ,(symbol->string (car e)))
                             (type . ,(symbol->string (cadr e))))) 
                         syntax-sigs)))

    (define (spec-value->json block)
      (define vals 
        (map (lambda (e)
               `((value . ,(car e))
                 (desc . ,(cadr e))))
          (cdr block)))
      `((field . ,(symbol->string (car block)))
        (values . ,(list->vector vals))))

    (define (json->spec-value block)
      (define vals
        (map
          (lambda (e)
            (list (cdr (assoc 'value e))
                  (cdr (assoc 'desc e))))
          (vector->list (cdr (assoc 'values block)))))
      (define field (string->symbol (cdr (assoc 'field block))))
      (cons field vals))

    (define-record-type <search-result>
      (make-search-result items total libs params tags returns parameterized-by)
      search-result?
      (items search-result-items)
      (total search-result-total)
      (libs search-result-libs)
      (params search-result-params)
      (tags search-result-tags)
      (returns search-result-returns)
      (parameterized-by search-result-parameterized-by))

    (define-record-type <search-result-facet>
      (make-search-result-facet value count)
      search-result-facet?
      (value search-result-facet-value)
      (count search-result-facet-count))

    (define-record-type <filter-entry>
      (make-filter-entry filter source target)
      filter-entry?
      (filter filter-entry-filter)
      (source filter-entry-source)
      (target filter-entry-target))

    (define (search-result->json sr)
      `((items . ,(list->vector (map (lambda (e) (index-entry->json e)) (search-result-items sr))))
        (total . ,(search-result-total sr))
        (libs . ,(list->vector (map search-result-facet->json (search-result-libs sr))))
        (params . ,(list->vector (map search-result-facet->json (search-result-params sr))))
        (returns . ,(list->vector (map search-result-facet->json (search-result-returns sr))))
        (tags . ,(list->vector (map search-result-facet->json (search-result-tags sr))))
        (parameterized . ,(list->vector (map search-result-facet->json (search-result-parameterized-by sr))))))

    (define (search-result-facet->json f)
      `((value . ,(search-result-facet-value f))
        (count . ,(search-result-facet-count f))))

    (define-interface 
      make-searcher
      (save-index-entries entries)
      (query-index start page-size query libs param-types return-types parameterized-by tags filter-params-loose?)
      (facet-values libs facet))

    (define-interface
      make-filterset-store
      (save-filterset-entries entries)
      (save-filterset-names names)
      (get-name code)
      (code-list)
      (source-list code)
      (get-target code source)
      (get-source code target))

    (define-interface
      make-settings
      (deploy-setting/port)
      (deploy-setting/spec-index)
      (deploy-setting/solr-embed)
      (deploy-setting/solr-home)
      (deploy-setting/solr-url)
      (deploy-setting/solr-core)
      (deploy-setting/cache-templates)
      (deploy-setting/page-size)
      (deploy-setting/serve-static)
      (deploy-setting/filterset-index)
      (deploy-setting/sqlite-location)
      (deploy-setting/enable-user-settings)
      (user-setting/page-size)
      (user-setting/param-filter-loose)
      (user-setting/ctrl-f-override)
      )

    (define (transform-request-libraries filterset-store filtername libraries)
      (if (null? libraries)
          (source-list filterset-store filtername)
          (map
            (lambda (lib)
              (get-source filterset-store filtername lib))
            libraries)))

    (define (transform-result-libraries filterset-store filtername search-result)
      (define new-items
        (map
          (lambda (item)
            (make-index-entry
              (get-target filterset-store filtername (index-entry-lib item))
              (index-entry-name item)
              (index-entry-param-names item)
              (index-entry-signature item)
              (index-entry-param-signatures item)
              (index-entry-syntax-param-signatures item)
              (index-entry-tags item)
              (index-entry-param-types item)
              (index-entry-return-types item)
              (index-entry-parameterized-by item)
              (index-entry-spec-values item)
              (index-entry-supertypes item)))
          (search-result-items search-result)))
      (define new-lib-facets
        (let* ((libs (search-result-libs search-result))
               (libs (map
                       (lambda (lib-facet)
                         (define t (get-target filterset-store filtername (search-result-facet-value lib-facet)))
                         (if t
                             (make-search-result-facet t
                                                       (search-result-facet-count lib-facet))
                             #f))
                       libs))
               (libs (filter (lambda (f) f) libs))
               (libs (sort libs string<? search-result-facet-value)))
          libs))
      (make-search-result
        new-items
        (search-result-total  search-result)
        new-lib-facets
        (search-result-params search-result)
        (search-result-tags search-result)
        (search-result-returns search-result)
        (search-result-parameterized-by search-result)))

))
