#|
    This module contains core domain entities / records,
    and misc conversion methods to and from json.
|#
(define-library
  (scmindex domain)
  (import (scheme base)
          (scheme read)
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
    json->index-entry
    search-result->json
    
    make-searcher
    save-index-entries
    query-index
    facet-values

    make-filterset-store
    save-filterset-entries
    name-list
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
      `((lib . ,(write* (index-entry-lib func)))
        (name . ,(symbol->string (index-entry-name func)))
        (param_names . ,(list->vector (map write* (index-entry-param-names func))))
        (signature . ,(write* (index-entry-signature func)))
        (param_signatures . ,(write* (index-entry-param-signatures func)))
        (syntax_param_signatures . ,(write* (index-entry-syntax-param-signatures func)))
        (tags . ,(list->vector (map symbol->string (index-entry-tags func))))
        (param_types . ,(list->vector (map symbol->string (index-entry-param-types func))))
        (return_types . ,(list->vector (map symbol->string (index-entry-return-types func))))
        (parameterized_by . ,(list->vector (index-entry-parameterized-by func)))
        (spec_values . ,(list->vector (map spec-value->json (index-entry-spec-values func))))
        (super_types . ,(list->vector (map symbol->string (index-entry-supertypes func))))))

    (define (spec-value->json block)
      (define vals 
        (map (lambda (e)
               `((value . ,(car e))
                 (desc . ,(cadr e))))
          (cdr block)))
      `((field . ,(symbol->string (car block)))
        (values . ,(list->vector vals))))

    (define (json->index-entry json)
      (define (get field type default)
        (cond
          ((assoc field json) =>
                              (lambda (value)
                                (case type
                                  ((sexpr) (read* (cdr value)))
                                  ((symbol) (string->symbol (cdr value)))
                                  ((symbol-lst) (map string->symbol (vector->list (cdr value))))
                                  ((string-lst) (vector->list (cdr value)))
                                  (else (cdr value)))))
          (else default)))
      (make-index-entry
        (get 'lib 'sexpr #f)
        (get 'name 'symbol #f)
        (get 'param_names 'symbol-lst '())
        (get 'signature 'sexpr #f)
        (get 'param_signatures 'sexpr '())
        (get 'syntax_param_signatures 'sexpr '())
        (get 'tags 'symbol-lst '())
        (get 'param_types 'symbol-lst '())
        (get 'return_types 'symbol-lst '())
        (get 'parameterized_by 'string-lst '())
        (cond
          ((assoc 'spec_values json) => (lambda (value)
                                          (map
                                            json->spec-value
                                            (vector->list (cdr value)))))
          (else '()))
        (get 'supertypes 'symbol-lst '())))

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
      `((items . ,(list->vector (map index-entry->json (search-result-items sr))))
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
      (name-list)
      (source-list filtername)
      (get-target filtername source)
      (get-source filtername target))

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
