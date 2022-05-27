(define-library
  (scmindex domain)
  (import (scheme base)
          (scheme read)
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

    index-entry->json
    json->func
    search-result->json)

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
      (supertypes index-entry-supertypes))

    (define (read* str)
      (define port (open-input-string str))
      (read port))

    (define (index-entry->json func)
      `((lib . ,(->string (index-entry-lib func)))
        (name . ,(symbol->string (index-entry-name func)))
        (param_names . ,(list->vector (map ->string (index-entry-param-names func))))
        (signature . ,(->string (index-entry-signature func)))
        (param_signatures . ,(->string (index-entry-param-signatures func)))
        (syntax_param_signatures . ,(->string (index-entry-syntax-param-signatures func)))
        (tags . ,(list->vector (map symbol->string (index-entry-tags func))))
        (param_types . ,(list->vector (map symbol->string (index-entry-param-types func))))
        (return_types . ,(list->vector (map symbol->string (index-entry-return-types func))))
        (parameterized_by . ,(list->vector (index-entry-parameterized-by func)))
        (super_types . ,(list->vector (map symbol->string (index-entry-supertypes func))))))

    (define (json->func json)
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
        (get 'supertypes 'symbol-lst '())))

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

))
