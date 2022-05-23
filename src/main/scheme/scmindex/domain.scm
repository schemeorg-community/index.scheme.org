(define-library
  (scmindex domain)
  (import (scheme base)
          (scheme read)
          (scheme write))
  (export

    make-search-result
    search-result?
    search-result-items
    search-result-total
    search-result-libs
    search-result-params
    search-result-tags
    search-result-returns

    make-search-result-facet
    search-result-facet?
    search-result-facet-value
    search-result-facet-count

    make-func
    func?
    func-lib
    func-name
    func-param-names
    func-signature
    func-param-signatures
    func-syntax-param-signatures
    func-tags
    func-param-types
    func-return-types
    func-supertypes

    func->json
    json->func
    search-result->json)

  (begin

    (define-record-type <scmindex-function>
      (make-func
        lib
        name
        param-names
        signature
        param-signatures
        syntax-param-signatures
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
      (syntax-param-signatures func-syntax-param-signatures)
      (tags func-tags)
      (param-types func-param-types)
      (return-types func-return-types)
      (supertypes func-supertypes))

    ;;TODO move to util?
    (define (->string obj)
      (define port (open-output-string))
      (write obj port)
      (get-output-string port))

    (define (read* str)
      (define port (open-input-string str))
      (read port))

    (define (func->json func)
      `((lib . ,(->string (func-lib func)))
        (name . ,(symbol->string (func-name func)))
        (param_names . ,(list->vector (map ->string (func-param-names func))))
        (signature . ,(->string (func-signature func)))
        (param_signatures . ,(->string (func-param-signatures func)))
        (syntax_param_signatures . ,(->string (func-syntax-param-signatures func)))
        (tags . ,(list->vector (map symbol->string (func-tags func))))
        (param_types . ,(list->vector (map symbol->string (func-param-types func))))
        (return_types . ,(list->vector (map symbol->string (func-return-types func))))
        (super_types . ,(list->vector (map symbol->string (func-supertypes func))))))

    (define (json->func json)
      (define (get field type default)
        (cond
          ((assoc field json) =>
                              (lambda (value)
                                (case type
                                  ((sexpr) (read* (cdr value)))
                                  ((symbol) (string->symbol (cdr value)))
                                  ((symbol-lst) (map string->symbol (vector->list (cdr value))))
                                  (else (cdr value)))))
          (else default)))
      (make-func
        (get 'lib 'sexpr #f)
        (get 'name 'symbol #f)
        (get 'param_names 'symbol-lst '())
        (get 'signature 'sexpr #f)
        (get 'param_signatures 'sexpr '())
        (get 'syntax_param_signatures 'sexpr '())
        (get 'tags 'symbol-lst '())
        (get 'param_types 'symbol-lst '())
        (get 'return_types 'symbol-lst '())
        (get 'supertypes 'symbol-lst '())))

    (define-record-type <search-result>
      (make-search-result items total libs params tags returns)
      search-result?
      (items search-result-items)
      (total search-result-total)
      (libs search-result-libs)
      (params search-result-params)
      (tags search-result-tags)
      (returns search-result-returns))

    (define-record-type <search-result-facet>
      (make-search-result-facet value count)
      search-result-facet?
      (value search-result-facet-value)
      (count search-result-facet-count))

    (define (search-result->json sr)
      `((items . ,(list->vector (map func->json (search-result-items sr))))
        (total . ,(search-result-total sr))
        (libs . ,(list->vector (map search-result-facet->json (search-result-libs sr))))
        (params . ,(list->vector (map search-result-facet->json (search-result-params sr))))
        (returns . ,(list->vector (map search-result-facet->json (search-result-returns sr))))
        (tags . ,(list->vector (map search-result-facet->json (search-result-tags sr))))))

    (define (search-result-facet->json f)
      `((value . ,(search-result-facet-value f))
        (count . ,(search-result-facet-count f))))

))
