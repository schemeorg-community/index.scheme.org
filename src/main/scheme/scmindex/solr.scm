(define-library
  (scmindex solr)
  (import (scheme base)
          (arvyy solrj)
          (scmindex domain)
          (scmindex types-parser))

  (export
    index-types
    build-solr-query
    exec-solr-query
    parse-solr-response
    solr-facet-values
    solr-get-suggestions)

  (begin

    (define (solr-get-suggestions solr-client core text)
      (define resp (query solr-client core "/suggest" `((q . ,text))))
      (define suggest (cdr (assoc 'suggest resp)))
      (define nameSuggester (cdar suggest))
      (define value (cdar nameSuggester))
      (define suggestions (cdr (assoc 'suggestions value)))
      (vector-map
        (lambda (s)
          (cdr (assoc 'term s)))
        suggestions))

    (define (index-types solr-client core funcs)
      (define-values
        (supertype-map subtype-strict-map subtype-loose-map)
        (make-type-maps funcs))
      (define payload
        (list->vector
          (map
            (lambda (f)
              (define json (func->json f))
              (define extra
                `((param_subtypes_loose . ,(list->vector (map symbol->string (flatten-type subtype-loose-map (func-param-types f)))))
                  (param_subtypes . ,(list->vector (map symbol->string (flatten-type subtype-strict-map (func-param-types f)))))
                  (return_supertypes . ,(list->vector (map symbol->string (flatten-type supertype-map (func-return-types f)))))))
              (append extra json))
            funcs)))
      (parameterize ((commit-within 10))
        (delete-by-query solr-client core "*:*")
        (add solr-client core payload)
        (commit solr-client core)))

    (define (solr-facet-values solr-client core facet)
      (define solr-query `((rows . 0)))
      (define solr-resp (query solr-client core "/search" solr-query))
      (define facet-counts (cdr (assoc 'facet_counts solr-resp)))
      (define facet-fields (cdr (assoc 'facet_fields facet-counts)))
      (define facet-values (fold-facet-values (cdr (assoc facet facet-fields))))
      (map
          search-result-facet-value
          facet-values))

    (define (exec-solr-query solr-client core start page-size text libs params returns tags filter-params-loose?)
      (define body (build-solr-query start page-size text libs params returns tags filter-params-loose?))
      (define solr-resp (query solr-client core "/search" body))
      (parse-solr-response solr-resp))

    (define (build-solr-query start page-size text libs params returns tags filter-params-loose?)
      (define fq-lib
        (if (and libs (not (null? libs)))
            (let loop ((libs libs)
                       (str "lib: ("))
              (if (null? (cdr libs))
                  (list (string-append str "\"" (escape-solr-spec (car libs)) "\")"))
                  (loop (cdr libs)
                        (string-append str "\"" (escape-solr-spec (car libs)) "\" OR "))))
            `()))
      (define param-filter-field (if filter-params-loose? "param_subtypes_loose" "param_subtypes"))
      (define fq-params
        (map 
          (lambda (p)
            (string-append param-filter-field ": \"" (escape-solr-spec p) "\""))
          params))
      (define fq-returns
        (map
          (lambda (r)
            (string-append "return_supertypes: \"" (escape-solr-spec r) "\""))
          returns))
      (define fq-tags
        (map
          (lambda (t)
            (string-append "tags: \"" (escape-solr-spec t) "\""))
          tags))
      (define bq-params-types
        (map 
          (lambda (p)
            (string-append "param_types: \"" (escape-solr-spec p) "\"^5"))
          params))
      (define bq-params-subtypes
        (map 
          (lambda (p)
            (string-append "param_subtypes: \"" (escape-solr-spec p) "\"^2"))
          params))
      (define bq-params `(,@bq-params-types ,@bq-params-subtypes))
      (define bq
        (if (null? bq-params)
            '()
            `((bq . ,(list->vector bq-params)))))
      (define fq
        (let ((vals (list->vector (append fq-returns fq-params fq-lib fq-tags))))
          (if (= (vector-length vals) 0)
              '()
              `((fq . ,vals)))))
      (define q
        (if text
            `((q . ,text))
            `()))
      (define params-json
        (append q bq fq `((start . ,start) (rows . ,page-size))))
      params-json)

    (define (parse-solr-response response)
      (define resp (cdr (assoc 'response response)))
      (define total (cdr (assoc 'num-found resp)))
      (define docs (map json->func (vector->list (cdr (assoc 'docs resp)))))
      (define facet-counts (cdr (assoc 'facet_counts response)))
      (define facet-fields (cdr (assoc 'facet_fields facet-counts)))
      (define lib-facets (fold-facet-values (cdr (assoc 'lib facet-fields))))
      (define param-facets (fold-facet-values (cdr (assoc 'param_types facet-fields))))
      (define return-facets (fold-facet-values (cdr (assoc 'return_types facet-fields))))
      (define tag-facets (fold-facet-values (cdr (assoc 'tags facet-fields))))

      (make-search-result
        docs
        total
        lib-facets
        param-facets
        tag-facets
        return-facets))

    (define (fold-facet-values vals)
      (map
        (lambda (e)
          (make-search-result-facet
            (symbol->string (car e))
            (cdr e)))
        vals))

    (define (escape-solr-spec str)
      (define lst*
        (map
          (lambda (char)
            (case char
              ((#\+ #\- #\! #\( #\) #\{ #\} #\[ #\] #\^ #\" #\~ #\* #\: #\/)
               (list #\\ char))
              (else (list char))))
          (string->list str)))
      (list->string (apply append lst*)))
    
))
