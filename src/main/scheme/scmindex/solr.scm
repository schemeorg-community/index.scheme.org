#|
    The module for interacting with solr.
    The intent is to make interaction opaque, and resulting values shouldn't be tied to solr
    but converted to domain entities.
|#
(define-library
  (scmindex solr)
  (import (scheme base)
          (scheme write)
          (arvyy solrj)
          (scmindex domain)
          (scmindex types-parser)
          (scmindex util)
          (only (srfi 1) filter)
          (srfi 180))

  (export make-solr-searcher)

  (begin

    (define (make-solr-searcher solr-client core)
      (make-searcher
        ((save-index-entries entries)
         (index-types solr-client core entries))
        ((query-index start page-size text libs params returns parameterized-by tags filter-params-loose?)
         (exec-solr-query solr-client core start page-size text libs params returns parameterized-by tags filter-params-loose?))
        ((get-from-index lib name)
         (exec-solr-select solr-client core lib name))
        ((facet-values libs facet)
         (solr-facet-values solr-client core libs facet))))

    (define (index-types solr-client core funcs)
      (define-values
        (supertype-map subtype-strict-map subtype-loose-map)
        (make-type-maps funcs))
      (define payload
        (list->vector
          (map
            (lambda (f)
              (define jsondata
                `((name . ,(symbol->string (index-entry-name f)))
                  (description . ,(index-entry-description f))
                  (param_names . ,(list->vector (map symbol->string (index-entry-param-names f))))
                  (lib . ,(index-entry-lib f))
                  (tags . ,(list->vector (map symbol->string (index-entry-tags f))))
                  (parameterized_by . ,(list->vector (index-entry-parameterized-by f)))
                  (param_types . ,(list->vector (map symbol->string (index-entry-param-types f))))
                  (param_subtypes_loose . ,(list->vector (map symbol->string (flatten-type subtype-loose-map (index-entry-param-types f)))))
                  (param_subtypes . ,(list->vector (map symbol->string (flatten-type subtype-strict-map (index-entry-param-types f)))))
                  (return_types . ,(list->vector (map symbol->string (flatten-type supertype-map (index-entry-return-types f)))))
                  (return_supertypes . ,(list->vector (map symbol->string (flatten-type supertype-map (index-entry-return-types f)))))
                  (data . ,(write* (index-entry->alist f)))))
              jsondata)
            funcs)))
      (parameterize ((commit-within 10))
        (delete-by-query solr-client core "*:*")
        (add solr-client core payload)
        (commit solr-client core)))

    (define (solr-facet-values solr-client core libs facet)
      (define fq (let loop ((libs libs)
                            (str "lib: ("))
                   (if (null? (cdr libs))
                       (list (string-append str "\"" (escape-solr-spec (car libs)) "\")"))
                       (loop (cdr libs)
                             (string-append str "\"" (escape-solr-spec (car libs)) "\" OR ")))))
      (define solr-query `((rows . 0) (fq . ,fq)))
      (define solr-resp (query solr-client core "/search" solr-query))
      (define facet-counts (cdr (assoc 'facet_counts solr-resp)))
      (define facet-fields (cdr (assoc 'facet_fields facet-counts)))
      (define facet-values (fold-facet-values (cdr (assoc facet facet-fields))))
      (map
          search-result-facet-value
          (filter
            (lambda (f)
              (> (search-result-facet-count f) 0))
            facet-values)))

    (define (exec-solr-select solr-client core lib name)
      (define lib-fq
        (if lib
            (list (string-append "lib: \"" (escape-solr-spec lib) "\""))
            '()))
      (define name-fq
        (list (string-append "name_precise: \"" (escape-solr-spec name) "\"")))
      (define fq (append lib-fq name-fq))
      (define params
        `((fq . #(,@fq))
          (start . 0)
          (rows . 50)))
      (define solr-resp (query solr-client core "/search" params))
      (parse-solr-response solr-resp))

    (define (exec-solr-query solr-client core start page-size text libs params returns parameterized-by tags filter-params-loose?)
      (define body (build-solr-query start page-size text libs params returns parameterized-by tags filter-params-loose?))
      (define solr-resp (query solr-client core "/search" body))
      (parse-solr-response solr-resp))

    (define (build-solr-query start page-size text libs params returns parameterized-by tags filter-params-loose?)
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
      (define fq-parameterized-by
        (map
          (lambda (p)
            (string-append "parameterized_by: \"" (escape-solr-spec p) "\""))
          parameterized-by))
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
        (let ((vals (list->vector (append fq-returns fq-params fq-parameterized-by fq-lib fq-tags))))
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
      (define docs 
        (map (lambda (doc)
               (alist->index-entry (read* (cdr (assoc 'data doc))))) 
             (vector->list (cdr (assoc 'docs resp)))))
      (define facet-counts (cdr (assoc 'facet_counts response)))
      (define facet-fields (cdr (assoc 'facet_fields facet-counts)))
      (define lib-facets (fold-facet-values (cdr (assoc 'lib facet-fields))))
      (define param-facets (fold-facet-values (cdr (assoc 'param_types facet-fields))))
      (define return-facets (fold-facet-values (cdr (assoc 'return_types facet-fields))))
      (define parameterized-by-facets (fold-facet-values (cdr (assoc 'parameterized_by facet-fields))))
      (define tag-facets (fold-facet-values (cdr (assoc 'tags facet-fields))))

      (make-search-result
        docs
        total
        lib-facets
        param-facets
        tag-facets
        return-facets
        parameterized-by-facets))

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
