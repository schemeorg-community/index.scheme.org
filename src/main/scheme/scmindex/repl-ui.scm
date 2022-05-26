(define-library
  (scmindex repl-ui)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scmindex domain)
          (scmindex solr))
  (export init-repl-ui)
  (begin
    ;;TODO move to util
    (define (->string obj)
      (define port (open-output-string))
      (write obj port)
      (get-output-string port))
    (define (init-repl-ui config solr-client solr-core)
      (let loop ()
        (define value (read))
        (if (eof-object? value)
            #f
            (begin
              (handle-query solr-client solr-core value)
              (loop)))))
    (define (param alist key default)
      (cond
        ((assoc key alist) => cdr)
        (else default)))
    (define (handle-query solr-client solr-core q)
      (call/cc
        (lambda (k)
          (with-exception-handler
            (lambda (err)
              (write `(error ,(->string err)))
              (k #t))
            (lambda ()
              (do-handle-query solr-client solr-core q))))))
    (define (do-handle-query solr-client solr-core q)
      (define resp-value
        (cond
          ((equal? (car q) 'search)
           (let* ((p (cdr q))
                  (start (param p 'start 0))
                  (rows (param p 'rows 40))
                  (query (param p 'query ""))
                  (libs (param p 'libs '()))
                  (param-types (param p 'param-types '()))
                  (return-types (param p 'return-types '()))
                  (parameterized-by (param p 'parameterized '()))
                  (tags (param p 'tags '()))
                  (filter-params-loose? (param p 'filter-params-loose? #t)))
             (search-result->json (exec-solr-query solr-client solr-core start rows query libs param-types return-types parameterized-by tags filter-params-loose?))))
          ((equal? (car q) 'tags) (list->vector (solr-facet-values solr-client solr-core 'tag)))
          ((equal? (car q) 'libs) (list->vector (solr-facet-values solr-client solr-core 'lib)))
          ((equal? (car q) 'params) (list->vector (solr-facet-values solr-client solr-core 'param_types)))
          ((equal? (car q) 'returns) (list->vector (solr-facet-values solr-client solr-core 'return_types)))
          (else (error "Unknown request endpoint"))))
      (write resp-value))

))
