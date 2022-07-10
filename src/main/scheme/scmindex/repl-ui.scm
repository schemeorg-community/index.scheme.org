#|
    This module defines repl interaction loop through standard input / output
|#
(define-library
  (scmindex repl-ui)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (scmindex domain)
          (scmindex util))
  (export init-repl-ui)
  (begin
    (define (init-repl-ui config searcher filterset-store)
      (let loop ()
        (define value (read))
        (if (eof-object? value)
            #f
            (begin
              (handle-query searcher filterset-store value)
              (loop)))))
    (define (param alist key default)
      (cond
        ((assoc key alist) => cdr)
        (else default)))
    (define (handle-query searcher filterset-store q)
      (call/cc
        (lambda (k)
          (with-exception-handler
            (lambda (err)
              (write `(error ,(->string err)))
              (k #t))
            (lambda ()
              (do-handle-query searcher filterset-store q))))))
    (define (do-handle-query searcher filterset-store q)
      (define resp-value
        (cond
          ((equal? (car q) 'search)
           (let* ((p (cdr q))
                  (start (param p 'start 0))
                  (rows (param p 'rows 40))
                  (query (param p 'query ""))
                  (filterset (param p 'filterset #f))
                  (libs (param p 'libs '()))
                  (libs (transform-request-libraries filterset-store filterset libs))
                  (param-types (param p 'param-types '()))
                  (return-types (param p 'return-types '()))
                  (parameterized-by (param p 'parameterized '()))
                  (tags (param p 'tags '()))
                  (filter-params-loose? (param p 'filter-params-loose? #t))
                  (search-result (query-index searcher start rows query libs param-types return-types parameterized-by tags filter-params-loose?))
                  (search-result (transform-result-libraries filterset-store filterset search-result)))
             (search-result->json search-result)))
          ((equal? (car q) 'tags) (list->vector (facet-values searcher filterset-store 'tag)))
          ((equal? (car q) 'libs) (list->vector (facet-values searcher filterset-store 'lib)))
          ((equal? (car q) 'params) (list->vector (facet-values searcher filterset-store 'param_types)))
          ((equal? (car q) 'returns) (list->vector (facet-values searcher filterset-store 'return_types)))
          ((equal? (car q) 'parameterized) (list->vector (facet-values searcher filterset-store 'parameterized_by)))
          (else (error "Unknown request endpoint"))))
      (write resp-value))

))
