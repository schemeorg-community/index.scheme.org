#|
    This module defines http / rest interaction through spark
|#
(define-library
  (scmindex web-ui)
  (import
    (scheme base)
    (scheme write)
    (scheme read)
    (scheme file)
    (arvyy slf4j)
    (arvyy kawa-spark)
    (arvyy mustache)
    (scmindex domain)
    (scmindex types-parser)
    (scmindex mustache)
    (scmindex settings)
    (srfi 180))
  (export init-web-ui)
  (begin

    (define (init-web-ui config searcher filterset-store)

      (define logger (get-logger "web-ui"))

      (define filtersets (name-list filterset-store))

      ;; load templates from ./templates
      (define (partial-locator name)
        (open-input-file (string-append "templates/" name ".html")))

      ;; compile and cache templates for future invocations
      (define get-template/cached
        (let ((cache '()))
          (lambda (name)
            (cond
              ((assoc name cache) => cdr)
              (else (let ((tpl (compile name partial-locator)))
                      (set! cache (cons (cons name tpl) cache))
                      tpl))))))

      ;; recompile template on each fetch. Useful for development
      (define (get-template/uncached name)
        (compile name partial-locator))

      ;; choose between caching and uncached template fetching functions
      (define get-template
        (if (deploy-setting/cache-templates config)
            get-template/cached
            get-template/uncached))

      ;; utility function for defining endpoint for a mustache rendered page
      ;; handler is expected to return two values -- template name and template data
      (define (get/html path handler)
        (get path (lambda (req resp)
                    (define-values (name data) (handler req resp))
                    (parameterize ((current-lookup data-lookup)
                                   (current-collection list-collection))
                      (execute (get-template name) data)))))

      ;; utility function for defining a RESTful get endpoint
      ;; handler is expected to return sexpr representation of json response (as in srfi 180)
      ;; handles response type header, also handles actual return content -- returns sexpr as with write if
      ;; request param wt = sexpr, otherwise returns json.
      (define (get/rest path handler)
        (get path (lambda (req resp)
                    (define result (handler req resp))
                    (define type-param (req/query-param req "wt"))
                    (define keep-sexpr (equal? "sexpr" type-param))
                    (define payload (open-output-string))
                    (if keep-sexpr
                        (begin
                          (write result payload)
                          (resp/set-type! resp "application/sexpr"))
                        (begin
                          (json-write result payload)
                          (resp/set-type! resp "application/json")))
                    (resp/set-header! resp "Access-Control-Allow-Origin" "*")
                    (get-output-string payload))))

      (define default-page-size (deploy-setting/page-size config))

      (port (deploy-setting/port config))

      (when (deploy-setting/serve-static config)
        (static-files/external-location "static"))

      (not-found (lambda (req resp) (resp/redirect resp "/404.html")))
      ;(internal-server-error (lambda (req resp) (resp/redirect resp "/500.html")))
      (exception (lambda (exception req resp)
                   (log-info logger "Error handling request" exception)
                   (resp/redirect resp "/500.html")))

      (get/html "/"
                (lambda (req resp)
                  (render-home-page req filtersets)))

      (get/html "/settings"
                (lambda (req resp)
                  (render-settings-page req filtersets)))

      (post "/settings"
            (lambda (req resp)
              (for-each
                (lambda (opt)
                  (define value (req/query-param req opt))
                  (if value
                      (resp/set-cookie! resp opt value)
                      (resp/remove-cookie! resp opt)))
                settings-cookies)
              (resp/redirect resp "/settings")))

      (get/html "/filterset/:filterset/search"
                (lambda (req resp)
                  (define filterset (req/param req "filterset"))
                  (define page-size (user-setting/page-size req))
                  (define filter-params-loose?  (user-setting/param-filter-loose req))
                  (define page (let ((value (req/query-param req "page")))
                                 (if value
                                     (string->number value)
                                     1)))
                  (define start (* page-size (- page 1)))
                  (define query (req/query-param req "query"))
                  (define libs (req/query-param-values req "lib"))
                  (define libs* (transform-request-libraries filterset-store filterset libs))
                  (define param-types (or (req/query-param-values req "param") '()))
                  (define return-types (or (req/query-param-values req "return") '()))
                  (define tags (or (req/query-param-values req "tag") '()))
                  (define parameterized-by (or (req/query-param-values req "parameterized") '()))
                  (define search-result (query-index searcher start page-size query libs* param-types return-types parameterized-by tags filter-params-loose?))
                  (define search-result* (transform-result-libraries filterset-store filterset search-result))
                  (render-search-page req filtersets filterset page page-size query libs tags param-types return-types parameterized-by search-result*)))

      ;; REST api
      (path "/rest"

            (get/rest "/filterset"
                      (lambda (req resp)
                        (list->vector (name-list filterset-store))))

            (get/rest "/filterset/:filterset/libs"
                      (lambda (req resp)
                        (define filterset (req/param req "filterset"))
                        (define libs (source-list filterset-store filterset))
                        (list->vector (facet-values searcher libs 'lib))))

            (get/rest "/filterset/:filterset/params"
                      (lambda (req resp)
                        (define filterset (req/param req "filterset"))
                        (define libs (source-list filterset-store filterset))
                        (list->vector (facet-values searcher libs 'param_types))))

            (get/rest "/filterset/:filterset/returns"
                      (lambda (req resp)
                        (define filterset (req/param req "filterset"))
                        (define libs (source-list filterset-store filterset))
                        (list->vector (facet-values searcher libs 'return_types))))

            (get/rest "/filterset/:filterset/tags"
                      (lambda (req resp)
                        (define filterset (req/param req "filterset"))
                        (define libs (source-list filterset-store filterset))
                        (list->vector (facet-values searcher libs 'tags))))

            (get/rest "/filterset/:filterset/parameterized"
                      (lambda (req resp)
                        (define filterset (req/param req "filterset"))
                        (define libs (source-list filterset-store filterset))
                        (list->vector (facet-values searcher libs 'parameterized_by))))

            (get/rest "/filterset/:filterset/search"
                      (lambda (req resp)
                        (define filterset (req/param req "filterset"))
                        (define start (or (req/query-param req "start") 0))
                        (define rows (or (req/query-param req "rows") default-page-size))
                        (define query (req/query-param req "query"))
                        (define libs (req/query-param-values req "lib"))
                        (define libs* (transform-request-libraries filterset-store filterset libs))
                        (define param-types (or (req/query-param-values req "param") '()))
                        (define return-types (or (req/query-param-values req "return") '()))
                        (define tags (or (req/query-param-values req "tag") '()))
                        (define parameterized-by (or (req/query-param-values req "parameterized") '()))
                        (define filter-params-loose? (equal? (or (req/query-param req "filter_loose") "true") "true"))
                        (define search-result (query-index searcher start rows query libs* param-types return-types parameterized-by tags filter-params-loose?))
                        (define search-result* (transform-result-libraries filterset-store filterset search-result))
                        (search-result->json search-result*)))

            ))


))
