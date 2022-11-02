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
    (only (srfi 1) filter)
    (srfi 180))
  (export init-web-ui
          current-request)
  (begin

    (define current-request (make-parameter #f))
    (define logger (get-logger "web-ui"))

    (define (init-web-ui settings searcher filterset-store)

      (define filtersets (let ((lst (code-list filterset-store)))
                           (map
                             (lambda (code)
                               (cons code (get-name filterset-store code)))
                             lst)))

      (define downloads-config 
        (let* ((file (deploy-setting/downloads settings))
               (exists? (file-exists? file))
               (content (if exists?
                            (call-with-input-file file read)
                            #f)))
          content))

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
        (if (deploy-setting/cache-templates settings)
            get-template/cached
            get-template/uncached))

      ;; utility function for defining endpoint for a mustache rendered page
      ;; handler is expected to return two values -- template name and template data
      (define (get/html path handler)
        (get path (lambda (req resp)
                    (parameterize ((current-request req))
                      (define-values (name data) (handler req resp))
                      (parameterize ((current-lookup data-lookup)
                                     (current-collection list-collection))
                        (execute (get-template name) data)) ))))

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

      (define default-page-size (deploy-setting/page-size settings))

      (port (deploy-setting/port settings))

      (when (deploy-setting/serve-static settings)
        (static-files/external-location "static"))

      (not-found (lambda (req resp) (resp/redirect resp "/404.html")))
      ;(internal-server-error (lambda (req resp) (resp/redirect resp "/500.html")))
      (exception (lambda (exception req resp)
                   (log-info logger "Error handling request" exception)
                   (resp/redirect resp "/500.html")))

      (get/html "/"
                (lambda (req resp)
                  (render-home-page settings downloads-config filtersets)))

      (get/html "/settings"
                (lambda (req resp)
                  (if (deploy-setting/enable-user-settings settings)
                      (render-settings-page req settings filtersets)
                      (resp/redirect resp "/404.html"))))

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
                  (define filterset-code (req/param req "filterset"))
                  (define filterset-name (get-name filterset-store filterset-code))
                  (if (not filterset-name)
                      (resp/redirect resp "/404.html")
                      (let ()
                        (define page-size (user-setting/page-size settings))
                        (define filter-params-loose?  (user-setting/param-filter-loose settings))
                        (define page (let ((value (req/query-param req "page")))
                                       (if value
                                           (string->number value)
                                           1)))
                        (define start (* page-size (- page 1)))
                        (define query (req/query-param req "query"))
                        (define libs (req/query-param-values req "lib"))
                        (define libs* (transform-request-libraries filterset-store filterset-code libs))
                        (define param-types (or (req/query-param-values req "param") '()))
                        (define return-types (or (req/query-param-values req "return") '()))
                        (define tags (or (req/query-param-values req "tag") '()))
                        (define parameterized-by (or (req/query-param-values req "parameterized") '()))
                        (define search-result (query-index searcher start page-size query libs* param-types return-types parameterized-by tags filter-params-loose?))
                        (define search-result* (transform-result-libraries filterset-store filterset-code search-result))
                        (render-search-page settings filtersets filterset-name page page-size query libs tags param-types return-types parameterized-by search-result*)))))

      ;; REST api
      (path "/rest"

            (get/rest "/filterset"
                      (lambda (req resp)
                        (list->vector (code-list filterset-store))))

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
                        (define include-facets (string=? "true" (or (req/query-param req "facet") "true")))
                        (define json (search-result->json search-result*))
                        (if include-facets
                            json
                            (filter
                              (lambda (e)
                                (member (car e)
                                        '(items total))) 
                              json))))

            ))


))
