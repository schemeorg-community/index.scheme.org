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
    (arvyy solr-embedded)
    (arvyy solrj)
    (scmindex domain)
    (scmindex types-parser)
    (scmindex mustache)
    (scmindex solr)
    (scmindex settings)
    (srfi 180))
  (export init-web-ui)
  (begin

    (define (init-web-ui config solr-client solr-core)

      (define logger (get-logger "web-ui"))

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

      (define solr-url (string-append (deploy-setting/solr-url config) "/solr/" (deploy-setting/solr-core config)))
      (define solr-search-url (string-append solr-url "/search"))
      (define solr-suggest-url (string-append solr-url "/suggest"))
      (define default-page-size (deploy-setting/page-size config))

      (port (deploy-setting/port config))

      (when (deploy-setting/serve-static config)
        (static-files/external-location "static"))

      (not-found (lambda (req resp) (resp/redirect resp "/404.html")))
      ;(internal-server-error (lambda (req resp) (resp/redirect resp "/500.html")))
      (exception (lambda (exception req resp)
                   (log-info logger "TEST\nTEST\n")
                   (log-info logger "TEST\nTEST\n")
                   (log-info logger "TEST\nTEST\n")
                   (log-info logger "TEST\nTEST\n")
                   (log-info logger "TEST\nTEST\n")
                   (log-info logger "Error handling request" exception)
                   (resp/redirect resp "/500.html")))

      (get/html "/"
                (lambda (req resp)
                  (render-home-page req)))

      (get/html "/settings"
                (lambda (req resp)
                  (render-settings-page req)))

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

      (get/html "/search"
                (lambda (req resp)
                  (define page-size (user-setting/page-size req))
                  (define filter-params-loose?  (user-setting/param-filter-loose req))
                  (define page (let ((value (req/query-param req "page")))
                                 (if value
                                     (string->number value)
                                     1)))
                  (define start (* page-size (- page 1)))
                  (define query (req/query-param req "query"))
                  (define libs (req/query-param-values req "lib"))
                  (define param-types (or (req/query-param-values req "param") '()))
                  (define return-types (or (req/query-param-values req "return") '()))
                  (define tags (or (req/query-param-values req "tag") '()))
                  (define parameterized-by (or (req/query-param-values req "parameterized") '()))
                  (define data (exec-solr-query solr-client solr-core start page-size query libs param-types return-types parameterized-by tags filter-params-loose?))
                  (render-search-page req page page-size query libs tags param-types return-types parameterized-by data)))

      ;; type ahead, triggered while writing in search textfield
      (get/rest "/suggest"
                (lambda (req resp)
                  (define text (req/query-param req "text"))
                  (solr-get-suggestions solr-client solr-core text)))

      ;; REST api
      (path "/rest"
            (get/rest "/libs"
                      (lambda (req resp)
                        (list->vector (solr-facet-values solr-client solr-core 'lib))))

            (get/rest "/params"
                      (lambda (req resp)
                        (list->vector (solr-facet-values solr-client solr-core 'param_types))))

            (get/rest "/returns"
                      (lambda (req resp)
                        (list->vector (solr-facet-values solr-client solr-core 'return_types))))

            (get/rest "/tags"
                      (lambda (req resp)
                        (list->vector (solr-facet-values solr-client solr-core 'tags))))

            (get/rest "/parameterized"
                      (lambda (req resp)
                        (list->vector (solr-facet-values solr-client solr-core 'parameterized_by))))

            (get/rest "/search"
                      (lambda (req resp)
                        (define start (or (req/query-param req "start") 0))
                        (define rows (or (req/query-param req "rows") default-page-size))
                        (define query (req/query-param req "query"))
                        (define libs (req/query-param-values req "lib"))
                        (define param-types (or (req/query-param-values req "param") '()))
                        (define return-types (or (req/query-param-values req "return") '()))
                        (define tags (or (req/query-param-values req "tag") '()))
                        (define parameterized-by (or (req/query-param-values req "parameterized") '()))
                        (define filter-params-loose? (equal? (or (req/query-param req "filter_loose") "true") "true"))
                        (define search-result (exec-solr-query solr-client solr-core start rows query libs param-types return-types parameterized-by tags filter-params-loose?))
                        (search-result->json search-result)))))


))
