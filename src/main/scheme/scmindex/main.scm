(import 
  (scheme base)
  (scheme write)
  (scheme read)
  (arvyy httpclient)
  (arvyy kawa-spark)
  (arvyy mustache)
  (scmindex types-parser)
  (scmindex mustache)
  (scmindex solr)
  (srfi 180))

(define config
  (with-input-from-file "./config/configuration.scm" read))

(define (get-property prop)
  (cond
    ((assoc prop config) => cdr)
    (else (error (string-append "Required property missing " (symbol->string prop))))))

(define (partial-locator name)
  (open-input-file (string-append "templates/" name ".html")))

(define (make-tpl-getter name)
  (if (get-property 'cache-templates)
      (let ((tpl (compile name partial-locator)))
       (lambda () tpl))
      (lambda () (compile name partial-locator))))

(define get-index-tpl (make-tpl-getter "index"))
(define get-search-tpl (make-tpl-getter "search"))
(define get-settings-tpl (make-tpl-getter "settings"))
(define get-userguide-tpl (make-tpl-getter "userguide"))
(define get-restapi-tpl (make-tpl-getter "restapi"))

(define solr-url (string-append (get-property 'solr-url) "/solr/" (get-property 'solr-core)))
(define solr-search-url (string-append solr-url "/search"))
(define solr-suggest-url (string-append solr-url "/suggest"))
(define default-page-size (get-property 'page-size))

(define (->string obj)
  (define port (open-output-string))
  (write obj port)
  (get-output-string port))

(define (make-head-data req)
  (define light-theme?
    (cond
      ((assoc 'theme (req/cookies req)) => (lambda (e)
                                             (equal? "light" (cdr e))))
      (else #t)))
  (define override-ctrlf?
    (cond
      ((assoc 'overrideCtrlF (req/cookies req)) => (lambda (e)
                                                     (equal? "yes" (cdr e))))
      (else #f)))
  `((light-theme . ,light-theme?)
    (ctrlf-override . ,override-ctrlf?)))

(let ((funcs (read-specs "types/index.scm")))
 (index-types solr-url funcs))

(when (get-property 'serve-static)
  (static-files/external-location "static"))

(get "/"
     (lambda (req resp)
       (execute (get-index-tpl) 
                `((page-title . "Home") 
                  ,@(make-mustache-nav-data 'index)
                  ,@(make-head-data req)))))

(get "/settings"
     (lambda (req resp)
       (execute (get-settings-tpl) 
                `((page-title . "Settings") 
                  ,@(make-mustache-nav-data 'settings)
                  ,@(make-head-data req)
                  ,@(make-mustache-settings-data (req/cookies req))))))

(post "/settings"
      (lambda (req resp)
        (define options '("overrideCtrlF" "queryParser" "theme" "pageSize" "filterParamsLoose"))
        (for-each
          (lambda (opt)
            (define value (req/query-param req opt))
            (if value
                (resp/set-cookie! resp opt value)
                (resp/remove-cookie! resp opt))) 
          options)
        (resp/redirect resp "/settings")))

(get "/search"
     (lambda (req resp)
       (define page-size
         (cond
           ((assoc 'pageSize (req/cookies req)) => (lambda (e)
                                                     (string->number (cdr e))))
           (else default-page-size)))
       (define filter-params-loose?
         (cond
           ((assoc 'filterParamsLoose (req/cookies req)) => (lambda (e)
                                                              (equal? "yes" (cdr e))))
           (else #t)))
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
       (define data (exec-solr-query solr-search-url start page-size query libs param-types return-types tags filter-params-loose?))
       (define search-data
         (make-mustache-search-data 
           page
           page-size
           query
           libs
           param-types
           return-types
           tags
           data))
       (execute (get-search-tpl) `((page-title . "Search") 
                                   ,@search-data
                                   ,@(make-mustache-nav-data 'search)
                                   ,@(make-head-data req)))))

(get "/userguide"
     (lambda (req resp)
       (execute (get-userguide-tpl) 
                `((page-title . "User guide") 
                  ,@(make-mustache-nav-data 'userguide)
                  ,@(make-head-data req)))))

(get "/restapi"
     (lambda (req resp)
       (execute (get-restapi-tpl) 
                `((page-title . "REST api") 
                  ,@(make-mustache-nav-data 'restapi)
                  ,@(make-head-data req)))))

(get "/suggest"
     (lambda (req resp)
       (define text (req/query-param req "text"))
       (define suggest-json (solr-get-suggestions solr-suggest-url text))
       (define payload (open-output-string))
       (json-write suggest-json payload)
       (resp/set-type! resp "application/json")
       (resp/set-header! resp "Access-Control-Allow-Origin" "*")
       (get-output-string payload)))

(path "/rest"
      (get "/libs"
           (lambda (req resp)
             (define libs (solr-facet-values solr-search-url 'lib))
             (define payload (open-output-string))
             (json-write libs payload)
             (resp/set-type! resp "application/json")
             (resp/set-header! resp "Access-Control-Allow-Origin" "*")
             (get-output-string payload)))
      
      (get "/params"
           (lambda (req resp)
             (define libs (solr-facet-values solr-search-url 'param_types))
             (define payload (open-output-string))
             (json-write libs payload)
             (resp/set-type! resp "application/json")
             (resp/set-header! resp "Access-Control-Allow-Origin" "*")
             (get-output-string payload)))
      
      (get "/returns"
           (lambda (req resp)
             (define libs (solr-facet-values solr-search-url 'return_types))
             (define payload (open-output-string))
             (json-write libs payload)
             (resp/set-type! resp "application/json")
             (resp/set-header! resp "Access-Control-Allow-Origin" "*")
             (get-output-string payload)))
      
      (get "/tags"
           (lambda (req resp)
             (define libs (solr-facet-values solr-search-url 'tags))
             (define payload (open-output-string))
             (json-write libs payload)
             (resp/set-type! resp "application/json")
             (resp/set-header! resp "Access-Control-Allow-Origin" "*")
             (get-output-string payload)))
      
      (get "/procedures"
           (lambda (req resp)
             (define start (or (req/query-param req "start") 0))
             (define rows (or (req/query-param req "rows") default-page-size))
             (define query (req/query-param req "query"))
             (define libs (req/query-param-values req "lib"))
             (define param-types (or (req/query-param-values req "param") '()))
             (define return-types (or (req/query-param-values req "return") '()))
             (define tags (or (req/query-param-values req "tag") '()))
             (define filter-params-loose? (or (req/query-param req "filter_loose") #t))
             (define data (exec-solr-query solr-search-url start rows query libs param-types return-types tags filter-params-loose?))
             (define payload (open-output-string))
             (json-write data payload)
             (resp/set-type! resp "application/json")
             (resp/set-header! resp "Access-Control-Allow-Origin" "*")
             (get-output-string payload))))
