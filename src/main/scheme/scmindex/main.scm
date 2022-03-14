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
  (scmindex settings)
  (srfi 180))

(define config
  (with-input-from-file "./config/configuration.scm" read))

(define (partial-locator name)
  (open-input-file (string-append "templates/" name ".html")))

(define get-template/cached
  (let ((cache '()))
    (lambda (name)
      (cond
        ((assoc name cache) => cdr)
        (else (let ((tpl (compile name partial-locator)))
                (set! cache (cons (cons name tpl) cache))
                tpl))))))

(define (get-template/uncached name)
  (compile name partial-locator))

(define get-template
  (if (deploy-setting/cache-templates config)
    get-template/cached
    get-template/uncached))

(define (get/html path handler)
  (get path (lambda (req resp)
              (define-values (name data) (handler req resp))
              (execute (get-template name) data))))

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

(define (make-tpl-getter name)
  (if (deploy-setting/cache-templates config)
      (let ((tpl (compile name partial-locator)))
       (lambda () tpl))
      (lambda () (compile name partial-locator))))

(define solr-url (string-append (deploy-setting/solr-url config) "/solr/" (deploy-setting/solr-core config)))
(define solr-search-url (string-append solr-url "/search"))
(define solr-suggest-url (string-append solr-url "/suggest"))
(define default-page-size (deploy-setting/page-size config))

(define (make-head-data req)
  `((light-theme . ,(user-setting/light-theme? req))
    (ctrlf-override . ,(user-setting/ctrl-f-override req))))

(let ((funcs (read-specs "types/index.scm")))
 (index-types solr-url funcs))

(when (deploy-setting/serve-static config)
  (static-files/external-location "static"))

(get/html "/"
          (lambda (req resp)
            (values "index"
                    `((page-title . "Home") 
                       ,@(make-mustache-nav-data 'index)
                       ,@(make-head-data req)))))

(get/html "/settings"
     (lambda (req resp)
       (values "settings" 
               `((page-title . "Settings") 
                 ,@(make-mustache-nav-data 'settings)
                 ,@(make-head-data req)
                 ,@(mustache-settings-data req)))))

(post "/settings"
      (lambda (req resp)
        (for-each
          (lambda (opt)
            (define value (req/query-param req opt))
            (if value
                (resp/set-cookie! resp opt value)
                (resp/remove-cookie! resp opt))) 
          settings-options)
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
       (values "search" 
               `((page-title . "Search") 
                 ,@search-data
                 ,@(make-mustache-nav-data 'search)
                 ,@(make-head-data req)))))

(get/html "/userguide"
          (lambda (req resp)
            (values "userguide" 
                    `((page-title . "User guide") 
                      ,@(make-mustache-nav-data 'userguide)
                      ,@(make-head-data req)))))

(get/html "/restapi"
     (lambda (req resp)
       (values "restapi" 
               `((page-title . "REST api") 
                 ,@(make-mustache-nav-data 'restapi)
                 ,@(make-head-data req)))))

(get/rest "/suggest"
     (lambda (req resp)
       (define text (req/query-param req "text"))
       (solr-get-suggestions solr-suggest-url text)))

(path "/rest"
      (get/rest "/libs"
           (lambda (req resp)
             (solr-facet-values solr-search-url 'lib)))
      
      (get/rest "/params"
           (lambda (req resp)
             (solr-facet-values solr-search-url 'param_types)))
      
      (get/rest "/returns"
           (lambda (req resp)
             (solr-facet-values solr-search-url 'return_types)))
      
      (get/rest "/tags"
           (lambda (req resp)
             (solr-facet-values solr-search-url 'tags)))
      
      (get/rest "/procedures"
           (lambda (req resp)
             (define start (or (req/query-param req "start") 0))
             (define rows (or (req/query-param req "rows") default-page-size))
             (define query (req/query-param req "query"))
             (define libs (req/query-param-values req "lib"))
             (define param-types (or (req/query-param-values req "param") '()))
             (define return-types (or (req/query-param-values req "return") '()))
             (define tags (or (req/query-param-values req "tag") '()))
             (define filter-params-loose? (or (req/query-param req "filter_loose") #t))
             (exec-solr-query solr-search-url start rows query libs param-types return-types tags filter-params-loose?))))
