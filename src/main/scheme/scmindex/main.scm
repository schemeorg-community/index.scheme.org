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

(define index-tpl (compile "search" partial-locator))
(define get-index-tpl
  (if (get-property 'cache-templates)
      (lambda () index-tpl)
      (lambda () (compile "search" partial-locator))))

(define solr-url (string-append (get-property 'solr-url) "/solr/" (get-property 'solr-core)))
(define solr-search-url (string-append solr-url "/search"))
(define page-size (get-property 'page-size))

(define (->string obj)
  (define port (open-output-string))
  (write obj port)
  (get-output-string port))

(let ((funcs (read-specs "types/index.scm")))
 (index-types solr-url funcs))

(when (get-property 'serve-static)
  (static-files/external-location "static"))

(get "/search"
     (lambda (req resp)
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
       (define data (exec-solr-query solr-search-url start page-size query libs param-types return-types tags))
       (define mustache-data
         (make-mustache-data 
           page
           page-size
           query
           libs
           param-types
           return-types
           tags
           data))
       (execute (get-index-tpl) mustache-data)))

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
             (define rows (or (req/query-param req "rows") page-size))
             (define query (req/query-param req "query"))
             (define libs (req/query-param-values req "lib"))
             (define param-types (or (req/query-param-values req "param") '()))
             (define return-types (or (req/query-param-values req "return") '()))
             (define tags (or (req/query-param-values req "tag") '()))
             (define data (exec-solr-query solr-search-url start rows query libs param-types return-types tags))
             (define payload (open-output-string))
             (json-write data payload)
             (resp/set-type! resp "application/json")
             (resp/set-header! resp "Access-Control-Allow-Origin" "*")
             (get-output-string payload))))
