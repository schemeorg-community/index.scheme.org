#|
    Main entry point of the software
    Reads configuration, parses and indexes the types, launches web APIs.
|#
(import 
  (scheme base)
  (scheme read)
  (arvyy solr-embedded)
  (arvyy solrj)
  (arvyy kawa-spark)
  (scmindex domain)
  (scmindex types-parser)
  (scmindex filterset-parser)
  (scmindex filterset-store)
  (scmindex solr)
  (scmindex web-ui))

(define settings
  (let* ((config-file (cond
                       ((= 0 (vector-length command-line-arguments)) "./config/configuration.scm")
                       (else (vector-ref command-line-arguments 0))))
         (config (with-input-from-file config-file read))
         (get-property (lambda (prop default-value)
                         (cond
                           ((assoc prop config) => cdr)
                           (else default-value))))
         (get-cookie-property (if (get-property 'enable-user-settings #t)
                                  (lambda (prop default-value)
                                    (define req (current-request))
                                    (cond
                                      ((not req) default-value)
                                      ((assoc prop (req/cookies req)) => cdr)
                                      (else default-value)))
                                  (lambda (prop default-value)
                                    default-value))))
    (make-settings
      ((deploy-setting/port) (get-property 'port 8080))
      ((deploy-setting/spec-index) (get-property 'spec-index "types/index.scm"))
      ((deploy-setting/solr-embed) (get-property 'solr-embed #t))
      ((deploy-setting/solr-home) (get-property 'solr-home "./solrhome"))
      ((deploy-setting/solr-url) (get-property 'solr-url "http://localhost:8983/solr"))
      ((deploy-setting/solr-core) (get-property 'solr-core "scmindex"))
      ((deploy-setting/cache-templates) (get-property 'cache-templates #t))
      ((deploy-setting/page-size) (get-property 'page-size 40))
      ((deploy-setting/serve-static) (get-property 'serve-static #t))
      ((deploy-setting/filterset-index) (get-property 'filterset-index "filters/index.scm"))
      ((deploy-setting/sqlite-location) (get-property 'sqlite-data "sqlitedb"))
      ((deploy-setting/enable-user-settings) (get-property 'enable-user-settings #t))
      ((user-setting/page-size)
       (string->number (get-cookie-property 'pageSize "40")))
      ((user-setting/param-filter-loose)
       (equal? "yes" (get-cookie-property 'filterParamsLoose "yes")))
      ((user-setting/ctrl-f-override)
       (equal? "yes" (get-cookie-property 'overrideCtrlF "yes"))))))

(define solr-core (deploy-setting/solr-core settings))
(define solr-client
  (cond
    ((deploy-setting/solr-embed settings) (create-embedded-solr-client (deploy-setting/solr-home settings) solr-core))
    (else (create-http-solr-client (deploy-setting/solr-url settings)))))

(define solr-searcher (make-solr-searcher solr-client solr-core))

(define sqlite-location (deploy-setting/sqlite-location settings))
(define sqlite-filterset-store (make-sqlite-filterset-store sqlite-location))

(let ((specs (read-specs (deploy-setting/spec-index settings))))
  (save-index-entries solr-searcher specs))

(let ((filters (read-filters (deploy-setting/filterset-index settings))))
  (save-filterset-entries sqlite-filterset-store filters))

(init-web-ui settings solr-searcher sqlite-filterset-store)
