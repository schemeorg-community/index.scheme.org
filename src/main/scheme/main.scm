#|
    Main entry point of the software
    Reads configuration, parses and indexes the types, launches web and/or repl APIs.
|#
(import 
  (scheme base)
  (scheme read)
  (arvyy solr-embedded)
  (arvyy solrj)
  (scmindex domain)
  (scmindex types-parser)
  (scmindex filterset-parser)
  (scmindex filterset-store)
  (scmindex solr)
  (scmindex settings)
  (scmindex web-ui)
  (scmindex repl-ui))

(define config-file
  (cond
    ((= 0 (vector-length command-line-arguments)) "./config/configuration.scm")
    (else (vector-ref command-line-arguments 0))))

(define config (with-input-from-file config-file read))

(define solr-core (deploy-setting/solr-core config))
(define solr-client
  (cond
    ((deploy-setting/solr-embed config) (create-embedded-solr-client (deploy-setting/solr-home config) solr-core))
    (else (create-http-solr-client (deploy-setting/solr-url config)))))

(define solr-searcher (make-solr-searcher solr-client solr-core))

(define sqlite-location (deploy-setting/sqlite-location config))
(define sqlite-filterset-store (make-sqlite-filterset-store sqlite-location))

(let ((specs (read-specs (deploy-setting/spec-index config))))
  (save-index-entries solr-searcher specs))

(let ((filters (read-filters (deploy-setting/filterset-index config))))
  (save-filterset-entries sqlite-filterset-store filters))

(when (deploy-setting/enable-web config)
  (init-web-ui config solr-searcher sqlite-filterset-store))

(when (deploy-setting/enable-repl config)
  (init-repl-ui config solr-searcher sqlite-filterset-store))
