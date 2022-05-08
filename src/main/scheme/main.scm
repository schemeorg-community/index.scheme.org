(import 
  (scheme base)
  (scheme read)
  (arvyy solr-embedded)
  (arvyy solrj)
  (scmindex types-parser)
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

(let ((funcs (read-specs (deploy-setting/spec-index config))))
 (index-types solr-client solr-core funcs))

(when (deploy-setting/enable-web config)
    (init-web-ui config solr-client solr-core))

(when (deploy-setting/enable-repl config)
    (init-repl-ui config solr-client solr-core))