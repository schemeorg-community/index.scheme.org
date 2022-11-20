(define-library
  (test-json)
  (import (scheme base)
          (scheme write)
          (scheme read)
          (scmindex domain)
          (scmindex types-parser)
          (srfi 64)
          (srfi 180)
          
          (class com.networknt.schema JsonSchemaFactory)
          (class com.fasterxml.jackson.databind ObjectMapper)
          (class java.io File))
  
  (export run-json-tests)
  
  (begin
    (define (run-json-tests)
      (test-group
        "Read all and build search result and validate against schema"
        (define specs (read-specs "types/index.scm"))
        (define json-entries (list->vector (map index-entry->json specs)))
        (define json-scm
          `((items . ,json-entries)
            (total . 1)
            (libs . #())
            (params . #())
            (returns . #())
            (tags . #())
            (parameterized . #())))
        (define json-str
          (let ((port (open-output-string)))
            (json-write json-scm port)
            (get-output-string port)))
        (define om (ObjectMapper))
        (define schema-node (om:readTree (File "searchresult.schema.json")))
        (define content-node (om:readTree json-str))
        (define factory (JsonSchemaFactory:getInstance))
        (define schema (factory:getSchema schema-node))
        (define messages (schema:validate content-node))
        (define message "")
        (messages:forEach (lambda (m)
                            (set! message (string-append message (m:toString)))))
        (test-equal message "")))))
