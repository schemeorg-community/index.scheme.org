(test-group
  "Read, convert to json and render all real types"
  (define specs (read-specs "types/index.scm"))
  (define json-scm (list->vector (map func->json specs)))
  (define json-str
    (let ((port (open-output-string)))
      (json-write json-scm port)
      (get-output-string port)))
  (test-assert (> (length specs) 1000))
  (test-assert (> (vector-length json-scm) 1000))
  (test-assert (> (string-length json-str) 10000))
  (vector-for-each
    (lambda (doc)
      (test-assert (not (null? (make-doc-data doc)))))
    specs))

