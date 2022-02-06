(define-library
  (kawa-spark-test main-test)
  (import (scheme base)
          (kawa-spark-test mapping-test)
          (kawa-spark-test request-test)
          (kawa-spark-test response-test)
          (kawa-spark-test session-test)
          (srfi 64))
  (export do-kawa-spark-test)
  (begin
    (define (do-kawa-spark-test)
      (test-begin "Kawa-Spark test")

      (test-group 
        "Mapping test"
        (do-mapping-test))

      (test-group
        "Request methods test"
        (do-request-test))

      (test-group
        "Response methods test"
        (do-response-test))

      (test-group
        "Session methods test"
        (do-session-test))
      (test-end))))
