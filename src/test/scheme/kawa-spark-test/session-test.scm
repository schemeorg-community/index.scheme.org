(define-library
  (kawa-spark-test session-test)
  (import 
    (scheme base)
    (scheme write)
    (srfi 64)
    (srfi 95) ;sort
    (arvyy kawa-spark)
    (kawa-spark-test test-util)
    (class org.apache.http.client.fluent Request Response Content Form)
    (class org.apache.http.entity ContentType))
  (export do-session-test)
  (begin
    (define (do-session-test)
      #t
      )))
