(define-library
  (kawa-spark-test test-util)
  (import 
    (scheme base)
    (srfi 64)
    (class org.apache.http.client.fluent Request Response Content Form Executor)
    (class org.apache.http.impl.client BasicCookieStore)
    (class org.apache.http.impl.cookie BasicClientCookie)
    (class org.apache.http.cookie ClientCookie)
    (class org.apache.http.entity ContentType))
  (export test-req
          cookie-store
          set-cookie)
  (begin
    
    (define cookie-store (BasicCookieStore))
    (define exec (Executor:newInstance))
    
    (define (set-cookie name value)
      (define cookie (BasicClientCookie "foo" "foo-cookie"))
      (cookie:setAttribute ClientCookie:DOMAIN_ATTR "localhost")
      (cookie:setAttribute ClientCookie:VERSION_ATTR "1")
      (cookie:setPath "/")
      (cookie:setDomain "localhost")
      (cookie:setVersion 1)
      (cookie:setSecure #f)
      (cookie-store:addCookie cookie))
    
    (define (test-req req ::Request expected-result)
      (let* ((exec (exec:use cookie-store))
             (resp (exec:execute req))
             (content (resp:returnContent))
             (response-string (content:asString)))
        (cond
          ((string? expected-result)
           (test-equal expected-result response-string))
          ((procedure? expected-result)
           (expected-result response-string))
          (else (error "Bad expected-result type")))))))
