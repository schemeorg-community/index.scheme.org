(define-library
  (kawa-spark-test response-test)
  (export do-response-test)
  (import 
    (scheme base)
    (scheme write)
    (srfi 64)
    (srfi 95) ;sort
    (arvyy kawa-spark)
    (kawa-spark-test test-util)
    (class org.apache.http.client.fluent Request Response Content Form)
    (class org.apache.http.cookie Cookie)
    (class org.apache.http.entity ContentType))
  (begin
    (define (do-response-test)
      (port 8080)
      (init)
      (await-initialization)
     
      (before "/body"
              (lambda (req resp)
                (resp/set-body! resp "foo")))
      (get "/body"
           (lambda (req resp)
             (resp/body resp)))
      
      (get "/header"
           (lambda (req resp)
             (resp/set-header! resp "foo" "bar")
             "ok"))
      
      (get "/redirect"
           (lambda (req resp)
             (resp/redirect resp "/redirect-target")))
      
      (get "/redirect-target"
           (lambda (req resp)
             "foo"))
      
      (get "/status"
           (lambda (req resp)
             (resp/set-status! resp 203)
             (resp/status resp)))
      
      (get "/type"
           (lambda (req resp)
             (resp/set-type! resp "application/json")
             (string-append "\"" (resp/type resp) "\"")))
      
      (get "/set-cookie"
           (lambda (req resp)
             (resp/set-cookie! resp "foo" "foo-value")
             (resp/set-cookie! resp "bar" "bar-value")
             (resp/remove-cookie! resp "bar")
             "ok"))
      
      (test-req (Request:Get "http://localhost:8080/body") "foo")
      (let* ((req (Request:Get "http://localhost:8080/header"))
             (resp (req:execute))
             (http-resp (resp:returnResponse))
             (foo-header (http-resp:getFirstHeader "foo"))
             (foo-header-value (foo-header:getValue)))
        (test-equal foo-header-value "bar"))
      
      (test-req (Request:Get "http://localhost:8080/redirect") "foo")
      
      (let* ((req (Request:Get "http://localhost:8080/status"))
             (resp (req:execute))
             (http-resp (resp:returnResponse)))
        (test-equal ((http-resp:getStatusLine):getStatusCode) 203))
      (test-req (Request:Get "http://localhost:8080/status") "203")
      
      (let* ((req (Request:Get "http://localhost:8080/type"))
             (resp (req:execute))
             (http-resp (resp:returnResponse)))
        (test-equal ((http-resp:getFirstHeader "Content-Type"):getValue) "application/json"))
      (test-req (Request:Get "http://localhost:8080/type") "\"application/json\"")
      
      (let* ((req (Request:Get "http://localhost:8080/set-cookie"))
             (resp (req:execute))
             (http-resp (resp:returnResponse))
             (set-cookie-header (http-resp:getFirstHeader "Set-Cookie"))
             (set-cookie-header-value (set-cookie-header:getValue)))
        (test-equal set-cookie-header-value "foo=foo-value"))
      
      
      (stop)
      (await-stop))))
