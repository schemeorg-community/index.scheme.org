(define-library
  (kawa-spark-test request-test)
  (import 
    (scheme base)
    (scheme write)
    (srfi 64)
    (srfi 95) ;sort
    (arvyy kawa-spark)
    (kawa-spark-test test-util)
    (class org.apache.http.client.fluent Request Response Content Form)
    (class org.apache.http.entity ContentType))
  
  (export do-request-test)
  (begin
    (define (do-request-test)
      (port 8080)
      (init)
      (await-initialization)

      (before "/attributes1"
              (lambda (req resp)
                (req/set-attribute! req "foo" "bar")))
      (get "/attributes1"
           (lambda (req resp)
             (req/attribute req "foo")))

      (before "/attributes2"
              (lambda (req resp)
                (req/set-attribute! req "foo" "bar")))
      (get "/attributes2"
           (lambda (req resp)
             (car (req/attributes req))))

      (post "/body1"
            (lambda (req resp)
              (req/body req)))

      (post "/body2"
            (lambda (req resp)
              (define port (open-output-string))
              (write (req/body-as-bytes req) port)
              (get-output-string port)))
      
      (post "/body3"
            (lambda (req resp)
              (define port (open-output-string))
              (write (req/body req) port)
              (get-output-string port)))
      
      (post "/content-length"
            (lambda (req resp)
              (number->string (req/content-length req))))

      (post "/content-type"
            (lambda (req resp)
              (req/content-type req)))

      (get "/cookies"
            (lambda (req resp)
              (define port (open-output-string))
              (write (req/cookies req) port)
              (get-output-string port )))

      (get "/cookie"
            (lambda (req resp)
              (req/cookie req "foo")))

      (get "/host"
           (lambda (req resp)
             (req/host req)))

      (get "/ip"
           (lambda (req resp)
             (req/ip req)))

      (get "/param1/:var"
           (lambda (req resp)
             (resp/set-type! resp "text/plain; charset=utf-8")
             (req/param req "var")))

      (get "/param2/:var"
           (lambda (req resp)
             (define params (req/params req))
             (resp/set-type! resp "text/plain; charset=utf-8")
             (cdr (assoc ':var params))))
      
      (get "/param3/:var"
           (lambda (req resp)
             (define port (open-output-string))
             (write (req/param req "foo") port)
             (get-output-string port)))

      (get "/path-info/:foo"
           (lambda (req resp)
             (req/path-info req)))

      (get "/port"
           (lambda (req resp)
             (req/port req)))

      (get "/protocol"
           (lambda (req resp)
             (req/protocol req)))

      (get "/query-string"
           (lambda (req resp)
             (req/query-string req)))

      (get "/query-params"
           (lambda (req resp)
             (define port (open-output-string))
             (write (req/query-params req) port)
             (get-output-string port)))

      (get "/query-param"
           (lambda (req resp)
             (req/query-param req "foo")))
      
      (get "/query-param2"
           (lambda (req resp)
             (define port (open-output-string))
             (write (req/query-param req "foo") port)
             (get-output-string port)))

      (get "/query-param-values"
           (lambda (req resp)
             (define port (open-output-string))
             (write (sort (req/query-param-values req "foo") string<?) port)
             (get-output-string port)))

      (get "/request-method"
           (lambda (req resp)
             (req/request-method req)))

      (get "/scheme"
           (lambda (req resp)
             (req/scheme req)))

      (get "/splat/*"
           (lambda (req resp)
             (define port (open-output-string))
             (write (req/splat req) port)
             (get-output-string port)))

      (get "/uri"
           (lambda (req resp)
             (req/uri req)))

      (get "/url"
           (lambda (req resp)
             (req/url req)))

      (get "/user-agent"
           (lambda (req resp)
             (req/user-agent req)))
      
      (test-req (Request:Get "http://localhost:8080/attributes1") "bar")
      (test-req (Request:Get "http://localhost:8080/attributes2") "foo")
      (let* ((post (Request:Post "http://localhost:8080/body1"))
             (post (post:bodyString "a-body-content" ContentType:DEFAULT_TEXT)))
        (test-req post "a-body-content"))
      (let* ((post (Request:Post "http://localhost:8080/body2"))
             (post (post:bodyString "a" ContentType:DEFAULT_TEXT)))
        (test-req post "#(97)"))
      (let* ((post (Request:Post "http://localhost:8080/body3")))
        (test-req post "\"\""))
      (let* ((post (Request:Post "http://localhost:8080/content-length"))
             (post (post:bodyString "a" ContentType:DEFAULT_TEXT)))
        (test-req post "1"))
      (let* ((post (Request:Post "http://localhost:8080/content-type"))
             (post (post:bodyString "a" (ContentType:create "text/plain" "UTF-8"))))
        (test-req post "text/plain; charset=UTF-8"))
      (let ()
       (cookie-store:clear)
       (set-cookie "foo" "foo-cookie")
       (test-req (Request:Get "http://localhost:8080/cookies") "((foo . \"foo-cookie\"))"))
      (let ()
       (cookie-store:clear)
       (set-cookie "foo" "foo-cookie")
       (test-req (Request:Get "http://localhost:8080/cookie") "foo-cookie"))
      (test-req (Request:Get "http://localhost:8080/host") "localhost:8080")
      (test-req (Request:Get "http://localhost:8080/ip") "127.0.0.1")
      (test-req (Request:Get "http://localhost:8080/param1/foo") "foo")
      (test-req (Request:Get "http://localhost:8080/param2/foo") "foo")
      (test-req (Request:Get "http://localhost:8080/param3/foo") "#f")
      (test-req (Request:Get "http://localhost:8080/path-info/foo") "/path-info/foo")
      (test-req (Request:Get "http://localhost:8080/port") "8080")
      (test-req (Request:Get "http://localhost:8080/protocol") "HTTP/1.1")
      (test-req (Request:Get "http://localhost:8080/query-string?foo=bar") "foo=bar")
      (test-req (Request:Get "http://localhost:8080/query-params?foo=a&bar=b") "(\"bar\" \"foo\")")
      (test-req (Request:Get "http://localhost:8080/query-param?foo=a&foo=b") "a")
      (test-req (Request:Get "http://localhost:8080/query-param2?bar=a") "#f")
      (test-req (Request:Get "http://localhost:8080/query-param-values?foo=a&foo=b") "(\"a\" \"b\")")
      (test-req (Request:Get "http://localhost:8080/request-method") "GET")
      (test-req (Request:Get "http://localhost:8080/scheme") "http")
      (test-req (Request:Get "http://localhost:8080/splat/foo") "(\"foo\")")
      (test-req (Request:Get "http://localhost:8080/uri") "/uri")
      (test-req (Request:Get "http://localhost:8080/url") "http://localhost:8080/url")
      (test-req (Request:Get "http://localhost:8080/user-agent") 
                (lambda (str)
                  (define start "Apache-HttpClient")
                  (test-equal start (substring str 0 (string-length start)))))
      (stop)
      (await-stop))))
