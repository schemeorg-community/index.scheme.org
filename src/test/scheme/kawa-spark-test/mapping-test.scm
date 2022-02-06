(define-library
  (kawa-spark-test mapping-test)
  (import 
    (scheme base)
    (scheme write)
    (srfi 64)
    (kawa-spark-test test-util)
    (arvyy kawa-spark)
    (class org.apache.commons.io IOUtils)
    (class java.nio.charset StandardCharsets)
    (class org.apache.http.client.fluent Request Response Content Form)
    (class org.apache.http.entity ContentType))
  (export do-mapping-test)
  
  (begin
    (define (do-mapping-test)

      (static-files/location "/static1")
      (static-files/external-location "src/test/resources/static2")
      
      (port 8080)
      (init)
      (await-initialization)

      (get "/a" (lambda (req resp) 
                  (resp/set-type! resp "text/plain; charset=utf-8")
                  "a-get"))

      (put "/a" (lambda (req resp)
                  (resp/set-type! resp "text/plain; charset=utf-8")
                  "a-put"))

      (post "/a" (lambda (req resp)
                   (resp/set-type! resp "text/plain; charset=utf-8")
                   "a-post"))

      (delete "/a" (lambda (req resp)
                     (resp/set-type! resp "text/plain; charset=utf-8")
                     "a-delete"))

      (options "/a" (lambda (req resp)
                      (resp/set-type! resp "text/plain; charset=utf-8")
                      "a-options"))

      (path "/a"
            (get "/subpath" (lambda (req resp)
                              (resp/set-type! resp "text/plain; charset=utf-8")
                              "a-subpath-get")))

      (get "/before1" (lambda (req resp)
                          (resp/set-type! resp "text/plain; charset=utf-8")
                          ""))

      (get "/before2" (lambda (req resp)
                          (resp/set-type! resp "text/plain; charset=utf-8")
                          ""))

      (get "/after1" (lambda (req resp)
                         (resp/set-type! resp "text/plain; charset=utf-8")
                         ""))

      (get "/after2" (lambda (req resp)
                         (resp/set-type! resp "text/plain; charset=utf-8")
                         ""))

      (get "/after3" (lambda (req resp)
                         (resp/set-type! resp "text/plain; charset=utf-8")
                         ""))

      (get "/after4" (lambda (req resp)
                         (resp/set-type! resp "text/plain; charset=utf-8")
                         ""))

      (redirect/get "/redirect-get" "/a")
      (redirect/post "/redirect-post" "/a")
      (redirect "/redirect-any" "/a")

      (before "/before1" (lambda (req resp)
                             (resp/set-type! resp "text/plain; charset=utf-8")
                             (halt! 200 "a-before1")))

      (before-all (lambda (req resp)
                    (when (equal? (req/url req) "http://localhost:8080/before2")
                      (resp/set-type! resp "text/plain; charset=utf-8")
                      (halt! 200 "a-before2"))))

      (after "/after1" (lambda (req resp)
                           (resp/set-type! resp "text/plain; charset=utf-8")
                           (resp/set-body! resp "a-after1")))

      (after-all (lambda (req resp)
                   (when (equal? (req/url req) "http://localhost:8080/after2")
                     (resp/set-type! resp "text/plain; charset=utf-8")
                     (resp/set-body! resp "a-after2"))))

      (after-after "/after3" (lambda (req resp)
                                 (resp/set-type! resp "text/plain; charset=utf-8")
                                 (resp/set-body! resp "a-after3")))

      (after-after-all (lambda (req resp)
                         (when (equal? (req/url req) "http://localhost:8080/after4")
                           (resp/set-type! resp "text/plain; charset=utf-8")
                           (resp/set-body! resp "a-after4"))))
      
      (not-found
        (lambda (req resp)
          "Not found message"))
      
      (get "/exception"
           (lambda (req resp)
             (error "exception-msg" 'exception-obj)))
      
      (exception 
        (lambda (e req resp)
          (define port (open-output-string))
          (write (cons (error-object-message e)
                       (error-object-irritants e)) port)
          (resp/set-body! resp (get-output-string port))))
      

      (await-initialization)

      (test-req (Request:Get "http://localhost:8080/a") "a-get")
      (test-req (Request:Put "http://localhost:8080/a") "a-put")
      (test-req (Request:Post "http://localhost:8080/a") "a-post")
      (test-req (Request:Delete "http://localhost:8080/a") "a-delete")
      (test-req (Request:Options "http://localhost:8080/a") "a-options")
      (test-req (Request:Get "http://localhost:8080/a/subpath") "a-subpath-get")
      (test-req (Request:Get "http://localhost:8080/before1") "a-before1")
      (test-req (Request:Get "http://localhost:8080/before2") "a-before2")
      (test-req (Request:Get "http://localhost:8080/after1") "a-after1")
      (test-req (Request:Get "http://localhost:8080/after2") "a-after2")
      (test-req (Request:Get "http://localhost:8080/after3") "a-after3")
      (test-req (Request:Get "http://localhost:8080/after4") "a-after4")
      (test-req (Request:Get "http://localhost:8080/redirect-get") "a-get")
      (test-req (Request:Get "http://localhost:8080/redirect-any") "a-get")
      (let* ((req (Request:Get "http://localhost:8080/not-found"))
             (resp (req:execute))
             (http-resp (resp:returnResponse))
             (resp-string (IOUtils:toString ((http-resp:getEntity):getContent) StandardCharsets:UTF_8)))
        (test-equal 404 ((http-resp:getStatusLine):getStatusCode))
        (test-equal "Not found message" resp-string))
      (test-req (Request:Get "http://localhost:8080/exception") "(\"exception-msg\" exception-obj)")
      (test-req (Request:Get "http://localhost:8080/test1.txt") "test1\n")
      (test-req (Request:Get "http://localhost:8080/test2.txt") "test2\n")


      (stop)
      (await-stop))))

