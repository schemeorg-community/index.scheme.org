(define-library
  (arvyy httpclient)
  (import (scheme base)
          (scheme write)
          (srfi 180)
          (class org.apache.http.client.fluent Request)
          (class org.apache.http.entity ContentType)
          (class java.net URLEncoder))
  (export post-json encode-query)
  
  (begin
    
    (define (post-json url ::String payload)
      (let ((content (open-output-string))
            (r (Request:Post url)))
        (json-write payload content)
        (r:bodyString (get-output-string content) ContentType:APPLICATION_JSON)
        (let* ((resp (r:execute))
               (content (resp:returnContent))
               (str-content (content:asString))
               (str-port (open-input-string str-content)))
          (json-read str-port))))
    
    (define (encode-query alist)
      (let loop ((str "")
                 (alist alist)
                 (first #t))
        (cond
          ((null? alist) str)
          (else (let ((key (caar alist))
                      (value (cdar alist))
                      (rest (cdr alist)))
                  (define fragment
                    (string-append
                      (URLEncoder:encode (symbol->string key) "UTF-8")
                      "="
                      (URLEncoder:encode value "UTF-8")))
                  (define new-str
                    (if first
                        fragment
                        (string-append str "&" fragment)))
                  (loop new-str rest #f))))))
    ))
