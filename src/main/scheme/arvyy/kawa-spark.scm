(define-library
  (arvyy kawa-spark)
  (import (scheme base)
          (class spark Spark Request Response Session Filter Route))
  (export 
    ;; path mappings
    get
    post
    put
    delete
    options
    before
    before-all
    after
    after-all
    after-after
    after-after-all
    redirect/get
    redirect/post
    redirect
    path
    
    ;; request
    req/attributes
    req/attribute
    req/set-attribute!
    req/body
    req/body-as-bytes
    req/content-length
    req/content-type
    req/context-path
    req/cookies
    req/cookie
    req/headers
    req/header
    req/host
    req/ip
    req/params
    req/param
    req/path-info
    req/port
    req/protocol
    req/query-string
    req/query-params
    req/query-param
    req/query-param-values
    req/request-method
    req/scheme
    req/session
    req/create-session!
    req/splat
    req/uri
    req/url
    req/user-agent
    
    ;; response
    resp/body
    resp/set-body!
    resp/set-header!
    resp/redirect
    resp/status
    resp/set-status!
    resp/type
    resp/set-type!
    resp/set-cookie!
    resp/remove-cookie!
    
    ;; session
    session/attribute
    session/set-attribute!
    session/remove-attribute!
    session/attributes
    session/id
    session/new?
    
    ;; halt
    halt!
    
    ;; error handling
    not-found
    internal-server-error
    exception
    
    ;; static files
    static-files/location
    static-files/external-location
    static-files/expire-time
    static-files/header
    
    ;; other
    init
    stop
    port
    secure
    thread-pool
    await-initialization
    await-stop)
  
  (begin
    
    ;; private util
    (define (map->alist m ::java.util.Map)
      (define (entry->pair e ::java.util.Map:Entry)
        (cons
          (string->symbol (e:getKey))
          (e:getValue)))
      (map entry->pair (m:entrySet)))
    
    (define-syntax route-stx
      (syntax-rules ()
        ((_ fn)
         (lambda (req resp) (fn req resp)))))
    
    
    ;; path mappings
    (define (get path route)
      (Spark:get path (route-stx route)))
    (define (post path route)
      (Spark:post path (route-stx route)))
    (define (put path route)
      (Spark:put path (route-stx route)))
    (define (delete path route)
      (Spark:delete path (route-stx route)))
    (define (options path route)
      (Spark:options path (route-stx route)))
    (define (before path handler)
      (define (before* p ::String h ::Filter)
        (Spark:before p h))
      (before* path (route-stx handler)))
    (define (before-all handler)
      (define (before-all* h ::Filter)
        (Spark:before h))
      (before-all* (route-stx handler)))
    (define (after path handler)
      (define (after* p ::String h ::Filter)
        (Spark:after p h))
      (after* path (route-stx handler)))
    (define (after-all handler)
      (define (after-all* h ::Filter)
        (Spark:after h))
      (after-all* (route-stx handler)))
    (define (after-after path handler)
      (Spark:afterAfter path (route-stx handler)))
    (define (after-after-all handler)
      (Spark:afterAfter (route-stx handler)))
    (define (redirect/get from to)
      (Spark:redirect:get from to))
    (define (redirect/post from to)
      (Spark:redirect:post from to))
    (define (redirect from to)
      (Spark:redirect:any from to))
    (define-syntax path
      (syntax-rules ()
        ((_ p body ...)
         (Spark:path p (lambda () body ...)))))
    
    ;; request
    (define (req/attributes req ::Request)
      ; convert Set to list
      (map (lambda (e) e) (req:attributes)))
    (define (req/attribute req ::Request attr)
      (req:attribute attr))
    (define (req/set-attribute! req ::Request attr value)
      (req:attribute attr value))
    (define (req/body req ::Request) 
      (req:body))
    (define (req/body-as-bytes req ::Request)
      ; convert java byte array into vector
      (vector-map (lambda (e) e) (req:bodyAsBytes)))
    (define (req/content-length req ::Request) 
      (req:contentLength))
    (define (req/content-type req ::Request) 
      (req:contentType))
    (define req/context-path Request:contextPath)
    (define (req/cookies req ::Request)
      (map->alist (req:cookies)))
    (define (req/cookie req ::Request name) 
      (req:cookie name))
    (define (req/headers req ::Request)
      (map (lambda (e) e) (req:headers)))
    (define (req/header req ::Request name)
      (req:headers name))
    (define (req/host req ::Request) 
      (req:host))
    (define (req/ip req ::Request) 
      (req:ip))
    (define (req/params req ::Request)
      (map->alist (req:params)))
    (define (req/param req ::Request param)
      (define p (req:params param))
      (if (eq? #!null p)
          #f
          p))
    (define (req/path-info req ::Request) 
      (req:pathInfo))
    (define (req/port req ::Request) 
      (req:port))
    (define (req/protocol req ::Request) 
      (req:protocol))
    (define (req/query-string req ::Request) 
      (req:queryString))
    (define (req/query-params req ::Request)
      (map (lambda (e) e) (req:queryParams)))
    (define (req/query-param req ::Request param)
      (define p (req:queryParams param))
      (if (eq? #!null p)
          #f
          p))
    (define (req/query-param-values req ::Request param)
      (define vals (req:queryParamsValues param))
      (map (lambda (e) e) (if (eq? #!null vals) '() vals)))
    (define (req/request-method req ::Request) 
      (req:requestMethod))
    (define (req/scheme req ::Request)
      (req:scheme))
    (define (req/session req ::Request)
      (req:session #f))
    (define (req/create-session! req ::Request)
      (req:session #t))
    (define (req/splat req ::Request)
      (map (lambda (e) e) (req:splat)))
    (define (req/uri req ::Request) 
      (req:uri))
    (define (req/url req ::Request) 
      (req:url))
    (define (req/user-agent req ::Request) 
      (req:userAgent))
    
    ;; response
    (define (resp/body resp ::Response)
      (resp:body))
    (define (resp/set-body! resp ::Response body)
      (resp:body body))
    (define (resp/set-header! resp ::Response name value) 
      (resp:header name value))
    (define (resp/redirect resp ::Response target) 
      (resp:redirect target))
    (define (resp/status resp ::Response)
      (resp:status))
    (define (resp/set-status! resp ::Response status)
      (resp:status status))
    (define (resp/type resp ::Response)
      (resp:type))
    (define (resp/set-type! resp ::Response type)
      (resp:type type))
    (define (resp/set-cookie! resp ::Response name value) 
      (resp:cookie name value))
    (define (resp/remove-cookie! resp ::Response name) 
      (resp:removeCookie name))
    
    ;; session
    (define (session/attribute s ::Session attr)
      (s:attribute attr))
    (define (session/set-attribute! s ::Session attr value)
      (s:attribute attr value))
    (define session/remove-attribute! Session:removeAttribute)
    (define session/attributes Session:attributes)
    (define session/id Session:id)
    (define session/new? Session:isNew)
    
    ;; halt
    (define (halt! code message)
      (Spark:halt code message))

    ;; error handling
    (define (not-found route)
      (define (not-found* r ::Route)
        (Spark:notFound r))
      (not-found* (route-stx route)))
    (define (internal-server-error route)
      (define (internal-server-error* r ::Route)
        (Spark:internalServerError r))
      (internal-server-error* (route-stx route)))
    (define (exception handler)
      (Spark:exception Object:class 
                       (lambda (exception req resp) (handler exception req resp))))
    
    ;; static files
    (define static-files/location Spark:staticFiles:location)
    (define static-files/external-location Spark:staticFiles:externalLocation)
    (define static-files/expire-time Spark:staticFiles:expireTime)
    (define static-files/header Spark:staticFiles:header)
    
    ;; other
    (define init Spark:init)
    (define stop Spark:stop)
    (define port Spark:port)
    (define secure Spark:secure)
    (define thread-pool Spark:threadPool)
    (define await-initialization Spark:awaitInitialization)
    (define await-stop Spark:awaitStop)))
