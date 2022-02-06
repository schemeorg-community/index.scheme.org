(define-library
  (arvyy slf4j)
  (import (scheme base)
          (class org.slf4j Logger LoggerFactory)
          )
  (export
    get-logger
    logger-name 
    
    log-info
    info-enabled?
    
    log-debug
    debug-enabled?
    
    log-error
    error-enabled?
    
    log-trace
    trace-enabled?
    
    log-warn
    warn-enabled?)
  
  (begin

    (define (get-logger name ::String) ::Logger
      (LoggerFactory:getLogger name))
    
    (define (logger-name logger ::Logger) ::String
      (logger:getName))
    
    (define-syntax log-info
      (syntax-rules ()
        ((_ logger message args ...)
         (when (info-enabled? logger)
           (logger:info message args ...)))))
    
    (define (info-enabled? logger ::Logger) ::boolean
      (logger:isInfoEnabled))
    
    (define-syntax log-warn
      (syntax-rules ()
        ((_ logger message args ...)
         (when (warn-enabled? logger)
           (logger:warn message args ...)))))
    
    (define (warn-enabled? logger ::Logger) ::boolean
      (logger:isWarnEnabled))
    
    (define-syntax log-error
      (syntax-rules ()
        ((_ logger message args ...)
         (when (error-enabled? logger)
           (logger:error message args ...)))))
    
    (define (error-enabled? logger ::Logger) ::boolean
      (logger:isErrorEnabled))
    
    (define-syntax log-debug
      (syntax-rules ()
        ((_ logger message args ...)
         (when (debug-enabled? logger)
           (logger:debug message args ...)))))
    
    (define (debug-enabled? logger ::Logger) ::boolean
      (logger:isDebugEnabled))
    
    (define-syntax log-trace
      (syntax-rules ()
        ((_ logger message args ...)
         (when (trace-enabled? logger)
           (logger:trace message args ...)))))
    
    (define (trace-enabled? logger ::Logger) ::boolean
      (logger:isTraceEnabled))))
