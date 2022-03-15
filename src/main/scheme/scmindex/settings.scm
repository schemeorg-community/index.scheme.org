(define-library
  (scmindex settings)
  (import (scheme base)
          (scheme read)
          (srfi 26)
          (arvyy kawa-spark))
  (export
    deploy-setting/solr-url
    deploy-setting/solr-core
    deploy-setting/cache-templates
    deploy-setting/page-size
    deploy-setting/serve-static

    user-setting/page-size
    user-setting/light-theme?
    user-setting/ctrl-f-override
    user-setting/param-filter-loose
    
    settings-options
    mustache-settings-data)

  (begin

    (define (get-property deploy-settings prop)
      (cond
        ((assoc prop deploy-settings) => cdr)
        (else (error (string-append "Required property missing " (symbol->string prop))))))

    (define deploy-setting/solr-url (cut get-property <> 'solr-url))
    (define deploy-setting/solr-core (cut get-property <> 'solr-core))
    (define deploy-setting/cache-templates (cut get-property <> 'cache-templates))
    (define deploy-setting/page-size (cut get-property <> 'page-size))
    (define deploy-setting/serve-static (cut get-property <> 'serve-static))

    (define settings-data
      `#(
         ((name . "pageSize")
          (legend . "Page size")
          (description . "")
          (default-value . "40")
          (values . #("10" "40" "100")))
         
         ((name . "theme")
          (legend . "Theme")
          (description . "")
          (default-value . "light")
          (values . #("light" "dark")))
         
         ((name . "overrideCtrlF")
          (legend . "Override control+f behavior")
          (description . "If enabled, pressing control+f will highlight and focus search text field")
          (default-value . "no")
          (values . #("yes" "no")))
         
         ((name . "filterParamsLoose")
          (legend . "Use loose parameter filtering")
          (description . "When enabled, filtering by parameter of union type, will return results that take parameter of 
                       a type that composes that union. For example, filtering by `list?` (which is union type of `pair?` 
                       and `null?`) will find functions that take `pair?` argument. This leads to showing functions
                       that searcher is probably interested in, however at the drawback that those functions won't be
                       applicable in general case")
          (default-value . "yes")
          (values . #("yes" "no")))))

      (define settings-options '("overrideCtrlF" "theme" "pageSize" "filterParamsLoose"))

      (define (user-setting/page-size req)
        (cond
          ((assoc 'pageSize (req/cookies req)) => (lambda (e)
                                                    (string->number (cdr e))))
          (else 40)))

      (define (user-setting/light-theme? req)
        (cond
          ((assoc 'overrideCtrlF (req/cookies req)) => (lambda (e)
                                                    (equal? "light" (cdr e))))
          (else #t)))

      (define (user-setting/ctrl-f-override req)
        (cond
          ((assoc 'theme (req/cookies req)) => (lambda (e)
                                                    (equal? "yes" (cdr e))))
          (else #f)))
      
      (define (user-setting/param-filter-loose req)
        (cond
          ((assoc 'filterParamsLoose (req/cookies req)) => (lambda (e)
                                                             (equal? "yes" (cdr e))))
          (else #t)))
    
    (define (mustache-settings-data req)
      (define cookies (req/cookies req))
      (define data
        (vector-map
          (lambda (option)
            (define name (cdr (assoc 'name option)))
            (define selected-value
              (cond
                ((assoc (string->symbol name) cookies) => cdr)
                (else (cdr (assoc 'default-value option)))))
            (define values+selection
              (vector-map
                (lambda (value)
                  `((value . ,value)
                    (selected . ,(equal? value selected-value))))
                (cdr (assoc 'values option))))
            `((values . ,values+selection) ,@option))
          settings-data))
      `((options . ,data)))

    ))
