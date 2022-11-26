
;; setting block in settings page
(define-mustache-record-type <setting>
  setting-lookup
  (make-setting name legend description default-value options)
  setting?
  (name setting-name)
  (legend setting-legend)
  (description setting-description)
  (default-value setting-default-value)
  (options setting-options))

;; settings block's option
(define-mustache-record-type <setting-option>
  setting-option-lookup
  (make-setting-option value selected?)
  setting-option?
  (value setting-option-value)
  (selected? setting-option-selected?))


(define settings-data
  (list
   (make-setting "pageSize"
                 "Page size"
                 ""
                 "40"
                 '("10" "40" "100"))

   (make-setting "filterParamsLoose"
                 "Use loose parameter filtering"
                 "When enabled, filtering by parameter of union type, will return results that take parameter of
                      a type that composes that union. For example, filtering by `list?` (which is union type of `pair?`
                      and `null?`) will find functions that take `pair?` argument. This leads to showing functions
                      that searcher is probably interested in, however at the drawback that those functions won't be
                      applicable in general case"
                 "yes"
                 '("yes" "no"))))

(define settings-cookies '("pageSize" "filterParamsLoose"))

(define (render-settings req)
  (define cookies (req/cookies req))
  (map
   (lambda (s)
     (define name (setting-name s))
     (define selected-value
       (cond
        ((assoc (string->symbol name) cookies) => cdr)
        (else (setting-default-value s))))
     (define options
       (map
        (lambda (value)
          (make-setting-option value (equal? value selected-value)))
        (setting-options s)))
     (make-setting (setting-name s)
                   (setting-legend s)
                   (setting-description s)
                   (setting-default-value s)
                   options))
   settings-data))

(define (render-settings-page req settings filtersets)
  (values
   "settings"
   (make-page
    (make-page-head "Settings")
    (make-navigation (make-mustache-nav-data 'settings filtersets (deploy-setting/enable-user-settings settings)))
    (render-settings req))))
