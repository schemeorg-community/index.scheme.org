(define-library
  (scmindex mustache)
  (import (scheme base)
          (scheme char)
          (scheme read)
          (scheme write)
          (scheme cxr)
          (arvyy mustache)
          (arvyy kawa-spark)
          (scmindex domain)
          (only (srfi 1) iota filter find))
  
  (export
    render-home-page
    render-search-page
    render-settings-page
    settings-cookies
    data-lookup
    user-setting/page-size
    user-setting/param-filter-loose

    ;; exported for testing
    render-index-entry)
  (begin

    (define (make-lookup pred alist)
      (lambda (obj name found not-found)
        (cond
          ((not (pred obj)) (not-found))
          ((assoc (string->symbol name) alist)
           =>
           (lambda (e)
             (found ((cdr e) obj))))
          (else (not-found)))))

    (define-syntax define-mustache-record-type
      (syntax-rules ()
        ((_ type lookup constructor pred (field getter) ...)
         (begin
           (define-record-type type constructor pred (field getter) ...)
           (define lookup
             (let ((getter-map (list (cons (quote field) getter) ...)))
               (make-lookup pred getter-map)))))))


    (define-mustache-record-type <sexpr-el>
                                 sexpr-el-lookup
                                 (make-sexpr-el html text class link sub-exprs)
                                 sexpr-el?
                                 (html sexpr-el-html)
                                 (text sexpr-el-text)
                                 (class sexpr-el-class)
                                 (link sexpr-el-link)
                                 (sub-exprs sexpr-el-sub-exprs))

    (define-mustache-record-type <navigation>
                                 navigation-lookup
                                 (make-navigation items)
                                 navigation?
                                 (items navigation-items))

    (define-mustache-record-type <nav-item>
                                 nav-item-lookup
                                 (make-nav-item label icon-cls link active?)
                                 nav-item?
                                 (label nav-item-label)
                                 (icon-cls nav-item-icon-cls)
                                 (link nav-item-link)
                                 (active? nav-item-active?))

    (define-mustache-record-type <pager-btn>
                                 pager-button-lookup
                                 (make-pager-button number link gap?)
                                 pager-button?
                                 (number pager-button-number)
                                 (link pager-button-link)
                                 (gap? pager-button-gap?))

    (define-mustache-record-type <facet>
                                 facet-lookup
                                 (make-facet name title options)
                                 facet?
                                 (name facet-name)
                                 (title facet-title)
                                 (options facet-options))

    (define-mustache-record-type <facet-option>
                                 facet-option-lookup
                                 (make-facet-option value label count selected?)
                                 facet-option?
                                 (value facet-option-value)
                                 (label facet-option-label)
                                 (count facet-option-count)
                                 (selected? facet-option-selected?))

    (define-mustache-record-type <search-result-mustache>
                                 search-result-mustache-lookup
                                 (make-search-result-mustache query facets pages procedures)
                                 search-result-mustache?
                                 (query search-result-mustache-query)
                                 (facets search-result-mustache-facets)
                                 (pages search-result-mustache-pages)
                                 (procedures search-result-mustache-procedures))

    (define-mustache-record-type <result-item>
                                 result-item-lookup
                                 (make-result-item signature param-signatures subsyntax-signatures syntax-param-signatures tags lib parameterized-by)
                                 result-item?
                                 (signature result-item-signature)
                                 (param-signatures result-item-param-signatures)
                                 (subsyntax-signatures result-item-subsyntax-signatures)
                                 (syntax-param-signatures result-item-syntax-param-signatures)
                                 (tags result-item-tags)
                                 (lib result-item-lib)
                                 (parameterized-by result-item-parameterized-by))

    (define-mustache-record-type <page-head>
                                 page-head-lookup
                                 (make-page-head title light-theme ctrlf-override)
                                 page-head?
                                 (title page-head-title)
                                 (light-theme page-head-light-theme)
                                 (ctrlf-override page-head-ctrlf-override))

    (define-mustache-record-type <setting>
                                 setting-lookup
                                 (make-setting name legend description default-value options)
                                 setting?
                                 (name setting-name)
                                 (legend setting-legend)
                                 (description setting-description)
                                 (default-value setting-default-value)
                                 (options setting-options))

    (define-mustache-record-type <setting-option>
                                 setting-option-lookup
                                 (make-setting-option value selected?)
                                 setting-option?
                                 (value setting-option-value)
                                 (selected? setting-option-selected?))

    (define-mustache-record-type <page>
                                 page-lookup
                                 (make-page head navigation body)
                                 page?
                                 (head page-head)
                                 (navigation page-navigation)
                                 (body page-body))

    (define-mustache-record-type <syntax-rule>
                                 syntax-rule-lookup
                                 (make-syntax-rule name rules)
                                 syntax-rule?
                                 (name syntax-rule-name)
                                 (rules syntax-rule-rules))

    (define (result-item-extra-lookup obj name found not-found)
      (cond
        ((not (result-item? obj)) (not-found))
        ((equal? "has-param-signatures?" name) (found (not (null? (result-item-param-signatures obj)))))
        ((equal? "has-subsyntax-signatures?" name) (found (not (null? (result-item-subsyntax-signatures obj)))))
        ((equal? "has-syntax-param-signatures?" name) (found (not (null? (result-item-syntax-param-signatures obj)))))
        ((equal? "has-parameterized-by?" name) (found (not (null? (result-item-parameterized-by obj)))))
        (else (not-found))))

    (define (facet-extra-lookup obj name found not-found)
      (cond
        ((not (facet? obj)) (not-found))
        ((equal? "show-facet-controls?" name) (found (> (length (facet-options obj)) 10)))
        (else (not-found))))

    (define data-lookup
      (compose-lookups
        sexpr-el-lookup
        navigation-lookup
        nav-item-lookup
        pager-button-lookup
        facet-lookup
        facet-option-lookup
        search-result-mustache-lookup
        result-item-lookup
        page-head-lookup
        setting-lookup
        setting-option-lookup
        page-lookup
        syntax-rule-lookup
        result-item-extra-lookup
        facet-extra-lookup))

    (define make-mustache-nav-data
      (let ((pages '(("Home" "icon-home3" "/" index)
                     ("Search" "icon-search" "/search" search)
                     ("Settings" "icon-cog" "/settings" settings)
                     ("Documentation" "icon-file-text2" "/README.html" docs))))
        (lambda (page)
          (map
            (lambda (p)
              (define active? (equal? (list-ref p 3) page))
              (make-nav-item (list-ref p 0) (list-ref p 1) (list-ref p 2) active?))
            pages))))

    (define settings-data
      (list
        (make-setting "pageSize"
                      "Page size"
                      ""
                      "40"
                      '("10" "40" "100"))

        (make-setting "theme"
                      "Theme"
                      ""
                      "light"
                      '("light" "dark"))

        (make-setting "overrideCtrlF"
                      "Override control+f behavior"
                      "If enabled, pressing control+f will highlight and focus search text field"
                      "no"
                      '("yes" "no"))

        (make-setting "filterParamsLoose"
                      "Use loose parameter filtering"
                      "When enabled, filtering by parameter of union type, will return results that take parameter of
                      a type that composes that union. For example, filtering by `list?` (which is union type of `pair?`
                                                                                                and `null?`) will find functions that take `pair?` argument. This leads to showing functions
                      that searcher is probably interested in, however at the drawback that those functions won't be
                      applicable in general case"
                      "yes"
                      '("yes" "no"))))

    (define settings-cookies '("overrideCtrlF" "theme" "pageSize" "filterParamsLoose"))

    (define (user-setting/page-size req)
      (cond
        ((assoc 'pageSize (req/cookies req)) => (lambda (e)
                                                  (string->number (cdr e))))
        (else 40)))

    (define (user-setting/light-theme? req)
      (cond
        ((assoc 'theme (req/cookies req)) => (lambda (e)
                                               (equal? "light" (cdr e))))
        (else #t)))

    (define (user-setting/ctrl-f-override req)
      (cond
        ((assoc 'overrideCtrlF (req/cookies req)) => (lambda (e)
                                                       (equal? "yes" (cdr e))))
        (else #f)))

    (define (user-setting/param-filter-loose req)
      (cond
        ((assoc 'filterParamsLoose (req/cookies req)) => (lambda (e)
                                                           (equal? "yes" (cdr e))))
        (else #t)))

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

    (define (render-search-result page page-size query libs tags param-types return-types parameterized-by search-result)

      (define (remove-parens str)
        (list->string
          (filter
            (lambda (ch)
              (and (not (eqv? ch #\())
                   (not (eqv? ch #\)))))
            (string->list str))))

      (define current-query
        (append
          `((page . ,(number->string page)))
          (if query
              `((query . ,query))
              '())
          (map
            (lambda (lib)
              (cons 'lib lib)) 
            libs)
          (map
            (lambda (param)
              (cons 'param param)) 
            param-types)
          (map
            (lambda (return)
              (cons 'return return)) 
            return-types)
          (map
            (lambda (tag)
              (cons 'tag tag)) 
            tags)
          (map
            (lambda (tag)
              (cons 'parameterized tag)) 
            parameterized-by)))

      (make-search-result-mustache
        query
        (list
          (make-facet "lib" "Library" (parse-facet-options (search-result-libs search-result) libs remove-parens))
          (make-facet "tag" "Tag" (parse-facet-options (search-result-tags search-result) tags #f))
          (make-facet "param" "Parameter type" (parse-facet-options (search-result-params search-result) param-types #f))
          (make-facet "return" "Return type" (parse-facet-options (search-result-returns search-result) return-types #f))
          (make-facet "parameterized" "Parameterized by" (parse-facet-options (search-result-parameterized-by search-result) parameterized-by #f)))
        (render-pager page (ceiling (/ (search-result-total search-result) page-size)) current-query)
        (map render-index-entry (search-result-items search-result))))

    (define (parse-facet-options facet-result selected-values label-transformer)
      (define fn (if label-transformer label-transformer (lambda (x) x)))
      (map
        (lambda (f)
          (define value (search-result-facet-value f))
          (define selected? (member value selected-values))
          (make-facet-option value (fn value) (search-result-facet-count f) selected?))
        facet-result))

    (define percent-encoding
      '((#\space . "%20")
        (#\! . "%21")
        (#\# . "%23")
        (#\$ . "%24")
        (#\% . "%25")
        (#\& . "%26")
        (#\' . "%27")
        (#\( . "%28")
        (#\) . "%29")
        (#\* . "%2A")
        (#\+ . "%2B")
        (#\, . "%2C")
        (#\/ . "%2F")
        (#\: . "%3A")
        (#\; . "%3B")
        (#\= . "%3D")
        (#\? . "%3F")
        (#\@ . "%40")
        (#\[ . "%5B")
        (#\] . "%5D")))

    (define (urlencode str)
      (define port (open-output-string))
      (string-for-each
        (lambda (c)
          (cond
            ((assv c percent-encoding) => (lambda (e)
                                            (write-string (cdr e) port)))
            (else (write-char c port))))
        str)
      (get-output-string port))

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
                      (urlencode (symbol->string key))
                      "="
                      (urlencode value)))
                  (define new-str
                    (if first
                        fragment
                        (string-append str "&" fragment)))
                  (loop new-str rest #f))))))

    (define (render-pager page total-pages query)
      (define query-without-page
        (filter
          (lambda (e)
            (not (equal? 'page (car e))))
          query))
      (define near/start (max 1 (- page 3)))
      (define near/end (min total-pages (+ page 3)))
      (define near-range (iota (+ 1 near/end (- near/start)) near/start))
      (define begining
        (case near/start
          ((1) '())
          ((2) '(1))
          (else '(1 #f))))
      (define ending
        (case (- total-pages near/end)
          ((0) '())
          ((1) (list total-pages))
          (else (list #f total-pages))))
      (define shown-pages
        (append begining near-range ending))

      (map
        (lambda (p)
          (if (not p)
              (make-pager-button #f #f #t)
              (let* ((link-query
                       (cons
                         (cons 'page (number->string p))
                         query-without-page))
                     (link
                       (if (= p page)
                           #f
                           (string-append "?" (encode-query link-query)))))
                (make-pager-button p link #f))))
        shown-pages))

    (define (render-index-entry index-entry)
      (define signature (index-entry-signature index-entry))
      (define-values
        (param-signatures subsyntax-signatures signature-sd)
        (case (car signature)
          ((lambda)
           (values (map
                     (lambda (param-sig)
                       (parameterize ((suppress-make-link #t))
                         (render-procedure-signature (symbol->string (car param-sig))
                                                  (cadr param-sig)
                                                  #t)))
                     (index-entry-param-signatures index-entry))
                   '()
                   (render-procedure-signature (index-entry-name index-entry)
                                              signature 
                                              #f)))
          ((syntax-rules)
           (values '()
                   (let ((literals (cadr signature)))
                     (map
                       (lambda (param)
                         (make-syntax-rule (symbol->string (car param))
                                           (map
                                             (lambda (rule)
                                               (render-syntax-signature-signature literals rule))
                                             (cdr param))))
                       (index-entry-param-signatures index-entry)))
                   (render-syntax-signature (index-entry-name index-entry)
                                                     signature)))
          ((value)
           (values '()
                   '()
                   (render-value-signature (index-entry-name index-entry)
                                                    signature)))))
      (define syntax-param-signatures
        (render-param-signatures (index-entry-syntax-param-signatures index-entry)))

      (make-result-item
        signature-sd
        param-signatures
        subsyntax-signatures
        syntax-param-signatures
        (index-entry-tags index-entry)
        (index-entry-lib index-entry)
        (index-entry-parameterized-by index-entry)))

    (define (render-syntax-signature-signature literals rule)
      (define (term-handler term)
        (cond
          ((find (lambda (el) (equal? term el)) literals)
           (make-sexpr-el #f (symbol->string term) "bright-syntax" #f #f))
          ((or (equal? '... term)
               (equal? '|#| term))
           (make-sexpr-el #f (symbol->string term) "muted" #f #f))
          (else
            (make-sexpr-el #f #f "sexpr-flex muted" #f
                           (list (make-sexpr-el "&#x27E8" #f #f #f #f)
                                 (make-sexpr-el #f (symbol->string term) #f #f #f)
                                 (make-sexpr-el "&#x27E9" #f #f #f #f))))))
      (make-sexpr-el #f #f "sexpr-flex" #f (render-sexpr (cons rule '()) term-handler 1)))

    (define (render-param-signatures params)
      (map
        (lambda (param)
          (define name (symbol->string (car param)))
          (define type (cadr param))
          (make-sexpr-el #f #f "sexpr-flex" #f
                         (list
                           paren-open-sd
                           (render-param-type type #f)
                           spacer-sd
                           (make-sexpr-el #f name "muted" #f #f)
                           paren-close-sd)))
        params))

    (define suppress-make-link (make-parameter #f))

    (define (make-link type param?)
      (cond
        ((suppress-make-link) #f)
        (param? (string-append "?" (encode-query `((return . ,(symbol->string type))))))
        (else (string-append "?" (encode-query `((param . ,(symbol->string type))))))))

    (define (render-syntax-signature name signature)
      (define rules
        (map 
          (lambda (r)
            `((,name ,@(cdar r)) ,@(cdr r)))
          (cddr signature)))
      (define literals (cadr signature))
      (define (term-handler term)
        (cond
          ((or (equal? name term)
               (find (lambda (el) (equal? term el)) literals))
           (make-sexpr-el #f (symbol->string term) "bright-syntax" #f #f))
          ((equal? '... term)
           (make-sexpr-el #f "..." "muted" #f #f))
          (else
            (make-sexpr-el #f #f "sexpr-flex muted" #f
                           (list (make-sexpr-el "&#x27E8" #f #f #f #f)
                                 (make-sexpr-el #f (symbol->string term) #f #f #f)
                                 (make-sexpr-el "&#x27E9" #f #f #f #f))))))
      (define rules-sds
        (map
          (lambda (rule)
            (define return
              (if (= 1 (length rule))
                  '()
                  `(,(render-return-type (cadr rule) #f))))
            (make-sexpr-el #f #f "sexpr-flex" #f (append (render-sexpr (cons (car rule) '()) term-handler 0) return)))
          rules))
      (make-sexpr-el #f #f "sexpr-flex-col" #f rules-sds))

    (define (render-sexpr sexpr term-handler depth)
      (define (wrap-list sexpr)
        (define new-depth
          (if (pair? sexpr)
              (+ 1 depth)
              depth))
        (define processed-lst 
          (render-sexpr sexpr term-handler new-depth))
        (if (pair? sexpr)
            (list (make-sexpr-el #f #f "sexpr-flex" #f
                                 `(,(make-sexpr-el #f "(" (string-append "syntaxbracket-" (number->string depth)) #f #f)
                                    ,@processed-lst
                                    ,(make-sexpr-el #f ")" (string-append "syntaxbracket-" (number->string depth)) #f #f))))
            processed-lst))

      (cond
        ((and (pair? sexpr)
              (pair? (car sexpr))
              (equal? '_append (caar sexpr)))
         (apply append (map
                         (lambda (el)
                           (render-sexpr (cons el '()) term-handler depth))
                         (cdar sexpr))))
        ((symbol? sexpr) 
         (list (term-handler sexpr)))
        ((and (pair? sexpr) (symbol? (cdr sexpr)))
         `(,@(wrap-list (car sexpr))
            ,spacer-sd
            ,(make-sexpr-el "." #f "muted" #f #f)
            ,spacer-sd
            ,@(render-sexpr (cdr sexpr) term-handler depth)))
        ((pair? sexpr)
         `(,@(wrap-list (car sexpr))
            ,spacer-sd
            ,@(render-sexpr (cdr sexpr) term-handler depth)))
        ((null? sexpr)
         (list))
        (else (error sexpr))))

    (define spacer-sd
      (make-sexpr-el #f #f "spacer" #f #f))

    (define paren-open-sd
      (make-sexpr-el #f "(" "muted" #f #f))

    (define paren-close-sd
      (make-sexpr-el #f ")" "muted" #f #f))

    (define long-arrow-sd
      (make-sexpr-el "&DoubleLongRightArrow;" #f "muted" #f #f))

    (define slash (make-sexpr-el #f "/" "muted-type" #f #f))

    (define (render-return-type returns sub?)
      (define (do-render-return-type value)
        (cond
          ((or (equal? value '...)
               (equal? value 'undefined)
               (equal? value '*))
           (make-sexpr-el #f (symbol->string value) "muted" #f #f))
          ((equal? value #f)
           (make-sexpr-el #f "#f" (if sub? "muted-name" "bright-syntax") #f #f))
          ((symbol? value)
           (make-sexpr-el #f (symbol->string value) (if sub? "muted-name" "bright-type") (make-link value #f) #f))
          ((list? value)
           (make-sexpr-el #f #f "sexpr-flex" #f `(
                                                  ,paren-open-sd
                                                  ,(make-sexpr-el #f (symbol->string (car value)) "muted" #f #f)
                                                  ,@(map
                                                      (lambda (e)
                                                        (make-sexpr-el #f #f "sexpr-flex" #f (list spacer-sd (do-render-return-type e))))
                                                      (cdr value))
                                                  ,paren-close-sd)))))
      (make-sexpr-el #f #f "sexpr-flex" #f (list spacer-sd long-arrow-sd spacer-sd (do-render-return-type returns))))

    (define (render-param-type type sub?)
      (cond
        ((symbol? type)
         (make-sexpr-el #f type (if sub? "muted-type" "bright-type") (make-link type #t) #f))
        ((equal? #f type)
         (make-sexpr-el #f "#f" (if sub? "muted-type" "bright-syntax") #f #f))
        ((list? type)
         (let loop ((types (cdr type))
                    (sds '()))
           (cond
             ((null? types)
              (make-sexpr-el #f #f "sexpr-flex" #f (cdr sds)))
             (else (let* ((type (car types))
                          (sd (render-param-type type sub?)))
                     (loop (cdr types)
                           (append (list slash sd) sds)))))))))

    (define (render-procedure-signature name sig sub?)

      (define name-sd (make-sexpr-el #f name (if sub? "muted-name" "bright-name") #f #f))

      (define (render-params-block params)
        (let loop ((params params)
                   (last (null? (cdr (cadr sig))))
                   (result '()))
          (define param (car params))
          (define sd
            (cond
              ((list? param)
               (make-sexpr-el #f #f "sexpr-flex" #f
                              `(,spacer-sd
                                 ,paren-open-sd
                                 ,(render-param-type (car param) sub?)
                                 ,spacer-sd
                                 ,(make-sexpr-el #f (cadr param) "muted" #f #f)
                                 ,paren-close-sd
                                 ,@(if last (list paren-close-sd) (list)))))
              (else
                (make-sexpr-el #f #f "sexpr-flex" #f
                               `(,spacer-sd
                                  ,(make-sexpr-el #f (symbol->string param) "muted" #f #f)
                                  ,@(if last (list paren-close-sd) (list)))))))

          (if last
              (reverse (cons sd result))
              (loop (cdr params)
                    (null? (cddr params))
                    (cons sd result)))))

      (define params (cadr sig))
      (define return-sd (render-return-type (caddr sig) sub?))

      (if (null? params)
          (make-sexpr-el #f #f "sexpr-flex" #f
                         (list paren-open-sd
                               name-sd
                               paren-close-sd
                               return-sd))
          (make-sexpr-el #f #f "sexpr-flex" #f
                         (list paren-open-sd
                               name-sd
                               (make-sexpr-el #f #f "sexpr-flex sexpr-shrink" #f (render-params-block params))
                               return-sd))))

    (define (render-value-signature name sig)
      (make-sexpr-el #f #f "sexpr-flex" #f
                     (list (make-sexpr-el #f name "bright-name" #f #f)
                           spacer-sd
                           long-arrow-sd
                           spacer-sd
                           (make-sexpr-el #f (symbol->string (cadr sig)) "bright-type" (make-link (cadr sig) #f) #f))))

    (define (get-page-head title req)
      (make-page-head title (user-setting/light-theme? req) (user-setting/ctrl-f-override req)))

    (define (render-home-page req)
      (values
        "index"
        (make-page
          (get-page-head #f req)
          (make-navigation (make-mustache-nav-data 'index))
          #f)))

    (define (render-search-page req page page-size query libs tags param-types return-types parameterized-by search-result)
      (values
        "search"
        (make-page
          (get-page-head "Search" req)
          (make-navigation (make-mustache-nav-data 'search))
          (render-search-result page page-size query libs tags param-types return-types parameterized-by search-result))))

    (define (render-settings-page req)
      (values
        "settings"
        (make-page
          (get-page-head "Settings" req)
          (make-navigation (make-mustache-nav-data 'settings))
          (render-settings req))))

))
