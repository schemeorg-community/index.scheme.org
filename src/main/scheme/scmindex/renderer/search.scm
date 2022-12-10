;; paging bar data
(define-mustache-record-type <pager-btn>
  pager-button-lookup
  (make-pager-button number link gap?)
  pager-button?
  (number pager-button-number)
  (link pager-button-link)
  ;; #t if this is a button showing `...` to indicate a gap between visible range and first / last page.
  (gap? pager-button-gap?))

(define-mustache-record-type <ui-link>
  ui-link-lookup
  (make-ui-link href label)
  ui-link?
  (href ui-link-href)
  (label ui-link-label))

;; facet filter, eg "library filter"
(define-mustache-record-type <facet>
  facet-lookup
  (make-facet name title options)
  facet?
  (name facet-name)
  (title facet-title)
  (options facet-options))

;; facet filter's possible options
(define-mustache-record-type <facet-option>
  facet-option-lookup
  (make-facet-option value label count selected?)
  facet-option?
  (value facet-option-value)
  (label facet-option-label)
  (count facet-option-count)
  (selected? facet-option-selected?))

;; search result page data
(define-mustache-record-type <search-result-mustache>
  search-result-mustache-lookup
  (make-search-result-mustache filterset query facets pages search-items)
  search-result-mustache?
  (filterset search-result-mustache-filterset)
  (query search-result-mustache-query)
  (facets search-result-mustache-facets)
  (pages search-result-mustache-pages)
  (search-items search-result-mustache-search-items))

;; single result page data
(define-mustache-record-type <single-item-mustache>
  single-item-mustache-lookup
  (make-single-item-mustache filterset-code filterset-name item searchurl)
  single-item-mustache?
  (filterset-code single-item-mustache-filterset-code)
  (filterset-name single-item-mustache-filterset-name)
  (item single-item-mustache-item)
  (searchurl single-item-mustache-searchurl))

;; result item corresponding to a search-item / index-item in domain
(define-mustache-record-type <result-item>
  result-item-lookup
  (make-result-item name signature param-signatures subsyntax-signatures tags lib parameterized-by spec-values description)
  result-item?
  (name result-item-name)
  ;; sexpr of main signature
  (signature result-item-signature)
  ;; list of sexprs of parameter signatures
  (param-signatures result-item-param-signatures)
  ;; list of sexprs of signatures of types in syntax
  (subsyntax-signatures result-item-subsyntax-signatures)
  (tags result-item-tags)
  (lib result-item-lib)
  (parameterized-by result-item-parameterized-by)
  (spec-values result-item-spec-values)
  (description result-item-description))

(define-mustache-record-type <syntax-rule>
  syntax-rule-lookup
  (make-syntax-rule name rules)
  syntax-rule?
  (name syntax-rule-name)
  (rules syntax-rule-rules))

(define-mustache-record-type <spec-value-fieldblock>
  spec-value-fieldblock-lookup
  (make-spec-value-fieldblock name values)
  spec-value-fieldblock?
  (name spec-value-fieldblock-name)
  (values spec-value-fieldblock-values))

(define-mustache-record-type <spec-value-fieldentry>
  spec-value-fieldentry-lookup
  (make-spec-value-fieldentry value description)
  spec-value-fieldentry?
  (value spec-value-fieldentry-value)
  (description spec-value-fieldentry-description))


;; hide faceting controls (search / expand / collapse) if there are only < 10 choices
(define (facet-extra-lookup obj name found not-found)
  (cond
   ((not (facet? obj)) (not-found))
   ((equal? "show-facet-controls?" name) (found (> (length (facet-options obj)) 10)))
   (else (not-found))))

;; additional lookup names, mostly "null?" predicates
(define (result-item-extra-lookup obj name found not-found)
  (cond
   ((not (result-item? obj)) (not-found))
   ((equal? "has-param-signatures?" name) (found (not (null? (result-item-param-signatures obj)))))
   ((equal? "has-subsyntax-signatures?" name) (found (not (null? (result-item-subsyntax-signatures obj)))))
   ((equal? "has-parameterized-by?" name) (found (not (null? (result-item-parameterized-by obj)))))
   ((equal? "has-spec-values?" name) (found (not (null? (result-item-spec-values obj)))))
   ((equal? "has-description?" name) (found (> (string-length (result-item-description obj)) 0)))
   (else (not-found))))

(define (remove-parens str)
  (list->string
   (filter
    (lambda (ch)
      (and (not (eqv? ch #\())
           (not (eqv? ch #\)))))
    (string->list str))))

(define (render-search-result filterset page page-size query libs tags param-types return-types parameterized-by search-result)
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
   filterset
   query
   (append
    (make-facet* "lib" "Library" (parse-facet-options (search-result-libs search-result) libs remove-parens libname-comparator))
    (make-facet* "tag" "Tag" (parse-facet-options (search-result-tags search-result) tags #f #f))
    (make-facet* "param" "Parameter type" (parse-facet-options (search-result-params search-result) param-types #f #f))
    (make-facet* "return" "Return type" (parse-facet-options (search-result-returns search-result) return-types #f #f))
    (make-facet* "parameterized" "Parameterized by" (parse-facet-options (search-result-parameterized-by search-result) parameterized-by #f #f)))
   (render-pager page (ceiling (/ (search-result-total search-result) page-size)) current-query)
   (map render-index-entry (search-result-items search-result))))

;; returns a list of one element if facet to be shown
;; returns an empty list if facet should be hidden due to not having visible options
(define (make-facet* name title options)
  (if (null? options)
      '()
      (list (make-facet name title options))))

;; in case both libraries point to srfi
;; compare srfi numbers numerically instead of alphabetically
(define (libname-comparator l1 l2)
  (define (srfi? lib)
    (and (list? lib)
         (symbol=? 'srfi (car lib))
         (integer? (cadr lib))))
  (define str1 (facet-option-value l1))
  (define str2 (facet-option-value l2))
  (define v1 (read* str1))
  (define v2 (read* str2))
  (if (and (srfi? v1) (srfi? v2))
      (let ((num1 (cadr v1))
            (num2 (cadr v2)))
        (< num1 num2))
      (string<? str1 str2)))

(define (parse-facet-options facet-result selected-values label-transformer comparator)
  (define fn (if label-transformer label-transformer values))
  (define options
    (map
     (lambda (f)
       (define value (search-result-facet-value f))
       (define selected? (member value selected-values))
       (make-facet-option value (fn value) (search-result-facet-count f) selected?))
     facet-result))
  (define selected-options
    (filter
     (lambda (opt)
       (or (facet-option-selected? opt) (> (facet-option-count opt) 0)))
     options))
  (if comparator
      (sort selected-options comparator)
      selected-options))

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
                     (string-append (baseurl) "search?" (encode-query link-query)))))
           (make-pager-button p link #f))))
   shown-pages))

(define (render-index-entry index-entry)
  (define signature (index-entry-signature index-entry))
  (define link (string-append (baseurl) (urlencode (index-entry-lib index-entry)) "/" (urlencode (symbol->string (index-entry-name index-entry)))))
  (define-values
      (param-signatures subsyntax-signatures signature-sd)
    (case (car signature)
      ((lambda case-lambda)
       (values (map
                (lambda (param-sig)
                  (render-subsig (symbol->string (car param-sig))
                                 (cadr param-sig)))
                (index-entry-param-signatures index-entry))
               '()
               (render-procedure-signature (index-entry-name index-entry)
                                           link
                                           signature
                                           #f)))
      ((syntax-rules)
       (values (map
                (lambda (param-sig)
                  (render-subsig (symbol->string (car param-sig))
                                 (cadr param-sig)))
                (filter
                 (lambda (s)
                   (not (symbol=? 'pattern (caadr s))))
                 (index-entry-param-signatures index-entry)))
               (let ((literals (cadr signature)))
                 (map
                  (lambda (param)
                    ;; param is like: (name (syntax template ...))
                    (define name (symbol->string (car param)))
                    (define templates (cdr (cadr param)))
                    (make-syntax-rule name
                                      (map
                                       (lambda (rule)
                                         (render-syntax-signature-signature literals rule))
                                       templates)))
                  (filter
                   (lambda (s)
                     (symbol=? 'pattern (caadr s)))
                   (index-entry-param-signatures index-entry))))
               (render-syntax-signature (index-entry-name index-entry)
                                        link
                                        signature)))
      ((value)
       (values '()
               '()
               (render-value-signature (index-entry-name index-entry)
                                       link
                                       signature)))))
  (define spec-values
    (render-spec-values (index-entry-spec-values index-entry)))
  (define description (index-entry-description index-entry))
  (make-result-item
   (index-entry-name index-entry)
   signature-sd
   param-signatures
   subsyntax-signatures
   (map
    (lambda (tag)
      (make-ui-link (string-append (baseurl) "search?" (encode-query `((tag . ,(symbol->string tag)))))
                    tag))
    (index-entry-tags index-entry))
   (let ((lib (index-entry-lib index-entry)))
     (make-ui-link (string-append (baseurl) "search?" (encode-query `((lib . ,lib))))
                   lib))
   (index-entry-parameterized-by index-entry)
   spec-values
   description))

(define (render-spec-values spec-values)
  (map
   (lambda (block)
     (define vals
       (map
        (lambda (val)
          (make-spec-value-fieldentry (car val) (cadr val)))
        (cdr block)))
     (make-spec-value-fieldblock (car block) vals))
   spec-values))

(define (render-syntax-signature-signature literals rule)
  (define (term-handler term)
    (cond
     ((find (lambda (el) (equal? term el)) literals)
      (make-sexpr-el #f (symbol->string term) "bright-syntax" #f #f))
     ((string? term)
      (make-sexpr-el #f term "muted" #f #f))
     ((or (equal? '... term)
          (boolean? term)
          (number? term))
      (make-sexpr-el #f (write* term) "muted" #f #f))
     (else
      (make-sexpr-el #f #f "sexpr-flex muted" #f
                     (list (make-sexpr-el "&#x27E8" #f #f #f #f)
                           (make-sexpr-el #f (symbol->string term) #f #f #f)
                           (make-sexpr-el "&#x27E9" #f #f #f #f))))))
  (make-sexpr-el #f #f "sexpr-flex" #f (render-sexpr (cons rule '()) term-handler 1)))

(define suppress-make-link (make-parameter #f))
(define singlepage-link-maker (make-parameter #f))
(define baseurl (make-parameter ""))

(define (make-link type param?)
  (cond
   ((suppress-make-link) #f)
   (param? (string-append (baseurl) "search?" (encode-query `((return . ,(symbol->string type))))))
   (else (string-append (baseurl) "search?" (encode-query `((param . ,(symbol->string type))))))))

(define (render-syntax-signature name link signature)
  (define rules
    (map
     (lambda (r)
       `((,name ,@(cdar r)) ,@(cdr r)))
     (cddr signature)))
  (define literals (cadr signature))
  (define (term-handler term)
    (cond
     ((equal? name term)
       (make-sexpr-el #f (symbol->string term) "bright-syntax" link #f))
     ((find (lambda (el) (equal? term el)) literals)
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
             (render-return-type (cadr rule) #f)))
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
   ((or (symbol? sexpr)
        (boolean? sexpr)
        (string? sexpr)
        (number? sexpr))
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

(define (render-return-type returns sub?)
  (define (do-render-return-type value)
    (cond
     ((or (equal? value '...)
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
                                                  (make-sexpr-el #f #f "sexpr-flex" #f (list wide-spacer-sd (do-render-return-type e))))
                                                (cdr value))
                                             ,paren-close-sd)))))
  (if (equal? 'undefined returns)
      (list)
      (list (make-sexpr-el #f #f "sexpr-flex" #f
                           (list spacer-sd
                                 long-arrow-sd
                                 spacer-sd
                                 (do-render-return-type returns))))))

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

(define (render-procedure-signature/param param sub?)
  (if (list? param)
      `(,sq-paren-open-sd
        ,(render-param-type (car param) sub?)
        ,spacer-sd
        ,(make-sexpr-el #f (cadr param) "muted" #f #f)
        ,sq-paren-close-sd)
      `(,(make-sexpr-el #f (symbol->string param) "muted" #f #f))))

(define (render-procedure-signature name link sig sub?)
  (cond
   ((symbol=? 'lambda (car sig))
    (render-procedure-single-signature name link (cadr sig) (caddr sig) sub?))
   ((symbol=? 'case-lambda (car sig))
    (make-sexpr-el #f #f "sexpr-flex-col" #f
                   (map
                    (lambda (e)
                      (render-procedure-single-signature name link (car e) (cadr e) sub?))
                    (cdr sig))))
   (else (error "Wrong signature"))))

(define (render-procedure-single-signature name link params return sub?)
  (define name-sd (make-sexpr-el #f name (if sub? "muted-name" "bright-name") link #f))
  (define (render-params-block params)
    (let loop ((params params)
               (last (null? (cdr params)))
               (result '()))
      (define param (car params))
      (define sd
        (make-sexpr-el #f #f "sexpr-flex" #f
                       `(,wide-spacer-sd
                         ,@(render-procedure-signature/param param sub?)
                         ,@(if last (list paren-close-sd) (list)))))
      (if last
          (reverse (cons sd result))
          (loop (cdr params)
                (null? (cddr params))
                (cons sd result)))))
  (define return-sd (render-return-type return sub?))
  (if (null? params)
      (make-sexpr-el #f #f "sexpr-flex" #f
                     `(,paren-open-sd
                       ,name-sd
                       ,paren-close-sd
                       ,@return-sd))
      (make-sexpr-el #f #f "sexpr-flex" #f
                     `(,paren-open-sd
                       ,name-sd
                       ,(make-sexpr-el #f #f "sexpr-flex sexpr-shrink" #f (render-params-block params))
                       ,@return-sd))))

(define (render-subsig/lambda sig)
  (parameterize ((suppress-make-link #t))
    (render-procedure-signature "\x03BB" #f sig #t)))

(define (render-subsig/alist sig)
  (make-sexpr-el #f #f "sexpr-flex" #f
                 `(,quote-sd
                   ,paren-open-sd
                   ,paren-open-sd
                   ,@(render-procedure-signature/param (cadr sig) #f)
                   ,spacer-sd
                   ,period-sd
                   ,spacer-sd
                   ,@(render-procedure-signature/param (caddr sig) #f)
                   ,paren-close-sd
                   ,spacer-sd
                   ,(make-sexpr-el #f "..." "muted" #f #f)
                   ,paren-close-sd)))

(define (render-subsig/list sig)
  (make-sexpr-el #f #f "sexpr-flex" #f
                 `(,quote-sd
                   ,paren-open-sd
                   ,@(render-procedure-signature/param (cadr sig) #f)
                   ,spacer-sd
                   ,(make-sexpr-el #f "..." "muted" #f #f)
                   ,paren-close-sd)))

(define (render-subsig/value sig)
  (render-param-type (cadr sig) #f))

(define (render-subsig/vector sig)
  (make-sexpr-el #f #f "sexpr-flex" #f
                 `(,hash-sd
                   ,paren-open-sd
                   ,@(render-procedure-signature/param (cadr sig) #f)
                   ,spacer-sd
                   ,(make-sexpr-el #f "..." "muted" #f #f)
                   ,paren-close-sd)))

(define (render-subsig name sig)
  (define name-sd (make-sexpr-el #f name "muted-name"  #f #f))
  (define value
    (case (car sig)
      ((lambda) (render-subsig/lambda sig))
      ((alist) (render-subsig/alist sig))
      ((list) (render-subsig/list sig))
      ((vector) (render-subsig/vector sig))
      ((value) (render-subsig/value sig))
      (else (raise (string-append "Unexpected signature: " (write* sig))))))
  (make-sexpr-el #f #f "sexpr-flex" #f `(,name-sd  ,spacer-sd ,long-arrow-sd ,spacer-sd ,value)))

(define (render-value-signature name link sig)
  (make-sexpr-el #f #f "sexpr-flex" #f
                 (list (make-sexpr-el #f name "bright-name" link #f)
                       spacer-sd
                       long-arrow-sd
                       spacer-sd
                       (make-sexpr-el #f (symbol->string (cadr sig)) "bright-type" (make-link (cadr sig) #f) #f))))


(define (render-search-page settings filtersets filterset page page-size query libs tags param-types return-types parameterized-by search-result)
  (values
     "search"
     (make-page
      (make-page-head "Search")
      (make-navigation (make-mustache-nav-data 'search filtersets (deploy-setting/enable-user-settings settings)))
      (render-search-result filterset page page-size query libs tags param-types return-types parameterized-by search-result))))

(define (render-single-item-page settings filtersets filterset-code filterset-name entry)
  (values
     "single"
     (make-page
      (make-page-head "Search")
      (make-navigation (make-mustache-nav-data 'search filtersets (deploy-setting/enable-user-settings settings)))
      (make-single-item-mustache filterset-code filterset-name (render-index-entry entry) (string-append (baseurl) "search")))))
