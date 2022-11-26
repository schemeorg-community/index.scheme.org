;; creates mustache lookup from a given alist mapping
(define (make-lookup pred alist)
  (lambda (obj name found not-found)
    (cond
     ((not (pred obj)) (not-found))
     ((assoc (string->symbol name) alist)
      =>
      (lambda (e)
        (found ((cdr e) obj))))
     (else (not-found)))))

;; same as define-record-type, but additionally also creates a lookup binding
(define-syntax define-mustache-record-type
  (syntax-rules ()
    ((_ type lookup constructor pred (field getter) ...)
     (begin
       (define-record-type type constructor pred (field getter) ...)
       (define lookup
         (let ((getter-map (list (cons (quote field) getter) ...)))
           (make-lookup pred getter-map)))))))

;; top level page data wrapper
(define-mustache-record-type <page>
  page-lookup
  (make-page head navigation body)
  page?
  (head page-head)
  (navigation page-navigation)
  (body page-body))

;; data used in <head>
(define-mustache-record-type <page-head>
  page-head-lookup
  (make-page-head title)
  page-head?
  (title page-head-title))

;; navigation data
(define-mustache-record-type <navigation>
  navigation-lookup
  (make-navigation items)
  navigation?
  (items navigation-items))

;;navigation entry data
(define-mustache-record-type <nav-item>
  nav-item-lookup
  (make-nav-item label icon-cls link active? items)
  nav-item?
  (label nav-item-label)
  (icon-cls nav-item-icon-cls)
  (link nav-item-link)
  (active? nav-item-active?)
  (items nav-item-items))

;; data representing a fragment of sexpr code, forming a tree
(define-mustache-record-type <sexpr-el>
  sexpr-el-lookup
  (make-sexpr-el html text class link sub-exprs)
  sexpr-el?
  (html sexpr-el-html)
  (text sexpr-el-text) ;; unlike html field, text is properly escaped
  (class sexpr-el-class) ;; css class to be attached
  (link sexpr-el-link) ;; anchro href link
  (sub-exprs sexpr-el-sub-exprs) ;; children expressions
  )

(define (nav-item-extra-lookup obj name found not-found)
  (cond
   ((not (nav-item? obj)) (not-found))
   ((equal? "has-items?" name) (found (not (null? (nav-item-items obj)))))
   (else (not-found))))

(define (make-mustache-nav-data page filtersets show-settings?)
  (filter
   values
   (list
    (make-nav-item "Home" "icon-home3" "/" (equal? page 'index) '())
    (make-nav-item "Search" "icon-search" #f (equal? page 'search)
                   (map
                    (lambda (f)
                      (make-nav-item (cdr f) "" (string-append "/filterset/" (car f) "/search") #f '()))
                    filtersets))
    (if show-settings? (make-nav-item "Settings" "icon-cog" "/settings" (equal? page 'settings) '()) #f)
    (make-nav-item "Documentation" "icon-file-text2" "/README.html" (equal? page 'docs) '()))))

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

(define spacer-sd
  (make-sexpr-el "" #f "spacer" #f #f))

(define wide-spacer-sd
  (make-sexpr-el "" #f "wide-spacer" #f #f))

(define quote-sd
  (make-sexpr-el "'" #f "muted" #f #f))

(define hash-sd
  (make-sexpr-el "#" #f "muted" #f #f))

(define period-sd
  (make-sexpr-el "." #f "muted" #f #f))

(define paren-open-sd
  (make-sexpr-el #f "(" "muted" #f #f))

(define paren-close-sd
  (make-sexpr-el #f ")" "muted" #f #f))

(define sq-paren-open-sd
  (make-sexpr-el #f "[" "muted" #f #f))

(define sq-paren-close-sd
  (make-sexpr-el #f "]" "muted" #f #f))

(define long-arrow-sd
  (make-sexpr-el "&DoubleLongRightArrow;" #f "muted" #f #f))

(define slash (make-sexpr-el #f "/" "muted-type" #f #f))
