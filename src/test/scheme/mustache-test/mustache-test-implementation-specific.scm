(define-record-type <foo> (foo bar) foo? (bar foo-bar))

(define (foo-lookup obj name found not-found)
  (cond
    ((not (foo? obj)) (not-found))
    ((string=? "bar" name) (found (foo-bar obj)))
    (else (not-found))))

(define alist+foo (compose-lookups alist-lookup foo-lookup))

(define (write-foo obj out)
  (write-string "(foo " out)
  (display (foo-bar obj) out)
  (write-string ")" out))

(define-record-type <num-lst> (num-lst count) num-lst? (count num-lst-count))
(define num-lst-collection
  (collection
    num-lst?
    (lambda (obj) (= 0 (num-lst-count obj)))
    (lambda (proc obj)
      (define target (num-lst-count obj))
      (let loop ((i 0))
       (when (< i target)
         (begin
           (proc i)
           (loop (+ 1 i))))))))

(parameterize
  ((current-writer (lambda (obj out)
                     (cond
                       ((not obj) #t)
                       ((foo? obj) (write-foo obj out))
                       (else (display obj out))))))
  (test-mustache "Custom writer"
                 `((obj . ,(foo "baz")))
                 "Test {{obj}}"
                 "Test (foo baz)"))

(parameterize
  ((current-lookup alist+foo))
  (test-mustache "Custom lookup"
                 `((a . ((bar . "baz1")))
                   (b . ,(foo "baz2")))
                 "{{a.bar}}, {{b.bar}}"
                 "baz1, baz2"))

(parameterize
  ((current-collection num-lst-collection))
  (test-mustache "Custom collection"
                 `((a . ,(num-lst 3)))
                 "{{#a}}{{.}};{{/a}}"
                 "0;1;2;"))

(parameterize
  ((current-collection list-collection)
   (current-lookup foo-lookup))
  (test-mustache "List collection"
                 (foo '(0 1 2))
                 "{{#bar}}{{.}};{{/bar}}"
                 "0;1;2;"))

#;(parameterize
  ((current-collection stream-collection)
   (current-lookup foo-lookup))
  (test-mustache "Stream collection"
                 (foo (list->stream '(0 1 2)))
                 "{{#bar}}{{.}};{{/bar}}"
                 "0;1;2;"))
