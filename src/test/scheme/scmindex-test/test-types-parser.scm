(define funcs
  (read-specs "testtypes/index.scm"))

(define (test-flat-type expect actual)
  (test-equal (length expect) (length actual))
  (for-each
    (lambda (e)
      (test-assert (member e actual)))
    expect))

(test-equal 5 (length funcs))

(let* ((f (list-ref funcs 0))
       (f* (func->json f)))
 (test-equal '(scheme base) (func-lib f))
 (test-equal "(scheme base)" (cdr (assoc 'lib f*)))
 (test-equal 'eq? (func-name f))
 (test-equal "eq?" (cdr (assoc 'name f*)))
 (test-equal '(a b) (func-param-names f))
 (test-equal #("a" "b") (cdr (assoc 'param_names f*)))
 (test-equal '(boolean?) (func-return-types f))
 (test-equal #("boolean?") (cdr (assoc 'return_types f*)))
 (test-equal '() (func-param-types f))
 (test-equal #() (cdr (assoc 'param_types f*)))
 (test-equal '(pure) (func-tags f))
 (test-equal #("pure") (cdr (assoc 'tags f*)))
 (test-equal '() (func-supertypes f))
 (test-equal #() (cdr (assoc 'super_types f*))))

(let ((f (list-ref funcs 1)))
 (test-equal 'map (func-name f))
 (test-equal '(procedure? list?) (func-param-types f))
 (test-equal '(mapper lst) (func-param-names f))
 (test-equal '((mapper (lambda (el) *))) (func-param-signatures f)))

(let ((f (list-ref funcs 2)))
 (test-equal 'integer? (func-name f))
 (test-equal '(exact?) (func-supertypes f)))

(let-values
  (((supertype-map subtype-map)
    (make-type-maps funcs)))
  
  (test-equal '(exact? . (integer?)) (assoc 'exact? subtype-map))
  (test-equal '(exact? . (number?)) (assoc 'exact? supertype-map))
  
  (test-flat-type '(exact? number?) (flatten-type supertype-map '(exact?)))
  (test-flat-type '(exact? integer?) (flatten-type subtype-map '(exact?)))
  
  (test-flat-type '(integer? number? exact?) (flatten-type supertype-map '(integer?)))
  (test-flat-type '(integer?) (flatten-type subtype-map '(integer?)))
  
  (test-flat-type '(number?) (flatten-type supertype-map '(number?)))
  (test-flat-type '(integer? number? exact?) (flatten-type subtype-map '(number?))))

(let ((payload (list->vector (map func->json funcs)))
      (content (open-output-string)))
  (json-write payload content)
  (test-assert (get-output-string content)))
