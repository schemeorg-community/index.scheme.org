(test-group
  "Test function general"
  (define spec-raw
    `(integer?
        (lambda (obj) boolean?)
        (pure predicate)
        ()
        (number?)))
  (define f (car (read-spec '(test lib) (list spec-raw))))
  (define f* (func->json f))
  (test-equal '(test lib) (func-lib f))
  (test-equal "(test lib)" (cdr (assoc 'lib f*)))
  (test-equal 'integer? (func-name f))
  (test-equal "integer?" (cdr (assoc 'name f*)))
  (test-equal '(obj) (func-param-names f))
  (test-equal #("obj") (cdr (assoc 'param_names f*)))
  (test-equal '(boolean?) (func-return-types f))
  (test-equal #("boolean?") (cdr (assoc 'return_types f*)))
  (test-equal '() (func-param-types f))
  (test-equal #() (cdr (assoc 'param_types f*)))
  (test-equal '(pure predicate) (func-tags f))
  (test-equal #("pure" "predicate") (cdr (assoc 'tags f*)))
  (test-equal '(number?) (func-supertypes f))
  (test-equal #("number?") (cdr (assoc 'super_types f*))))

(test-group
  "Test function parameter name parsing"
  (define specs
    (read-spec '(test lib)
               `(
                 (f1
                   (lambda () *))
                 (f2
                   (lambda (obj1 obj2 ...) *))
                 (f3
                   (lambda ((string? str) ... (#f obj2)) *))
                 )))
  (test-equal '() (func-param-names (list-ref specs 0)))
  (test-equal '(obj1 obj2) (func-param-names (list-ref specs 1)))
  (test-equal '(str obj2) (func-param-names (list-ref specs 2))))

(test-group
  "Test function parameter type parsing"
  (define specs
    (read-spec '(test lib)
               `(
                 (f1
                   (lambda () *))
                 (f2
                   (lambda (obj) *))
                 (f3
                   (lambda ((string? str) (#f int)) *))
                 (f4
                   (lambda (((or #f string?) str)) *)))))
  (test-equal '() (func-param-types (list-ref specs 0)))
  (test-equal '() (func-param-types (list-ref specs 1)))
  (test-equal '(string?) (func-param-types (list-ref specs 2)))
  (test-equal '(string?) (func-param-types (list-ref specs 3))))

(test-group
  "Test function return type parsing"
  (define specs
    (read-spec '(test lib)
                `(
                  (f1
                    (lambda () *))
                  (f2
                    (lambda () undefined))
                  (f3
                    (lambda () string?))
                  (f4
                    (lambda () (or integer? string?)))
                  (f5
                    (lambda () (or #f string?)))
                  (f6
                    (lambda () (values integer? string? ...))))))
  (test-equal '() (func-return-types (list-ref specs 0)))
  (test-equal '() (func-return-types (list-ref specs 1)))
  (test-equal '(string?) (func-return-types (list-ref specs 2)))
  (test-equal '(integer? string?) (func-return-types (list-ref specs 3)))
  (test-equal '(string?) (func-return-types (list-ref specs 4)))
  (test-equal '(integer? string?) (func-return-types (list-ref specs 5))))

(test-group
  "Test subtyping"
  ;;
  ;;  A?  B?
  ;; /  \ |
  ;; E?  C?
  ;; |   |
  ;; F?  D?
  ;;
  (define specs
    (read-spec '(test lib)
               `(
                 (A?
                   (lambda (obj) boolean?)
                   (pure predicate)
                   ())
                 (B?
                   (lambda (obj) boolean?)
                   (pure predicate)
                   ())
                 (C?
                   (lambda (obj) boolean?)
                   (pure predicate)
                   ()
                   (A? B?))
                 (D?
                   (lambda (obj) boolean?)
                   (pure predicate)
                   ()
                   (C?))
                 (E?
                   (lambda (obj) boolean?)
                   (pure predicate)
                   ()
                   (A?))
                 (F?
                   (lambda (obj) boolean?)
                   (pure predicate)
                   ()
                   (E?)))))
      (define-values
        (supertype-map subtype-strict-map subtype-loose-map)
        (make-type-maps specs))

      ;;supertype
      (test-assert (lset= equal? '(A?) (flatten-type supertype-map '(A?))))
      (test-assert (lset= equal? '(B?) (flatten-type supertype-map '(B?))))
      (test-assert (lset= equal? '(A? B? C?) (flatten-type supertype-map '(C?))))
      (test-assert (lset= equal? '(A? B? C? D?) (flatten-type supertype-map '(D?))))
      (test-assert (lset= equal? '(A? E?) (flatten-type supertype-map '(E?))))
      (test-assert (lset= equal? '(A? E? F?) (flatten-type supertype-map '(F?))))

      ;;subtype strict
      (test-assert (lset= equal? '(A? E? F?) (flatten-type subtype-strict-map '(A?))))
      (test-assert (lset= equal? '(B?) (flatten-type subtype-strict-map '(B?))))
      (test-assert (lset= equal? '(C? D?) (flatten-type subtype-strict-map '(C?))))
      (test-assert (lset= equal? '(D?) (flatten-type subtype-strict-map '(D?))))
      (test-assert (lset= equal? '(E? F?) (flatten-type subtype-strict-map '(E?))))
      (test-assert (lset= equal? '(F?) (flatten-type subtype-strict-map '(F?))))

      ;;subtype loose
      (test-assert (lset= equal? '(A? C? D? E? F?) (flatten-type subtype-loose-map '(A?))))
      (test-assert (lset= equal? '(B? C? D?) (flatten-type subtype-loose-map '(B?))))
      (test-assert (lset= equal? '(C? D?) (flatten-type subtype-loose-map '(C?))))
      (test-assert (lset= equal? '(D?) (flatten-type subtype-loose-map '(D?))))
      (test-assert (lset= equal? '(E? F?) (flatten-type subtype-loose-map '(E?))))
      (test-assert (lset= equal? '(F?) (flatten-type subtype-loose-map '(F?)))))

(test-group
  "Test kawa mangling"
  (define specs
    (read-spec '(test lib)
               `((->char-set
                   (lambda ((char? x)) string?))

                 (|char-set:lower-case|
                   (value char-set?)))))
  ;; test workaround works to prevent kawa mangling
  (let ((f (list-ref specs 0)))
    (test-equal '|->char-set| (func-name f))
    (test-equal "->char-set" (cdr (assoc 'name (func->json f)))))

  (let ((f (list-ref specs 1)))
    (test-equal '|char-set:lower-case| (func-name f))
    (test-equal "char-set:lower-case" (cdr (assoc 'name (func->json f))))))
