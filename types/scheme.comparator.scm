(
 
 (comparator?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (comparator-ordered?
   (lambda ((comparator? comparator)) boolean?)
   (pure))
 
 (comparatory-hashable?
   (lambda ((comparator? comparator)) boolean?)
   (pure))
 
 (make-comparator
   (lambda ((procedure? type-test) (procedure? equality) ((or procedure? #f) ordering) ((or procedure? #f) hash)) comparator?)
   (pure)
   ((type-test (lambda (obj) boolean?))
    (equality (lambda (obj1 obj2) boolean?))
    (ordering (lambda (obj1 obj2) boolean?))
    (hash (lambda (obj) integer?))))
 
 (make-pair-comparator
   (lambda ((comparator? car-comparator) (comparator? cdr-comparator)) comparator?)
   (pure))
 
 (make-list-comparator
   (lambda ((comparator? element-comparator) (procedure? type-test) (procedure? empty?) (procedure? head) (procedure? tail)) comparator?)
   (pure)
   ((type-test (lambda (obj) boolean?))
    (empty? (lambda (obj) boolean?))
    (head (lambda (obj) *))
    (tail (lambda (obj) *))))
 
 (make-vector-comparator
   (lambda ((comparator? element-comparator) (procedure? type-test) (procedure? length) (procedure? ref)) comparator?)
   (pure)
   ((type-test (lambda (obj) boolean?))
    (length (lambda (obj) integer?))
    (ref (lambda (vec (integer? index)) *))))
 
 (make-eq-comparator
   (lambda () comparator?)
   (pure))
 
 (make-eqv-comparator
   (lambda () comparator?)
   (pure))
 
 (make-equal-comparator
   (lambda () comparator?)
   (pure))
 
 (boolean-hash
   (lambda (obj) integer?)
   (pure))
 
 (char-hash
   (lambda (obj) integer?)
   (pure))
 
 (char-ci-hash
   (lambda (obj) integer?)
   (pure))
 
 (string-hash
   (lambda (obj) integer?)
   (pure))
 
 (string-ci-hash
   (lambda (obj) integer?)
   (pure))
 
 (symbol-hash
   (lambda (obj) integer?)
   (pure))
 
 (number-hash
   (lambda (obj) integer?)
   (pure))
 
 (hash-bound
   (lambda () integer?)
   (syntax))
 
 (hash-salt
   (lambda () integer?)
   (syntax))
 
 (make-default-comparator
   (lambda () comparator?)
   ())

 (default-hash
   (lambda (obj) integer?)
   ())
 
 (comparator-register-default!
   (lambda ((comparator? comparator)) undefined))
 
 (comparator-type-test-predicate
   (lambda ((comparator? comparator)) procedure?)
   (pure)
   ((return (lambda (obj) boolean?))))
 
 (comparator-equality-predicate
   (lambda ((comparator? comparator)) procedure?)
   (pure)
   ((return (lambda (obj1 obj2) boolean?))))
 
 (comparator-ordering-predicate
   (lambda ((comparator? comparator)) (or #f procedure?))
   (pure)
   ((return (lambda (obj1 obj2) boolean?))))
 
 (comparator-hash-function
   (lambda ((comparator? comparator)) (or #f procedure?))
   (pure)
   ((return (lambda (obj) integer?))))
 
 (comparator-test-type
   (lambda ((comparator? comparator) obj) boolean?)
   (pure))
 
 (comparator-check-type
   (lambda ((comparator? comparator) obj) boolean?)
   ())
 
 (comparator-hash
   (lambda ((comparator? comparator) obj) integer?)
   (pure))
 
 (=?
   (lambda ((comparator? comparator) object1 object2 object3 ...) boolean?)
   (pure))
 
 (<?
   (lambda ((comparator? comparator) object1 object2 object3 ...) boolean?)
   (pure))
 
 (>?
   (lambda ((comparator? comparator) object1 object2 object3 ...) boolean?)
   (pure))
 
 (<=?
   (lambda ((comparator? comparator) object1 object2 object3 ...) boolean?)
   (pure))
 
 (>=?
   (lambda ((comparator? comparator) object1 object2 object3 ...) boolean?)
   (pure))
 
 (comparator-if<=>
   (syntax-rules ()
     ((_ comparator object1 object2 less-than equal-to greater-than))
     ((_ object1 object2 less-than equal-to greater-than))))
 
 )
