(
 
 (set
   (lambda ((comparator? comparator) element ...) set?)
   (pure))
 
 (set-unfold
   (lambda ((comparator? comparator) (procedure? stop?) (procedure? mapper) (procedure? successor) seed) set?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) *))
    (successor (lambda (seed) *))))
 
 (set?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (set-contains?
   (lambda ((set? set) element) boolean?)
   (pure))
 
 (set-empty?
   (lambda ((set? set)) boolean?)
   (pure))
 
 (set-disjoint?
   (lambda ((set? set1) (set? set2)) boolean?)
   (pure))
 
 (set-member
   (lambda ((set? set) element default) *)
   (pure))
 
 (set-element-comparator
   (lambda ((set? set)) comparator?)
   (pure))
 
 (set-adjoin
   (lambda ((set? set) element ...) set?)
   (pure))
 
 (set-adjoin!
   (lambda ((set? set) element ...) set?)
   ())
 
 (set-replace
   (lambda ((set? set) element) set?)
   (pure))
 
 (set-replace!
   (lambda ((set? set) element) set?)
   ())
 
 (set-delete
   (lambda ((set? set) element ...) set?)
   (pure))
 
 (set-delete!
   (lambda ((set? set) element ...) set?)
   ())
 
 (set-delete-all
   (lambda ((set? set) (list? elements)) set?)
   (pure))
 
 (set-delete-all!
   (lambda ((set? set) (list? elements)) set?)
   ())
 
 (set-search!
   (lambda ((set? set) element (procedure? failure) (procedure? success)) (values set? *))
   ()
   ((failure (lambda ((procedure? insert) (procedure? ignore)) *))
    (insert (lambda (obj) *))
    (ignore (lambda (obj) *))
    (success (lambda (element (procedure? update) (procedure? remove)) *))
    (update (lambda (new-element obj) *))
    (remove (lambda (obj) *))))
 
 (set-size
   (lambda ((set? set)) integer?)
   (pure))
 
 (set-find
   (lambda ((procedure? predicate) (set? set) (procedure? failure)) *)
   (pure)
   ((predicate (lambda (obj) boolean?))
    (failure (lambda () *))))
 
 (set-count
   (lambda ((procedure? predicate) (set? set)) integer?)
   (pure)
   ((predicate (lambda (obj) boolean?))))
 
 (set-any?
   (lambda ((procedure? predicate) (set? set)) boolean?)
   (pure)
   ((predicate (lambda (obj) boolean?))))
 
 (set-every?
   (lambda ((procedure? predicate) (set? set)) boolean?)
   (pure)
   ((predicate (lambda (obj) boolean?))))
 
 (set-map
   (lambda ((comparator? comparator) (procedure? proc) (set? set)) *)
   (pure)
   ((proc (lambda (obj) *))))
 
 (set-for-each
   (lambda ((procedure? proc) (set? set)) undefined)
   ()
   ((proc (lambda (obj) undefined))))
 
 (set-fold
   (lambda ((procedure? proc) nil (set? set)) *)
   (pure)
   ((proc (lambda (obj state) *))))
 
 (set-filter
   (lambda ((procedure? predicate) (set? set)) set?)
   (pure)
   ((predicate (lambda (obj) *))))
 
 (set-filter!
   (lambda ((procedure? predicate) (set? set)) set?)
   ()
   ((predicate (lambda (obj) *))))
 
 (set-remove
   (lambda ((procedure? predicate) (set? set)) set?)
   (pure)
   ((predicate (lambda (obj) *))))
 
 (set-remove!
   (lambda ((procedure? predicate) (set? set)) set?)
   ()
   ((predicate (lambda (obj) *))))
 
 (set-partition
   (lambda ((procedure? predicate) (set? set)) (values set? set?))
   (pure)
   ((predicate (lambda (obj) *))))
 
 (set-partition!
   (lambda ((procedure? predicate) (set? set)) (values set? set?))
   ()
   ((predicate (lambda (obj) *))))
 
 (set-copy
   (lambda ((set? set)) set?)
   (pure))
 
 (set->list
   (lambda ((set? set)) list?)
   ())
 
 (list->set
   (lambda ((comparator? comparator) (list? list)) set?)
   (pure))
 
 (list->set!
   (lambda ((set? set) (list? list)) set?)
   ())
 
 (set=?
   (lambda ((set? set1) (set? set2) ...) boolean?)
   (pure))
 
 (set<?
   (lambda ((set? set1) (set? set2) ...) boolean?)
   (pure))
 
 (set>?
   (lambda ((set? set1) (set? set2) ...) boolean?)
   (pure))
 
 (set<=?
   (lambda ((set? set1) (set? set2) ...) boolean?)
   (pure))
 
 (set>=?
   (lambda ((set? set1) (set? set2) ...) boolean?)
   (pure))
 
 (set-union
   (lambda ((set? set1) (set? set2) ...) set?)
   (pure))
 
 (set-intersection
   (lambda ((set? set1) (set? set2) ...) set?)
   (pure))
 
 (set-difference
   (lambda ((set? set1) (set? set2) ...) set?)
   (pure))
 
 (set-xor
   (lambda ((set? set1) (set? set2)) set?)
   (pure))
 
 (set-union!
   (lambda ((set? set1) (set? set2) ...) set?)
   ())
 
 (set-intersection!
   (lambda ((set? set1) (set? set2) ...) set?)
   ())
 
 (set-difference!
   (lambda ((set? set1) (set? set2) ...) set?)
   ())
 
 (set-xor!
   (lambda ((set? set1) (set? set2)) set?)
   ())
 
 ;;;;;
 
 
 (bag
   (lambda ((comparator? comparator) element ...) bag?)
   (pure))
 
 (bag-unfold
   (lambda ((comparator? comparator) (procedure? stop?) (procedure? mapper) (procedure? successor) seed) bag?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) *))
    (successor (lambda (seed) *))))
 
 (bag?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (bag-contains?
   (lambda ((bag? bag) element) boolean?)
   (pure))
 
 (bag-empty?
   (lambda ((bag? bag)) boolean?)
   (pure))
 
 (bag-disjoint?
   (lambda ((bag? bag1) (bag? bag2)) boolean?)
   (pure))
 
 (bag-member
   (lambda ((bag? bag) element default) *)
   (pure))
 
 (bag-element-comparator
   (lambda ((bag? bag)) comparator?)
   (pure))
 
 (bag-adjoin
   (lambda ((bag? bag) element ...) bag?)
   (pure))
 
 (bag-adjoin!
   (lambda ((bag? bag) element ...) bag?)
   ())
 
 (bag-replace
   (lambda ((bag? bag) element) bag?)
   (pure))
 
 (bag-replace!
   (lambda ((bag? bag) element) bag?)
   ())
 
 (bag-delete
   (lambda ((bag? bag) element ...) bag?)
   (pure))
 
 (bag-delete!
   (lambda ((bag? bag) element ...) bag?)
   ())
 
 (bag-delete-all
   (lambda ((bag? bag) (list? elements)) bag?)
   (pure))
 
 (bag-delete-all!
   (lambda ((bag? bag) (list? elements)) bag?)
   ())
 
 (bag-search!
   (lambda ((bag? bag) element (procedure? failure) (procedure? success)) (values bag? *))
   ()
   ((failure (lambda ((procedure? insert) (procedure? ignore)) *))
    (insert (lambda (obj) *))
    (ignore (lambda (obj) *))
    (success (lambda (element (procedure? update) (procedure? remove)) *))
    (update (lambda (new-element obj) *))
    (remove (lambda (obj) *))))
 
 (bag-size
   (lambda ((bag? bag)) integer?)
   (pure))
 
 (bag-find
   (lambda ((procedure? predicate) (bag? bag) (procedure? failure)) *)
   (pure)
   ((predicate (lambda (obj) boolean?))
    (failure (lambda () *))))
 
 (bag-count
   (lambda ((procedure? predicate) (bag? bag)) integer?)
   (pure)
   ((predicate (lambda (obj) boolean?))))
 
 (bag-any?
   (lambda ((procedure? predicate) (bag? bag)) boolean?)
   (pure)
   ((predicate (lambda (obj) boolean?))))
 
 (bag-every?
   (lambda ((procedure? predicate) (bag? bag)) boolean?)
   (pure)
   ((predicate (lambda (obj) boolean?))))
 
 (bag-map
   (lambda ((comparator? comparator) (procedure? proc) (bag? bag)) *)
   (pure)
   ((proc (lambda (obj) *))))
 
 (bag-for-each
   (lambda ((procedure? proc) (bag? bag)) undefined)
   ()
   ((proc (lambda (obj) undefined))))
 
 (bag-fold
   (lambda ((procedure? proc) nil (bag? bag)) *)
   (pure)
   ((proc (lambda (obj state) *))))
 
 (bag-filter
   (lambda ((procedure? predicate) (bag? bag)) bag?)
   (pure)
   ((predicate (lambda (obj) *))))
 
 (bag-filter!
   (lambda ((procedure? predicate) (bag? bag)) bag?)
   ()
   ((predicate (lambda (obj) *))))
 
 (bag-remove
   (lambda ((procedure? predicate) (bag? bag)) bag?)
   (pure)
   ((predicate (lambda (obj) *))))
 
 (bag-remove!
   (lambda ((procedure? predicate) (bag? bag)) bag?)
   ()
   ((predicate (lambda (obj) *))))
 
 (bag-partition
   (lambda ((procedure? predicate) (bag? bag)) (values bag? bag?))
   (pure)
   ((predicate (lambda (obj) *))))
 
 (bag-partition!
   (lambda ((procedure? predicate) (bag? bag)) (values bag? bag?))
   ()
   ((predicate (lambda (obj) *))))
 
 (bag-copy
   (lambda ((bag? bag)) bag?)
   (pure))
 
 (bag->list
   (lambda ((bag? bag)) list?)
   ())
 
 (list->bag
   (lambda ((comparator? comparator) (list? list)) bag?)
   (pure))
 
 (list->bag!
   (lambda ((bag? bag) (list? list)) bag?)
   ())
 
 (bag=?
   (lambda ((bag? bag1) (bag? bag2) ...) boolean?)
   (pure))
 
 (bag<?
   (lambda ((bag? bag1) (bag? bag2) ...) boolean?)
   (pure))
 
 (bag>?
   (lambda ((bag? bag1) (bag? bag2) ...) boolean?)
   (pure))
 
 (bag<=?
   (lambda ((bag? bag1) (bag? bag2) ...) boolean?)
   (pure))
 
 (bag>=?
   (lambda ((bag? bag1) (bag? bag2) ...) boolean?)
   (pure))
 
 (bag-union
   (lambda ((bag? bag1) (bag? bag2) ...) bag?)
   (pure))
 
 (bag-intersection
   (lambda ((bag? bag1) (bag? bag2) ...) bag?)
   (pure))
 
 (bag-difference
   (lambda ((bag? bag1) (bag? bag2) ...) bag?)
   (pure))
 
 (bag-xor
   (lambda ((bag? bag1) (bag? bag2)) bag?)
   (pure))
 
 (bag-union!
   (lambda ((bag? bag1) (bag? bag2) ...) bag?)
   ())
 
 (bag-intersection!
   (lambda ((bag? bag1) (bag? bag2) ...) bag?)
   ())
 
 (bag-difference!
   (lambda ((bag? bag1) (bag? bag2) ...) bag?)
   ())
 
 (bag-xor!
   (lambda ((bag? bag1) (bag? bag2)) bag?)
   ())
 
 ;;;;;;;;;;
 
 (bag-sum
   (lambda ((bag? bag1) (bag? bag2) ...) bag?)
   (pure))
 
 (bag-sum!
   (lambda ((bag? bag1) (bag? bag2) ...) bag?)
   ())
 
 (bag-product
   (lambda ((integer? n) (bag? bag)) bag?)
   (pure))
 
 (bag-product!
   (lambda ((integer? n) (bag? bag)) bag?)
   ())
 
 (bag-unique-size
   (lambda ((bag? bag)) integer?)
   (pure))
 
 (bag-element-count
   (lambda ((bag? bag) element) integer?)
   (pure))

 (bag-for-each-unique
   (lambda ((procedure? proc) (bag? bag)) undefined)
   ()
   ((proc (lambda (obj) undefined))))
 
 (bag-fold-unique
   (lambda ((procedure? proc) nil (bag? bag)) *)
   (pure)
   ((proc (lambda (obj state) *))))
 
 (bag-increment!
   (lambda ((bag? bag) element count) bag?)
   ())
 
 (bag-decrement!
   (lambda ((bag? bag) element count) bag?)
   ())
 
 (bag->set
   (lambda ((bag? bag)) set?)
   (pure))
 
 (set->bag
   (lambda ((set? set)) bag?)
   (pure))
 
 (set->bag!
   (lambda ((set? set)) bag?)
   ())
 
 (bag->alist
   (lambda ((bag? bag)) alist?))
 
 (alist->bag
   (lambda ((comparator? comparator) (alist? alist)) bag?)
   (pure))
 
 (set-comparator
   (value comparator?))
 
 (bag-comparator
   (value comparator?))
 
 )
