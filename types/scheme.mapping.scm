(

 (mapping
   (lambda ((comparator? comparator) arg ...) mapping?)
   (pure))

 (mapping-unfold
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed (comparator? comparator)) mapping?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) (values * *)))
    (successor (lambda (seed) *))))

 (mapping?
   (lambda (obj) boolean?)
   (pure predicate))

 (mapping-contains?
   (lambda ((mapping? mapping) key) boolean?)
   (pure))

 (mapping-empty?
   (lambda ((mapping? mapping)) boolean?)
   (pure))

 (mapping-disjoint?
   (lambda ((mapping? mapping1) (mapping? mapping2)) boolean?)
   (pure))

 (mapping-ref
   (lambda ((mapping? mapping) key) *)
   (pure))

 (mapping-ref
   (lambda ((mapping? mapping) key (procedure? failure)) *)
   (pure)
   ((failure (lambda () *))))

 (mapping-ref
   (lambda ((mapping? mapping) key (procedure? failure) (procedure? success)) *)
   (pure)
   ((failure (lambda () *))
    (success (lambda (value) *))))

 (mapping-ref/default
   (lambda ((mapping? mapping) key default) *)
   (pure))

 (mapping-key-comparator
   (lambda ((mapping? mapping)) comparator?)
   (pure))

 (mapping-adjoin
   (lambda ((mapping? mapping) key1 value1 ...) mapping?)
   (pure))

 (mapping-adjoin!
   (lambda ((mapping? mapping) key1 value1 ...) mapping?)
   ())

 (mapping-set
   (lambda ((mapping? mapping) key1 value1 ...) mapping?)
   (pure))

 (mapping-set!
   (lambda ((mapping? mapping) key1 value1 ...) mapping?)
   ())

 (mapping-replace
   (lambda ((mapping? mapping) key value) mapping?)
   (pure))

 (mapping-replace!
   (lambda ((mapping? mapping) key value) mapping?)
   ())

 (mapping-delete
   (lambda ((mapping? mapping) key ...) mapping?)
   (pure))

 (mapping-delete!
   (lambda ((mapping? mapping) key ...) mapping?)
   (pure))

 (mapping-delete-all
   (lambda ((mapping? mapping) (list? keys)) mapping?)
   (pure))

 (mapping-delete-all!
   (lambda ((mapping? mapping) (list? keys)) mapping?)
   ())

 (mapping-intern
   (lambda ((mapping? mapping) key (procedure? failure)) (values mapping? *))
   (pure)
   ((failure (lambda () *))))

 (mapping-intern!
   (lambda ((mapping? mapping) key (procedure? failure)) (values mapping? *))
   ()
   ((failure (lambda () *))))

 (mapping-update
   (lambda ((mapping? mapping) key (procedure? updater)) mapping?)
   (pure)
   ((updater (lambda (value) *))))

 (mapping-update
   (lambda ((mapping? mapping) key (procedure? updater) (procedure? failure)) mapping?)
   (pure)
   ((updater (lambda (value) *))
    (failure (lambda () *))))

 (mapping-update
   (lambda ((mapping? mapping) key (procedure? updater) (procedure? failure) (procedure? success)) mapping?)
   (pure)
   ((updater (lambda (value) *))
    (failure (lambda () *))
    (success (lambda (value) *))))

 (mapping-update!
   (lambda ((mapping? mapping) key (procedure? updater)) mapping?)
   ()
   ((updater (lambda (value) *))))

 (mapping-update!
   (lambda ((mapping? mapping) key (procedure? updater) (procedure? failure)) mapping?)
   ()
   ((updater (lambda (value) *))
    (failure (lambda () *))))

 (mapping-update!
   (lambda ((mapping? mapping) key (procedure? updater) (procedure? failure) (procedure? success)) mapping?)
   ()
   ((updater (lambda (value) *))
    (failure (lambda () *))
    (success (lambda (value) *))))

 (mapping-update/default
   (lambda ((mapping? mapping) key (procedure? updater) default) mapping?)
   (pure)
   ((updater (lambda (value) *))))

 (mapping-update!/default
   (lambda ((mapping? mapping) key (procedure? updater) default) mapping?)
   ()
   ((updater (lambda (value) *))))

 (mapping-pop
   (lambda ((mapping? mapping)) (values mapping? * *))
   (pure))

 (mapping-pop
   (lambda ((mapping? mapping) (procedure? failure)) (values mapping? * *))
   (pure)
   ((failure (lambda () (values mapping? * *)))))

 (mapping-pop!
   (lambda ((mapping? mapping)) (values mapping? * *))
   ())

 (mapping-pop!
   (lambda ((mapping? mapping) (procedure? failure)) (values mapping? * *))
   ()
   ((failure (lambda () (values mapping? * *)))))

 (mapping-search
   (lambda ((mapping? mapping) key (procedure? failure) (procedure? success)) (values mapping? *))
   (pure)
   ((failure (lambda ((procedure? insert) (procedure? ignore)) *))
    (insert (lambda (value obj) *))
    (ignore (lambda (obj) *))
    (success (lambda (key value (procedure? update) (procedure? remove)) *))
    (update (lambda (new-key new-value obj) *))
    (remove (lambda (obj) *))))

 (mapping-search!
   (lambda ((mapping? mapping) key (procedure? failure) (procedure? success)) (values mapping? *))
   ()
   ((failure (lambda ((procedure? insert) (procedure? ignore)) *))
    (insert (lambda (value obj) *))
    (ignore (lambda (obj) *))
    (success (lambda (key value (procedure? update) (procedure? remove)) *))
    (update (lambda (new-key new-value obj) *))
    (remove (lambda (obj) *))))

 (mapping-size
   (lambda ((mapping? mapping)) integer?)
   (pure))

 (mapping-find
   (lambda ((procedure? predicate) (mapping? mapping) (procedure? failure)) (values * *))
   (pure)
   ((predicate (lambda (key value) boolean?))
    (failure (lambda () *))))

 (mapping-count
   (lambda ((procedure? predicate) (mapping? mapping)) integer?)
   (pure)
   ((predicate (lambda (key value) boolean?))))

 (mapping-any?
   (lambda ((procedure? predicate) (mapping? mapping)) boolean?)
   (pure)
   ((predicate (lambda (key value) boolean?))))

 (mapping-every?
   (lambda ((procedure? predicate) (mapping? mapping)) boolean?)
   (pure)
   ((predicate (lambda (key value) boolean?))))

 (mapping-keys
   (lambda ((mapping? mapping)) list?)
   (pure))

 (mapping-values
   (lambda ((mapping? mapping)) list?)
   (pure))

 (mapping-entries
   (lambda ((mapping? mapping)) (values list? list))
   (pure))

 (mapping-map
   (lambda ((procedure? proc) (comparator? comparator) (mapping? mapping)) mapping?)
   (pure)
   ((proc (lambda (key value) (values * *)))))

 (mapping-for-each
   (lambda ((procedure? proc) (comparator? comparator) (mapping? mapping)) undefined)
   ()
   ((proc (lambda (key value) undefined))))

 (mapping-fold
   (lambda ((procedure? proc) nil (mapping? mapping)) *)
   (pure)
   ((proc (lambda (key value state) *))))

 (mapping-map->list
   (lambda ((procedure? proc) (mapping? mapping)) list?)
   (pure)
   ((proc (lambda (key value) *))))

 (mapping-filter
   (lambda ((procedure? predicate) (mapping? mapping)) mapping?)
   (pure)
   ((predicate (lambda (key value) boolean?))))

 (mapping-filter!
   (lambda ((procedure? predicate) (mapping? mapping)) mapping?)
   ()
   ((predicate (lambda (key value) boolean?))))

 (mapping-remove
   (lambda ((procedure? predicate) (mapping? mapping)) mapping?)
   (pure)
   ((predicate (lambda (key value) boolean?))))

 (mapping-remove!
   (lambda ((procedure? predicate) (mapping? mapping)) mapping?)
   ()
   ((predicate (lambda (key value) boolean?))))

 (mapping-partition
   (lambda ((procedure? predicate) (mapping? mapping)) (values mapping? mapping?))
   (pure)
   ((predicate (lambda (key value) boolean?))))

 (mapping-partition!
   (lambda ((procedure? predicate) (mapping? mapping)) (values mapping? mapping?))
   ()
   ((predicate (lambda (key value) boolean?))))

 (mapping-copy
   (lambda ((mapping? mapping)) mapping?)
   (pure))

 (mapping->alist
   (lambda ((mapping? mapping)) list?)
   (pure))

 (alist->mapping
   (lambda ((comparator? comparator) (list? alist)) mapping?)
   (pure))

 (alist->mapping!
   (lambda ((mapping? mapping) (list? alist)) mapping?)
   ())

 (mapping=?
   (lambda ((comparator? comparator) (mapping? mapping1) (mapping? mapping2) ...) boolean?)
   (pure))

 (mapping<?
   (lambda ((comparator? comparator) (mapping? mapping1) (mapping? mapping2) ...) boolean?)
   (pure))

 (mapping>?
   (lambda ((comparator? comparator) (mapping? mapping1) (mapping? mapping2) ...) boolean?)
   (pure))

 (mapping<=?
   (lambda ((comparator? comparator) (mapping? mapping1) (mapping? mapping2) ...) boolean?)
   (pure))

 (mapping>=?
   (lambda ((comparator? comparator) (mapping? mapping1) (mapping? mapping2) ...) boolean?)
   (pure))

 (mapping-union
   (lambda ((mapping? mapping1) (mapping? mapping2) ...) mapping?)
   (pure))

 (mapping-intersection
   (lambda ((mapping? mapping1) (mapping? mapping2) ...) mapping?)
   (pure))

 (mapping-difference
   (lambda ((mapping? mapping1) (mapping? mapping2) ...) mapping?)
   (pure))

 (mapping-xor
   (lambda ((mapping? mapping1) (mapping? mapping2)) mapping?)
   (pure))

 (mapping-union!
   (lambda ((mapping? mapping1) (mapping? mapping2) ...) mapping?)
   ())

 (mapping-intersection!
   (lambda ((mapping? mapping1) (mapping? mapping2) ...) mapping?)
   ())

 (mapping-difference!
   (lambda ((mapping? mapping1) (mapping? mapping2) ...) mapping?)
   ())

 (mapping-xor!
   (lambda ((mapping? mapping1) (mapping? mapping2)) mapping?)
   ())

 (mapping/ordered
   (lambda ((comparator? comparator) arg ...) mapping?)
   (pure))

 (mapping-unfold/ordered
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed (comparator? comparator)) mapping?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) (values * *)))
    (successor (lambda (seed) *))))

 (alist->mapping/ordered
   (lambda ((comparator? comparator) (list? alist)) mapping?)
   (pure))

 (alist->mapping/ordered!
   (lambda ((mapping? mapping) (list? alist)) mapping?)
   ())

 (mapping-min-key
   (lambda ((mapping? mapping)) *)
   (pure))

 (mapping-max-key
   (lambda ((mapping? mapping)) *)
   (pure))

 (mapping-min-value
   (lambda ((mapping? mapping)) *)
   (pure))

 (mapping-max-value
   (lambda ((mapping? mapping)) *)
   (pure))

 (mapping-min-entry
   (lambda ((mapping? mapping)) (values * *))
   (pure))

 (mapping-max-entry
   (lambda ((mapping? mapping)) (values * *))
   (pure))

 (mapping-key-predecessor
   (lambda ((mapping? mapping) obj (procedure? failure)) *)
   (pure)
   ((failure (lambda () *))))

 (mapping-key-successor
   (lambda ((mapping? mapping) obj (procedure? failure)) *)
   (pure)
   ((failure (lambda () *))))

 (mapping-range=
   (lambda ((mapping? mapping) obj) mapping?)
   (pure))

 (mapping-range<
   (lambda ((mapping? mapping) obj) mapping?)
   (pure))

 (mapping-range>
   (lambda ((mapping? mapping) obj) mapping?)
   (pure))

 (mapping-range<=
   (lambda ((mapping? mapping) obj) mapping?)
   (pure))

 (mapping-range>=
   (lambda ((mapping? mapping) obj) mapping?)
   (pure))

 (mapping-range=!
   (lambda ((mapping? mapping) obj) mapping?)
   ())

 (mapping-range<!
   (lambda ((mapping? mapping) obj) mapping?)
   ())

 (mapping-range>!
   (lambda ((mapping? mapping) obj) mapping?)
   ())

 (mapping-range<=!
   (lambda ((mapping? mapping) obj) mapping?)
   ())

 (mapping-range>=!
   (lambda ((mapping? mapping) obj) mapping?)
   ())

 (mapping-split
   (lambda ((mapping? mapping) obj) (values mapping? mapping? mapping? mapping? mapping?))
   (pure))

 (mapping-split!
   (lambda ((mapping? mapping) obj) (values mapping? mapping? mapping? mapping? mapping?))
   ())

 (mapping-catenate
   (lambda ((mapping? mapping1) key value (mapping? mapping2)) mapping?)
   (pure))

 (mapping-catenate!
   (lambda ((mapping? mapping1) key value (mapping? mapping2)) mapping?)
   ())

 (mapping-map/monotone
   (lambda ((procedure? proc) (comparator? comparator) (mapping? mapping)) mapping?)
   (pure)
   ((proc (lambda (key value) (values * *)))))

 (mapping-map/monotone!
   (lambda ((procedure? proc) (comparator? comparator) (mapping? mapping)) mapping?)
   ()
   ((proc (lambda (key value) (values * *)))))

 (mapping-fold/reverse
   (lambda ((procedure? proc) nil (mapping? mapping)) *)
   (pure)
   ((proc (lambda (key value state) *))))

 (comparator?
   (lambda (obj) boolean?)
   (pure predicate))

 (make-mapping-comparator
   (lambda ((comparator? comparator)) comparator?)
   (pure))

 (mapping-comparator (value comparator?))

 )
