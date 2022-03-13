(

 (hashmap
   (lambda ((comparator? comparator) arg ...) hashmap?)
   (pure))

 (hashmap-unfold
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed (comparator? comparator)) hashmap?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) (values * *)))
    (successor (lambda (seed) *))))

 (hashmap?
   (lambda (obj) boolean?)
   (pure predicate))

 (hashmap-contains?
   (lambda ((hashmap? hashmap) key) boolean?)
   (pure))

 (hashmap-empty?
   (lambda ((hashmap? hashmap)) boolean?)
   (pure))

 (hashmap-disjoint?
   (lambda ((hashmap? hashmap1) (hashmap? hashmap2)) boolean?)
   (pure))

 (hashmap-ref
   (lambda ((hashmap? hashmap) key) *)
   (pure))

 (hashmap-ref
   (lambda ((hashmap? hashmap) key (procedure? failure)) *)
   (pure)
   ((failure (lambda () *))))

 (hashmap-ref
   (lambda ((hashmap? hashmap) key (procedure? failure) (procedure? success)) *)
   (pure)
   ((failure (lambda () *))
    (success (lambda (value) *))))

 (hashmap-ref/default
   (lambda ((hashmap? hashmap) key default) *)
   (pure))

 (hashmap-key-comparator
   (lambda ((hashmap? hashmap)) comparator?)
   (pure))

 (hashmap-adjoin
   (lambda ((hashmap? hashmap) key1 value1 ...) hashmap?)
   (pure))

 (hashmap-adjoin!
   (lambda ((hashmap? hashmap) key1 value1 ...) hashmap?)
   ())

 (hashmap-set
   (lambda ((hashmap? hashmap) key1 value1 ...) hashmap?)
   (pure))

 (hashmap-set!
   (lambda ((hashmap? hashmap) key1 value1 ...) hashmap?)
   ())

 (hashmap-replace
   (lambda ((hashmap? hashmap) key value) hashmap?)
   (pure))

 (hashmap-replace!
   (lambda ((hashmap? hashmap) key value) hashmap?)
   ())

 (hashmap-delete
   (lambda ((hashmap? hashmap) key ...) hashmap?)
   (pure))

 (hashmap-delete!
   (lambda ((hashmap? hashmap) key ...) hashmap?)
   (pure))

 (hashmap-delete-all
   (lambda ((hashmap? hashmap) (list? keys)) hashmap?)
   (pure))

 (hashmap-delete-all!
   (lambda ((hashmap? hashmap) (list? keys)) hashmap?)
   ())

 (hashmap-intern
   (lambda ((hashmap? hashmap) key (procedure? failure)) (values hashmap? *))
   (pure)
   ((failure (lambda () *))))

 (hashmap-intern!
   (lambda ((hashmap? hashmap) key (procedure? failure)) (values hashmap? *))
   ()
   ((failure (lambda () *))))

 (hashmap-update
   (lambda ((hashmap? hashmap) key (procedure? updater)) hashmap?)
   (pure)
   ((updater (lambda (value) *))))

 (hashmap-update
   (lambda ((hashmap? hashmap) key (procedure? updater) (procedure? failure)) hashmap?)
   (pure)
   ((updater (lambda (value) *))
    (failure (lambda () *))))

 (hashmap-update
   (lambda ((hashmap? hashmap) key (procedure? updater) (procedure? failure) (procedure? success)) hashmap?)
   (pure)
   ((updater (lambda (value) *))
    (failure (lambda () *))
    (success (lambda (value) *))))

 (hashmap-update!
   (lambda ((hashmap? hashmap) key (procedure? updater)) hashmap?)
   ()
   ((updater (lambda (value) *))))

 (hashmap-update!
   (lambda ((hashmap? hashmap) key (procedure? updater) (procedure? failure)) hashmap?)
   ()
   ((updater (lambda (value) *))
    (failure (lambda () *))))

 (hashmap-update!
   (lambda ((hashmap? hashmap) key (procedure? updater) (procedure? failure) (procedure? success)) hashmap?)
   ()
   ((updater (lambda (value) *))
    (failure (lambda () *))
    (success (lambda (value) *))))

 (hashmap-update/default
   (lambda ((hashmap? hashmap) key (procedure? updater) default) hashmap?)
   (pure)
   ((updater (lambda (value) *))))

 (hashmap-update!/default
   (lambda ((hashmap? hashmap) key (procedure? updater) default) hashmap?)
   ()
   ((updater (lambda (value) *))))

 (hashmap-pop
   (lambda ((hashmap? hashmap)) (values hashmap? * *))
   (pure))

 (hashmap-pop
   (lambda ((hashmap? hashmap) (procedure? failure)) (values hashmap? * *))
   (pure)
   ((failure (lambda () (values hashmap? * *)))))

 (hashmap-pop!
   (lambda ((hashmap? hashmap)) (values hashmap? * *))
   ())

 (hashmap-pop!
   (lambda ((hashmap? hashmap) (procedure? failure)) (values hashmap? * *))
   ()
   ((failure (lambda () (values hashmap? * *)))))

 (hashmap-search
   (lambda ((hashmap? hashmap) key (procedure? failure) (procedure? success)) (values hashmap? *))
   (pure)
   ((failure (lambda ((procedure? insert) (procedure? ignore)) *))
    (insert (lambda (value obj) *))
    (ignore (lambda (obj) *))
    (success (lambda (key value (procedure? update) (procedure? remove)) *))
    (update (lambda (new-key new-value obj) *))
    (remove (lambda (obj) *))))

 (hashmap-search!
   (lambda ((hashmap? hashmap) key (procedure? failure) (procedure? success)) (values hashmap? *))
   ()
   ((failure (lambda ((procedure? insert) (procedure? ignore)) *))
    (insert (lambda (value obj) *))
    (ignore (lambda (obj) *))
    (success (lambda (key value (procedure? update) (procedure? remove)) *))
    (update (lambda (new-key new-value obj) *))
    (remove (lambda (obj) *))))

 (hashmap-size
   (lambda ((hashmap? hashmap)) integer?)
   (pure))

 (hashmap-find
   (lambda ((procedure? predicate) (hashmap? hashmap) (procedure? failure)) (values * *))
   (pure)
   ((predicate (lambda (key value) boolean?))
    (failure (lambda () *))))

 (hashmap-count
   (lambda ((procedure? predicate) (hashmap? hashmap)) integer?)
   (pure)
   ((predicate (lambda (key value) boolean?))))

 (hashmap-any?
   (lambda ((procedure? predicate) (hashmap? hashmap)) boolean?)
   (pure)
   ((predicate (lambda (key value) boolean?))))

 (hashmap-every?
   (lambda ((procedure? predicate) (hashmap? hashmap)) boolean?)
   (pure)
   ((predicate (lambda (key value) boolean?))))

 (hashmap-keys
   (lambda ((hashmap? hashmap)) list?)
   (pure))

 (hashmap-values
   (lambda ((hashmap? hashmap)) list?)
   (pure))

 (hashmap-entries
   (lambda ((hashmap? hashmap)) (values list? list))
   (pure))

 (hashmap-map
   (lambda ((procedure? proc) (comparator? comparator) (hashmap? hashmap)) hashmap?)
   (pure)
   ((proc (lambda (key value) (values * *)))))

 (hashmap-for-each
   (lambda ((procedure? proc) (comparator? comparator) (hashmap? hashmap)) undefined)
   ()
   ((proc (lambda (key value) undefined))))

 (hashmap-fold
   (lambda ((procedure? proc) nil (hashmap? hashmap)) *)
   (pure)
   ((proc (lambda (key value state) *))))

 (hashmap-map->list
   (lambda ((procedure? proc) (hashmap? hashmap)) list?)
   (pure)
   ((proc (lambda (key value) *))))

 (hashmap-filter
   (lambda ((procedure? predicate) (hashmap? hashmap)) hashmap?)
   (pure)
   ((predicate (lambda (key value) boolean?))))

 (hashmap-filter!
   (lambda ((procedure? predicate) (hashmap? hashmap)) hashmap?)
   ()
   ((predicate (lambda (key value) boolean?))))

 (hashmap-remove
   (lambda ((procedure? predicate) (hashmap? hashmap)) hashmap?)
   (pure)
   ((predicate (lambda (key value) boolean?))))

 (hashmap-remove!
   (lambda ((procedure? predicate) (hashmap? hashmap)) hashmap?)
   ()
   ((predicate (lambda (key value) boolean?))))

 (hashmap-partition
   (lambda ((procedure? predicate) (hashmap? hashmap)) (values hashmap? hashmap?))
   (pure)
   ((predicate (lambda (key value) boolean?))))

 (hashmap-partition!
   (lambda ((procedure? predicate) (hashmap? hashmap)) (values hashmap? hashmap?))
   ()
   ((predicate (lambda (key value) boolean?))))

 (hashmap-copy
   (lambda ((hashmap? hashmap)) hashmap?)
   (pure))

 (hashmap->alist
   (lambda ((hashmap? hashmap)) list?)
   (pure))

 (alist->hashmap
   (lambda ((comparator? comparator) (list? alist)) hashmap?)
   (pure))

 (alist->hashmap!
   (lambda ((hashmap? hashmap) (list? alist)) hashmap?)
   ())

 (hashmap=?
   (lambda ((comparator? comparator) (hashmap? hashmap1) (hashmap? hashmap2) ...) boolean?)
   (pure))

 (hashmap<?
   (lambda ((comparator? comparator) (hashmap? hashmap1) (hashmap? hashmap2) ...) boolean?)
   (pure))

 (hashmap>?
   (lambda ((comparator? comparator) (hashmap? hashmap1) (hashmap? hashmap2) ...) boolean?)
   (pure))

 (hashmap<=?
   (lambda ((comparator? comparator) (hashmap? hashmap1) (hashmap? hashmap2) ...) boolean?)
   (pure))

 (hashmap>=?
   (lambda ((comparator? comparator) (hashmap? hashmap1) (hashmap? hashmap2) ...) boolean?)
   (pure))

 (hashmap-union
   (lambda ((hashmap? hashmap1) (hashmap? hashmap2) ...) hashmap?)
   (pure))

 (hashmap-intersection
   (lambda ((hashmap? hashmap1) (hashmap? hashmap2) ...) hashmap?)
   (pure))

 (hashmap-difference
   (lambda ((hashmap? hashmap1) (hashmap? hashmap2) ...) hashmap?)
   (pure))

 (hashmap-xor
   (lambda ((hashmap? hashmap1) (hashmap? hashmap2)) hashmap?)
   (pure))

 (hashmap-union!
   (lambda ((hashmap? hashmap1) (hashmap? hashmap2) ...) hashmap?)
   ())

 (hashmap-intersection!
   (lambda ((hashmap? hashmap1) (hashmap? hashmap2) ...) hashmap?)
   ())

 (hashmap-difference!
   (lambda ((hashmap? hashmap1) (hashmap? hashmap2) ...) hashmap?)
   ())

 (hashmap-xor!
   (lambda ((hashmap? hashmap1) (hashmap? hashmap2)) hashmap?)
   ())

 )
