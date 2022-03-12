(

 (make-hash-table
   (lambda ((comparator? comparator) arg ...) hash-table?)
   (pure))

 (make-hash-table
   (lambda ((procedure? equality-predicate) arg ...) hash-table?)
   (pure deprecated)
   ((equality-predicate (lambda (a b) boolean?))))

 (make-hash-table
   (lambda ((procedure? equality-predicate) (procedure? hash-function) arg ...) hash-table?)
   (pure deprecated)
   ((equality-predicate (lambda (a b) boolean?))
    (hash-function (lambda (obj) integer?))))

 (hash-table
   (lambda ((comparator? comparator) key1 value1 ...) hash-table?)
   (pure))

 (hash-table-unfold
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed (comparator? comparator) arg ...) hash-table?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) (values * *)))
    (successor (lambda (seed) *))))

 (alist->hash-table
   (lambda ((list? alist) (comparator? comparator) arg ...) hash-table?)
   (pure))

 (alist->hash-table
   (lambda ((list? alist) (procedure? equality-predicate) arg ...) hash-table?)
   (pure deprecated)
   ((equality-predicate (lambda (a b) boolean?))
    (hash-function (lambda (obj) integer?))))

 (alist->hash-table
   (lambda ((list? alist) (procedure? equality-predicate) (procedure? hash-function) arg ...) hash-table?)
   (pure deprecated)
   ((equality-predicate (lambda (a b) boolean?))
    (hash-function (lambda (obj) integer?))))

 (hash-table?
   (lambda (obj) boolean?)
   (pure predicate))

 (hash-table-contains?
   (lambda ((hash-table? hash-table) key) boolean?)
   (pure))

 (hash-table-exists?
   (lambda ((hash-table? hash-table) key) boolean?)
   (pure deprecated))

 (hash-table-empty?
   (lambda ((hash-table? hash-table)) boolean?)
   (pure))

 (hash-table=?
   (lambda ((comparator? value-comparator) (hash-table? hash-table1) (hash-table? hash-table2)) boolean?)
   (pure))

 (hash-table-mutable?
   (lambda ((hash-table? hash-table)) boolean?)
   (pure))

 (hash-table-ref
   (lambda ((hash-table? hash-table) key) *)
   (pure))

 (hash-table-ref
   (lambda ((hash-table? hash-table) key (procedure? failure)) *)
   (pure)
   ((failure (lambda () *))))

 (hash-table-ref
   (lambda ((hash-table? hash-table) key (procedure? failure) (procedure? success)) *)
   (pure)
   ((failure (lambda () *))
    (success (lambda (value) *))))

 (hash-table-ref/default
   (lambda ((hash-table? hash-table) key default) *)
   (pure))

 (hash-table-set!
   (lambda ((hash-table? hash-table) key1 value1 ...) undefined))

 (hash-table-delete!
   (lambda ((hash-table? hash-table) key ...) undefined))

 (hash-table-intern!
   (lambda ((hash-table? hash-table) key (procedure? failure)) undefined)
   ()
   ((failure (lambda () *))))

 (hash-table-update!
   (lambda ((hash-table? hash-table) key (procedure? updater)) undefined)
   ()
   ((updated (lambda (value) *))))

 (hash-table-update!
   (lambda ((hash-table? hash-table) key (procedure? updater) (procedure? failure)) undefined)
   ()
   ((updater (lambda (value) *))
    (failure (lambda () *))))

 (hash-table-update!
   (lambda ((hash-table? hash-table) key (procedure? updater) (procedure? failure) (procedure? success)) undefined)
   ()
   ((updater (lambda (value) *))
    (failure (lambda () *))
    (success (lambda (value) *))))

 (hash-table-update!/default
   (lambda ((hash-table? hash-table) key (procedure? updater) default) undefined)
   ()
   ((updated (lambda (value) *))))

 (hash-table-pop!
   (lambda ((hash-table? hash-table)) (values * *))
   ())

 (hash-table-clear!
   (lambda ((hash-table? hash-table)) undefined))

 (hash-table-size
   (lambda ((hash-table? hash-table)) integer?)
   (pure))

 (hash-table-keys
   (lambda ((hash-table? hash-table)) list?)
   (pure))

 (hash-table-values
   (lambda ((hash-table? hash-table)) list?)
   (pure))

 (hash-table-entries
   (lambda ((hash-table? hash-table)) (values list? list?))
   (pure))

 (hash-table-find
   (lambda ((procedure? proc) (hash-table? hash-table) (procedure? failure)) *)
   (pure)
   ((proc (lambda (key value) *))
    (failure (lambda () *))))

 (hash-table-count
   (lambda ((procedure? pred) (hash-table? hash-table)) integer?)
   (pure)
   ((pred (lambda (key value) boolean?))))

 (hash-table-map
   (lambda ((procedure? proc) (comparator? comparator) (hash-table? hash-table)) hash-table?)
   (pure)
   ((proc (lambda (value) *))))

 (hash-table-for-each
   (lambda ((procedure? proc) (hash-table? hash-table)) undefined)
   ()
   ((proc (lambda (key value) undefined))))

 (hash-table-walk
   (lambda ((hash-table? hash-table) (procedure? proc)) undefined)
   (deprecated)
   ((proc (lambda (key value) undefined))))

 (hash-table-map!
   (lambda ((procedure? proc) (comparator? comparator) (hash-table? hash-table)) undefined)
   ()
   ((proc (lambda (key value) *))))

 (hash-table-map->list
   (lambda ((procedure? proc) (hash-table? hash-table)) list?)
   ()
   ((proc (lambda (key value) *))))

 (hash-table-fold
   (lambda ((procedure? proc) seed (hash-table? hash-table)) *)
   (pure)
   ((proc (lambda (key value state) *))))

 (hash-table-fold
   (lambda ((hash-table? hash-table) (procedure? proc) seed) *)
   (pure deprecated)
   ((proc (lambda (key value state) *))))

 (hash-table-prune!
   (lambda ((procedure? proc) (hash-table? hash-table)) undefined)
   ()
   ((proc (lambda (key value) boolean?))))

 (hash-table-copy
   (lambda ((hash-table? hash-table)) hash-table?)
   (pure))

 (hash-table-copy
   (lambda ((hash-table? hash-table) (boolean? mutable)) hash-table?)
   (pure))

 (hash-table-empty-copy
   (lambda ((hash-table? hash-table)) hash-table?)
   (pure))

 (hash-table->alist
   (lambda ((hash-table? hash-table)) list?)
   ())

 (hash-table-union!
   (lambda ((hash-table? hash-table1) (hash-table? hash-table2)) hash-table?)
   ())

 (hash-table-merge!
   (lambda ((hash-table? hash-table1) (hash-table? hash-table2)) hash-table?)
   (deprecated))

 (hash-table-intersection!
   (lambda ((hash-table? hash-table1) (hash-table? hash-table2)) hash-table?)
   ())

 (hash-table-difference!
   (lambda ((hash-table? hash-table1) (hash-table? hash-table2)) hash-table?)
   ())

 (hash-table-xor!
   (lambda ((hash-table? hash-table1) (hash-table? hash-table2)) hash-table?)
   ())

 (hash
   (lambda (obj) integer?)
   (deprecated))

 (hash
   (lambda (obj arg) integer?)
   (deprecated))

 (string-hash
   (lambda ((string? str)) integer?)
   (pure deprecated))

 (string-hash
   (lambda ((string? str) arg) integer?)
   (pure deprecated))

 (string-ci-hash
   (lambda ((string? str)) integer?)
   (pure deprecated))

 (string-ci-hash
   (lambda ((string? str) arg) integer?)
   (pure deprecated))

 (hash-by-identity
   (lambda (obj) integer?)
   (pure deprecated))

 (hash-by-identity
   (lambda (obj arg) integer?)
   (pure deprecated))

 (hash-table-equivalence-function
   (lambda ((hash-table? hash-table)) procedure?)
   (pure deprecated)
   ((return (lambda (a b) boolean?))))

 (hash-table-hash-function
   (lambda ((hash-table? hash-table)) procedure?)
   (pure deprecated)
   ((return (lambda (obj) integer?))))

 )
