(((name . "make-hash-table")
  (signature
   case-lambda
   (((comparator? comparator) arg ...) hash-table?)
   (((procedure? equality-predicate) arg ...) hash-table?)
   (((procedure? equality-predicate) (procedure? hash-function) arg ...)
    hash-table?))
  (subsigs
   (equality-predicate (lambda (a b) boolean?))
   (hash-function (lambda (obj) integer?)))
  (tags pure)
  (desc . "Returns a newly allocated hash table whose equality predicate and hash function are extracted from comparator. Alternatively, for backward compatibility with SRFI 69 the equality predicate and hash function can be passed as separate arguments; this usage is deprecated.
As mentioned above, implementations are free to use an appropriate implementation-dependent hash function instead of the specified hash function, provided that the specified equality predicate is a refinement of the equal? predicate. This applies whether the hash function and equality predicate are passed as separate arguments or packaged up into a comparator.
If an equality predicate rather than a comparator is provided, the ability to omit the hash-function argument is severely limited. The implementation must provide hash functions appropriate for use with the predicates eq?, eqv?, equal?, string=?, and string-ci=?, and may extend this list. But if any unknown equality predicate is provided without a hash function, an error should be signaled. The constraints on equality predicates and hash functions are given in SRFI 128.
The meaning of any further arguments is implementation-dependent. However, implementations which support the ability to specify the initial capacity of a hash table should interpret a non-negative exact integer as the specification of that capacity. In addition, if the symbols thread-safe, weak-keys, ephemeral-keys, weak-values, or ephemeral-values are present, implementations should create thread-safe hash tables, hash tables with weak keys or ephemeral keys, or hash tables with weak or ephemeral values respectively. Implementations are free to use ephemeral keys or values when weak keys or values respectively have been requested. To avoid collision with the hash-function argument, none of these arguments can be procedures.
(R6RS make-eq-hashtable, make-eqv-hashtable, and make-hashtable; Common Lisp make-hash-table) "))
 ((name . "hash-table")
  (signature lambda ((comparator? comparator) key1 value1 ...) hash-table?)
  (tags pure)
  (desc . "Returns a newly allocated hash table, created as if by make-hash-table using comparator. For each pair of arguments, an association is added to the new hash table with key as its key and value as its value. If the implementation supports immutable hash tables, this procedure returns an immutable hash table. If the same key (in the sense of the equality predicate) is specified more than once, it is an error."))
 ((name . "hash-table-unfold")
  (signature
   lambda
   ((procedure? stop?)
    (procedure? mapper)
    (procedure? successor)
    seed
    (comparator? comparator)
    arg
    ...)
   hash-table?)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (values * *)))
   (successor (lambda (seed) *)))
  (tags pure)
  (desc . "Create a new hash table as if by make-hash-table using comparator and the args. If the result of applying the predicate stop? to seed is true, return the hash table. Otherwise, apply the procedure mapper to seed. Mapper returns two values, which are inserted into the hash table as the key and the value respectively. Then get a new seed by applying the procedure successor to seed, and repeat this algorithm."))
 ((name . "alist->hash-table")
  (signature
   case-lambda
   (((list? alist) (comparator? comparator) arg ...) hash-table?)
   (((list? alist) (procedure? equality-predicate) arg ...) hash-table?)
   (((list? alist)
     (procedure? equality-predicate)
     (procedure? hash-function)
     arg
     ...)
    hash-table?))
  (subsigs
   (equality-predicate (lambda (a b) boolean?))
   (hash-function (lambda (obj) integer?)))
  (tags pure)
  (desc . "Returns a newly allocated hash-table as if by make-hash-table using comparator and the args. It is then initialized from the associations of alist. Associations earlier in the list take precedence over those that come later. The second form is for compatibility with SRFI 69, and is deprecated."))
 ((name . "hash-table?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a hash table, and #f otherwise. (R6RS hashtable?; Common Lisp hash-table-p)"))
 ((name . "hash-table-contains?")
  (signature lambda ((hash-table? hash-table) key) boolean?)
  (tags pure)
  (desc . "Returns #t if there is any association to key in hash-table, and #f otherwise. Must execute in expected amortized constant time. (R6RS hashtable-contains?)"))
 ((name . "hash-table-exists?")
  (signature lambda ((hash-table? hash-table) key) boolean?)
  (tags pure deprecated)
  (desc . "Returns #t if there is any association to key in hash-table, and #f otherwise. Must execute in expected amortized constant time. The hash-table-exists? procedure is the same as hash-table-contains?, is provided for backward compatibility with SRFI 69, and is deprecated. (R6RS hashtable-contains?)"))
 ((name . "hash-table-empty?")
  (signature lambda ((hash-table? hash-table)) boolean?)
  (tags pure)
  (desc . "Returns #t if hash-table contains no associations, and #f otherwise."))
 ((name . "hash-table=?")
  (signature
   lambda
   ((comparator? value-comparator)
    (hash-table? hash-table1)
    (hash-table? hash-table2))
   boolean?)
  (tags pure)
  (desc . " Returns #t if hash-table1 and hash-table2 have the same keys (in the sense of their common equality predicate) and each key has the same value (in the sense of value-comparator), and #f otherwise."))
 ((name . "hash-table-mutable?")
  (signature lambda ((hash-table? hash-table)) boolean?)
  (tags pure)
  (desc . "Returns #t if the hash table is mutable. Implementations may or may not support immutable hash tables. (R6RS hashtable-mutable?)"))
 ((name . "hash-table-ref")
  (signature
   case-lambda
   (((hash-table? hash-table) key) *)
   (((hash-table? hash-table) key (procedure? failure)) *)
   (((hash-table? hash-table) key (procedure? failure) (procedure? success))
    *))
  (subsigs (failure (lambda () *)) (success (lambda (value) *)))
  (tags pure)
  (desc . "Extracts the value associated to key in hash-table, invokes the procedure success on it, and returns its result; if success is not provided, then the value itself is returned. If key is not contained in hash-table and failure is supplied, then failure is invoked on no arguments and its result is returned. Otherwise, it is an error. Must execute in expected amortized constant time, not counting the time to call the procedures. SRFI 69 does not support the success procedure."))
 ((name . "hash-table-ref/default")
  (signature lambda ((hash-table? hash-table) key default) *)
  (tags pure)
  (desc . " Semantically equivalent to, but may be more efficient than, the following code:
    (hash-table-ref hash-table key (lambda () default))

(R6RS hashtable-ref; Common Lisp gethash)"))
 ((name . "hash-table-set!")
  (signature lambda ((hash-table? hash-table) key1 value1 ...) undefined)
  (desc . "Repeatedly mutates hash-table, creating new associations in it by processing the arguments from left to right. The args alternate between keys and values. Whenever there is a previous association for a key, it is deleted. It is an error if the type check procedure of the comparator of hash-table, when invoked on a key, does not return #t. Likewise, it is an error if a key is not a valid argument to the equality predicate of hash-table. Returns an unspecified value. Must execute in expected amortized constant time per key. SRFI 69, R6RS hashtable-set! and Common Lisp (setf gethash) do not handle multiple associations."))
 ((name . "hash-table-delete!")
  (signature lambda ((hash-table? hash-table) key ...) undefined)
  (desc . "Deletes any association to each key in hash-table and returns the number of keys that had associations. Must execute in expected amortized constant time per key. SRFI 69, R6RS hashtable-delete!, and Common Lisp remhash do not handle multiple associations."))
 ((name . "hash-table-intern!")
  (signature
   lambda
   ((hash-table? hash-table) key (procedure? failure))
   undefined)
  (subsigs (failure (lambda () *)))
  (desc . "Effectively invokes hash-table-ref with the given arguments and returns what it returns. If key was not found in hash-table, its value is set to the result of calling failure. Must execute in expected amortized constant time."))
 ((name . "hash-table-update!")
  (signature
   case-lambda
   (((hash-table? hash-table) key (procedure? updater)) undefined)
   (((hash-table? hash-table) key (procedure? updater) (procedure? failure))
    undefined)
   (((hash-table? hash-table)
     key
     (procedure? updater)
     (procedure? failure)
     (procedure? success))
    undefined))
  (subsigs
   (updater (lambda (value) *))
   (failure (lambda () *))
   (success (lambda (value) *)))
  (desc . " Semantically equivalent to, but may be more efficient than, the following code:
    (hash-table-set! hash-table key (updater (hash-table-ref hash-table key failure success)))
Must execute in expected amortized constant time. Returns an unspecified value. (SRFI 69 and R6RS hashtable-update! do not support the success procedure) "))
 ((name . "hash-table-update!/default")
  (signature
   lambda
   ((hash-table? hash-table) key (procedure? updater) default)
   undefined)
  (subsigs (updated (lambda (value) *)))
  (desc . " Semantically equivalent to, but may be more efficient than, the following code:
    (hash-table-set! hash-table key (updater (hash-table-ref/default hash-table key default)))
Must execute in expected amortized constant time. Returns an unspecified value. "))
 ((name . "hash-table-pop!")
  (signature lambda ((hash-table? hash-table)) (values * *))
  (desc . "Chooses an arbitrary association from hash-table and removes it, returning the key and value as two values.
It is an error if hash-table is empty."))
 ((name . "hash-table-clear!")
  (signature lambda ((hash-table? hash-table)) undefined)
  (desc . "Delete all the associations from hash-table. (R6RS hashtable-clear!; Common Lisp clrhash)"))
 ((name . "hash-table-size")
  (signature lambda ((hash-table? hash-table)) integer?)
  (tags pure)
  (desc . "Returns the number of associations in hash-table as an exact integer. Should execute in constant time. (R6RS hashtable-size; Common Lisp hash-table-count.)"))
 ((name . "hash-table-keys")
  (signature lambda ((hash-table? hash-table)) list?)
  (tags pure)
  (desc . "Returns a newly allocated list of all the keys in hash-table. R6RS hashtable-keys returns a vector."))
 ((name . "hash-table-values")
  (signature lambda ((hash-table? hash-table)) list?)
  (tags pure)
  (desc . "Returns a newly allocated list of all the keys in hash-table."))
 ((name . "hash-table-entries")
  (signature lambda ((hash-table? hash-table)) (values list? list?))
  (tags pure)
  (desc . "Returns two values, a newly allocated list of all the keys in hash-table and a newly allocated list of all the values in hash-table in the corresponding order. R6RS hash-table-entries returns vectors."))
 ((name . "hash-table-find")
  (signature
   lambda
   ((procedure? proc) (hash-table? hash-table) (procedure? failure))
   *)
  (subsigs (proc (lambda (key value) *)) (failure (lambda () *)))
  (tags pure)
  (desc . "For each association of hash-table, invoke proc on its key and value. If proc returns true, then hash-table-find returns what proc returns. If all the calls to proc return #f, return the result of invoking the thunk failure."))
 ((name . "hash-table-count")
  (signature lambda ((procedure? pred) (hash-table? hash-table)) integer?)
  (subsigs (pred (lambda (key value) boolean?)))
  (tags pure)
  (desc . "For each association of hash-table, invoke pred on its key and value. Return the number of calls to pred which returned true."))
 ((name . "hash-table-map")
  (signature
   lambda
   ((procedure? proc) (comparator? comparator) (hash-table? hash-table))
   hash-table?)
  (subsigs (proc (lambda (value) *)))
  (tags pure)
  (desc . " Returns a newly allocated hash table as if by (make-hash-table comparator). Calls proc for every association in hash-table with the value of the association. The key of the association and the result of invoking proc are entered into the new hash table. Note that this is not the result of lifting mapping over the domain of hash tables, but it is considered more useful.
If comparator recognizes multiple keys in the hash-table as equivalent, any one of such associations is taken."))
 ((name . "hash-table-for-each")
  (signature lambda ((procedure? proc) (hash-table? hash-table)) undefined)
  (subsigs (proc (lambda (key value) undefined)))
  (desc . "Calls proc for every association in hash-table with two arguments: the key of the association and the value of the association. The value returned by proc is discarded. Returns an unspecified value. (Common Lisp maphash)"))
 ((name . "hash-table-walk")
  (signature lambda ((hash-table? hash-table) (procedure? proc)) undefined)
  (subsigs (proc (lambda (key value) undefined)))
  (tags deprecated)
  (desc . "Calls proc for every association in hash-table with two arguments: the key of the association and the value of the association. The value returned by proc is discarded. Returns an unspecified value. The hash-table-walk procedure is equivalent to hash-table-for-each with the arguments reversed, is provided for backward compatibility with SRFI 69, and is deprecated. (Common Lisp maphash)"))
 ((name . "hash-table-map!")
  (signature
   lambda
   ((procedure? proc) (comparator? comparator) (hash-table? hash-table))
   undefined)
  (subsigs (proc (lambda (key value) *)))
  (desc . "Calls proc for every association in hash-table with two arguments: the key of the association and the value of the association. The value returned by proc is used to update the value of the association. Returns an unspecified value."))
 ((name . "hash-table-map->list")
  (signature lambda ((procedure? proc) (hash-table? hash-table)) list?)
  (subsigs (proc (lambda (key value) *)))
  (desc . "Calls proc for every association in hash-table with two arguments: the key of the association and the value of the association. The values returned by the invocations of proc are accumulated into a list, which is returned."))
 ((name . "hash-table-fold")
  (signature
   case-lambda
   (((procedure? proc) seed (hash-table? hash-table)) *)
   (((hash-table? hash-table) (procedure? proc) seed) *))
  (subsigs (proc (lambda (key value state) *)))
  (tags pure deprecated)
  (desc . "Calls proc for every association in hash-table with three arguments: the key of the association, the value of the association, and an accumulated value val. Val is seed for the first invocation of procedure, and for subsequent invocations of proc, the returned value of the previous invocation. The value returned by hash-table-fold is the return value of the last invocation of proc. The order of arguments with hash-table as the first argument is provided for SRFI 69 compatibility, and is deprecated."))
 ((name . "hash-table-prune!")
  (signature lambda ((procedure? proc) (hash-table? hash-table)) undefined)
  (subsigs (proc (lambda (key value) boolean?)))
  (desc . "Calls proc for every association in hash-table with two arguments, the key and the value of the association, and removes all associations from hash-table for which proc returns true. Returns an unspecified value."))
 ((name . "hash-table-copy")
  (signature
   case-lambda
   (((hash-table? hash-table)) hash-table?)
   (((hash-table? hash-table) (boolean? mutable)) hash-table?))
  (tags pure)
  (desc . "Returns a newly allocated hash table with the same properties and associations as hash-table. If the second argument is present and is true, the new hash table is mutable. Otherwise it is immutable provided that the implementation supports immutable hash tables. SRFI 69 hash-table-copy does not support a second argument. (R6RS hashtable-copy)"))
 ((name . "hash-table-empty-copy")
  (signature lambda ((hash-table? hash-table)) hash-table?)
  (tags pure)
  (desc . "Returns a newly allocated mutable hash table with the same properties as hash-table, but with no associations."))
 ((name . "hash-table->alist")
  (signature lambda ((hash-table? hash-table)) list?)
  (desc . "Returns an alist with the same associations as hash-table in an unspecified order."))
 ((name . "hash-table-union!")
  (signature
   lambda
   ((hash-table? hash-table1) (hash-table? hash-table2))
   hash-table?)
  (desc . "Adds the associations of hash-table2 to hash-table1 and returns hash-table1. If a key appears in both hash tables, its value is set to the value appearing in hash-table1. Returns hash-table1."))
 ((name . "hash-table-merge!")
  (signature
   lambda
   ((hash-table? hash-table1) (hash-table? hash-table2))
   hash-table?)
  (tags deprecated)
  (desc . "Adds the associations of hash-table2 to hash-table1 and returns hash-table1. If a key appears in both hash tables, its value is set to the value appearing in hash-table1. Returns hash-table1. The hash-table-merge! procedure is the same as hash-table-union!, is provided for compatibility with SRFI 69, and is deprecated."))
 ((name . "hash-table-intersection!")
  (signature
   lambda
   ((hash-table? hash-table1) (hash-table? hash-table2))
   hash-table?)
  (desc . "Deletes the associations from hash-table1 whose keys don't also appear in hash-table2 and returns hash-table1."))
 ((name . "hash-table-difference!")
  (signature
   lambda
   ((hash-table? hash-table1) (hash-table? hash-table2))
   hash-table?)
  (desc . "Deletes the associations of hash-table1 whose keys are also present in hash-table2 and returns hash-table1."))
 ((name . "hash-table-xor!")
  (signature
   lambda
   ((hash-table? hash-table1) (hash-table? hash-table2))
   hash-table?)
  (desc . "Deletes the associations of hash-table1 whose keys are also present in hash-table2, and then adds the associations of hash-table2 whose keys are not present in hash-table1 to hash-table1. Returns hash-table1."))
 ((name . "hash")
  (signature case-lambda ((obj) integer?) ((obj arg) integer?))
  (tags deprecated)
  (desc . "The same as SRFI 128's default-hash procedure, except that it must accept (and should ignore) an optional second argument."))
 ((name . "string-hash")
  (signature
   case-lambda
   (((string? str)) integer?)
   (((string? str) arg) integer?))
  (tags pure deprecated)
  (desc . "Similar to SRFI 128's string-hash procedure, except that it must accept (and should ignore) an optional second argument. It is incompatible with the procedure of the same name exported by SRFI 128 and SRFI 126."))
 ((name . "string-ci-hash")
  (signature
   case-lambda
   (((string? str)) integer?)
   (((string? str) arg) integer?))
  (tags pure deprecated)
  (desc . "Similar to SRFI 128's string-ci-hash procedure, except that it must accept (and should ignore) an optional second argument. It is incompatible with the procedure of the same name exported by SRFI 128 and SRFI 126."))
 ((name . "hash-by-identity")
  (signature case-lambda ((obj) integer?) ((obj arg) integer?))
  (tags pure deprecated)
  (desc . "The same as SRFI 128's default-hash procedure, except that it must accept (and should ignore) an optional second argument. However, if the implementation replaces the hash function associated with the eq? predicate with an implementation-dependent alternative, it is an error to call this procedure at all."))
 ((name . "hash-table-equivalence-function")
  (signature lambda ((hash-table? hash-table)) procedure?)
  (subsigs (return (lambda (a b) boolean?)))
  (tags pure deprecated)
  (desc . "Returns the equivalence procedure used to create hash-table."))
 ((name . "hash-table-hash-function")
  (signature lambda ((hash-table? hash-table)) procedure?)
  (subsigs (return (lambda (obj) integer?)))
  (tags pure deprecated)
  (desc . "Returns the hash function used to create hash-table. However, if the implementation has replaced the user-specified hash function with an implementation-specific alternative, the implementation may return #f instead.")))
