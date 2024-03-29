(((name . "make-eq-hashtable")
  (signature case-lambda (() hashtable?) (((integer? k)) hashtable?))
  (tags pure)
  (desc . "Returns a newly allocated mutable hashtable that accepts arbitrary objects as keys, and compares those keys with eq?. If an argument is given, the initial capacity of the hashtable is set to approximately k elements."))
 ((name . "make-eqv-hashtable")
  (signature case-lambda (() hashtable?) (((integer? k)) hashtable?))
  (tags pure)
  (desc . "Returns a newly allocated mutable hashtable that accepts arbitrary objects as keys, and compares those keys with eqv?. If an argument is given, the initial capacity of the hashtable is set to approximately k elements."))
 ((name . "make-hashtable")
  (signature
   case-lambda
   (((procedure? hash-function) (procedure? equiv)) hashtable?)
   (((procedure? hash-function) (procedure? equiv) (integer? k)) hashtable?))
  (subsigs
   (hash-function (lambda (key) integer?))
   (equiv (lambda (a b) boolean?)))
  (tags pure)
  (desc . "Hash-function and equiv must be procedures. Hash-function should accept a key as an argument and should return a non-negative exact integer object. Equiv should accept two keys as arguments and return a single value. Neither procedure should mutate the hashtable returned by make-hashtable. The make-hashtable procedure returns a newly allocated mutable hashtable using hash-function as the hash function and equiv as the equivalence function used to compare keys. If a third argument is given, the initial capacity of the hashtable is set to approximately k elements. Both hash-function and equiv should behave like pure functions on the domain of keys. For example, the string-hash and string=? procedures are permissible only if all keys are strings and the contents of those strings are never changed so long as any of them continues to serve as a key in the hashtable. Furthermore, any pair of keys for which equiv returns true should be hashed to the same exact integer objects by hash-function.
Implementation responsibilities: The implementation must check the restrictions on hash-function and equiv to the extent performed by applying them as described.
Note: Hashtables are allowed to cache the results of calling the hash function and equivalence function, so programs cannot rely on the hash function being called for every lookup or update. Furthermore any hashtable operation may call the hash function more than once."))
 ((name . "hashtable?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a hashtable, #f otherwise."))
 ((name . "hashtable-size")
  (signature lambda ((hashtable? hashtable)) integer?)
  (tags pure)
  (desc . "Returns the number of keys contained in hashtable as an exact integer object."))
 ((name . "hashtable-ref")
  (signature lambda ((hashtable? hashtable) key default) *)
  (tags pure)
  (desc . "Returns the value in hashtable associated with key. If hashtable does not contain an association for key, default is returned."))
 ((name . "hashtable-set!")
  (signature lambda ((hashtable? hashtable) key obj) undefined)
  (desc . "Changes hashtable to associate key with obj, adding a new association or replacing any existing association for key, and returns unspecified values."))
 ((name . "hashtable-delete!")
  (signature lambda ((hashtable? hashtable) key) undefined)
  (desc . "Removes any association for key within hashtable and returns unspecified values."))
 ((name . "hashtable-contains?")
  (signature lambda ((hashtable? hashtable) key) boolean?)
  (tags pure)
  (desc . "Returns #t if hashtable contains an association for key, #f otherwise."))
 ((name . "hashtable-update!")
  (signature
   lambda
   ((hashtable? hashtable) key (procedure? proc) default)
   boolean?)
  (subsigs (proc (lambda (value) *)))
  (desc . "Proc should accept one argument, should return a single value, and should not mutate hashtable. The hashtable-update! procedure applies proc to the value in hashtable associated with key, or to default if hashtable does not contain an association for key. The hashtable is then changed to associate key with the value returned by proc."))
 ((name . "hashtable-copy")
  (signature
   case-lambda
   (((hashtable? hashtable)) hashtable?)
   (((hashtable? hashtable) (boolean? mutable)) hashtable?))
  (tags pure)
  (desc . "Returns a copy of hashtable. If the mutable argument is provided and is true, the returned hashtable is mutable; otherwise it is immutable."))
 ((name . "hashtable-clear!")
  (signature
   case-lambda
   (((hashtable? hashtable)) undefined)
   (((hashtable? hashtable) (integer? k)) undefined))
  (desc . "Removes all associations from hashtable and returns unspecified values."))
 ((name . "hashtable-keys")
  (signature lambda ((hashtable? hashtable)) vector?)
  (tags pure)
  (desc . "Returns a vector of all keys in hashtable. The order of the vector is unspecified."))
 ((name . "hashtable-entries")
  (signature lambda ((hashtable? hashtable)) (values vector? vector?))
  (tags pure)
  (desc . "Returns two values, a vector of the keys in hashtable, and a vector of the corresponding values."))
 ((name . "hashtable-equivalence-function")
  (signature lambda ((hashtable? hashtable)) procedure?)
  (subsigs
    (return (lambda (a b) boolean?)))
  (tags pure)
  (desc . "Returns the equivalence function used by hashtable to compare keys. For hashtables created with make-eq-hashtable and make-eqv-hashtable, returns eq? and eqv? respectively."))
 ((name . "hashtable-hash-function")
  (signature lambda ((hashtable? hashtable)) procedure?)
  (subsigs
    (return (lambda (key) integer?)))
  (tags pure)
  (desc . "Returns the hash function used by hashtable. For hashtables created by make-eq-hashtable or make-eqv-hashtable, #f is returned."))
 ((name . "hashtable-mutable?")
  (signature lambda ((hashtable? hashtable)) boolean?)
  (tags pure)
  (desc . "Returns #t if hashtable is mutable, otherwise #f."))
 ((name . "equal-hash") 
  (signature lambda (obj) integer?)
  (tags pure)
  (desc . "Returns an integer hash value for obj, based on its structure and current contents. This hash function is suitable for use with equal? as an equivalence function.
Note: Like equal?, the equal-hash procedure must always terminate, even if its arguments contain cycles. "))
 ((name . "string-hash")
  (signature lambda ((string? string)) integer?)
  (tags pure)
  (desc . "Returns an integer hash value for string, based on its current contents. This hash function is suitable for use with string=? as an equivalence function."))
 ((name . "string-ci-hash")
  (signature lambda ((string? string)) integer?)
  (tags pure)
  (desc . "Returns an integer hash value for string based on its current contents, ignoring case. This hash function is suitable for use with string-ci=? as an equivalence function."))
 ((name . "symbol-hash")
  (signature lambda ((symbol? symbol)) integer?)
  (tags pure)
  (desc . "Returns an integer hash value for symbol.")))
