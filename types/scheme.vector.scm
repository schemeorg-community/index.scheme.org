(
 
 (make-vector
   (lambda ((integer? k)) vector?)
   ())

 (make-vector
   (lambda ((integer? k) fill) vector?)
   (pure))
 
 (vector
   (lambda (obj ...) vector?)
   (pure))
 
 (vector-unfold
   (lambda ((procedure? f) (integer? length) initial-seed ...) vector?)
   (pure)
   ((f (lambda ((integer? index) seed ...) (values * * ...)))))
 
 (vector-unfold-right
   (lambda ((procedure? f) (integer? length) initial-seed ...) vector?)
   (pure)
   ((f (lambda ((integer? index) seed ...) (values * * ...)))))
 
 (vector-copy
   (lambda ((vector? vector)) vector?)
   (pure))

 (vector-copy
   (lambda ((vector? vector) (integer? start)) vector?)
   (pure))

 (vector-copy
   (lambda ((vector? vector) (integer? start) (integer? end)) vector?)
   (pure))
 
 (vector-reverse-copy
   (lambda ((vector? vector)) vector?)
   (pure))

 (vector-reverse-copy
   (lambda ((vector? vector) (integer? start)) vector?)
   (pure))

 (vector-reverse-copy
   (lambda ((vector? vector) (integer? start) (integer? end)) vector?)
   (pure))
 
 (vector-append
   (lambda ((vector? vector) ...) vector?)
   (pure))
 
 (vector-concatenate
   (lambda ((list? list-of-vectors) ...) vector?)
   (pure))
 
 (vector-append-subvectors
   (lambda ((vector? vec1) (integer? start1) (integer? end1) ...) vector?)
   (pure))
 
 (vector?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (vector-empty?
   (lambda ((vector? vec)) boolean?)
   (pure))
 
 (vector=
   (lambda ((procedure? elt=?) (vector? vec) ...) boolean?)
   (pure)
   ((elt=? (lambda (a b) boolean?))))
 
 (vector-ref
   (lambda ((vector? vector) (integer? k)) *)
   (pure))
 
 (vector-length
   (lambda ((vector? vector)) integer?)
   (pure))
 
 (vector-fold
   (lambda ((procedure? kons) knil (vector? vec1) (vector? vec2) ...) *)
   (pure)
   ((kons (lambda (state obj1 obj2 ...) *))))
 
 (vector-fold-right
   (lambda ((procedure? kons) knil (vector? vec1) (vector? vec2) ...) *)
   (pure)
   ((kons (lambda (state obj1 obj2 ...) *))))
 
 (vector-map
   (lambda ((procedure? proc) (vector? vector1) (vector? vector2) ...) vector?)
   (pure)
   ((proc (lambda (obj ...) *))))
 
 (vector-map!
   (lambda ((procedure? proc) (vector? vector1) (vector? vector2) ...) undefined)
   ()
   ((proc (lambda (obj ...) *))))
 
 (vector-for-each
   (lambda ((procedure? proc) (vector? vector1) (vector? vector2) ...) undefined)
   ()
   ((proc (lambda (obj ...) undefined))))
 
 (vector-count
   (lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) integer?)
   (pure)
   ((pred? (lambda (obj1 obj2 ...) *))))
 
 (vector-cumulate
   (lambda ((procedure? f) knil (vector? vec)) vector?)
   (pure)
   ((f (lambda (obj1 obj2) *))))
 
 (vector-index
   (lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) (or integer? boolean?))
   (pure)
   ((pred? (lambda (obj1 obj2 ...) *))))
 
 (vector-index-right
   (lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) (or integer? boolean?))
   (pure)
   ((pred? (lambda (obj1 obj2 ...) *))))
 
 (vector-skip
   (lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) (or integer? boolean?))
   (pure)
   ((pred? (lambda (obj1 obj2 ...) *))))
 
 (vector-skip-right
   (lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) (or integer? boolean?))
   (pure)
   ((pred? (lambda (obj1 obj2 ...) *))))
 
 (vector-binary-search
   (lambda ((vector? vec) value (procedure? cmp)) (or integer? boolean?))
   (pure)
   ((cmp (lambda (a b) integer?))))
 
 (vector-any
   (lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) *)
   (pure)
   ((pred? (lambda (obj1 obj2 ...) *))))
 
 (vector-every
   (lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) *)
   (pure)
   ((pred? (lambda (obj1 obj2 ...) *))))
 
 (vector-partition
   (lambda ((procedure? pred?) (vector? vec) ) (values vector? integer?))
   (pure)
   ((pred? (lambda (obj) boolean?))))
 
 (vector-set!
   (lambda ((vector? vector) (integer? k) obj) undefined)
   ())
 
 (vector-swap!
   (lambda ((vector? vector) (integer? i) (integer? j)) undefined)
   ())

 (vector-fill!
   (lambda ((vector? vector) fill) undefined) 
   ())

 (vector-fill!
   (lambda ((vector? vector) fill (integer? start)) undefined) 
   ())

 (vector-fill!
   (lambda ((vector? vector) fill (integer? start) (integer? end)) undefined) 
   ())

 (vector-reverse!
   (lambda ((vector? vector)) undefined) 
   ())

 (vector-reverse!
   (lambda ((vector? vector) (integer? start)) undefined) 
   ())

 (vector-reverse!
   (lambda ((vector? vector) (integer? start) (integer? end)) undefined) 
   ())

 (vector-copy!
   (lambda ((vector? to) (integer? at) (vector? from)) undefined)
   ())

 (vector-copy!
   (lambda ((vector? to) (integer? at) (vector? from) (integer? start)) undefined)
   ())

 (vector-copy!
   (lambda ((vector? to) (integer? at) (vector? from) (integer? start) (integer? end)) undefined)
   ())
 
 (vector-reverse-copy!
   (lambda ((vector? to) (integer? at) (vector? from)) undefined)
   ())

 (vector-reverse-copy!
   (lambda ((vector? to) (integer? at) (vector? from) (integer? start)) undefined)
   ())

 (vector-reverse-copy!
   (lambda ((vector? to) (integer? at) (vector? from) (integer? start) (integer? end)) undefined)
   ())
 
 (vector-unfold!
   (lambda ((procedure? f) (vector? vec) (integer? start) (integer? end) initial-seed ...) undefined)
   ()
   ((f (lambda ((integer? index) seed ...) (values * * ...)))))
 
 (vector-unfold-right!
   (lambda ((procedure? f) (vector? vec) (integer? start) (integer? end) initial-seed ...) undefined)
   ()
   ((f (lambda ((integer? index) seed ...) (values * * ...)))))
 
 (vector->list
   (lambda ((vector? vector)) list?)
   (pure))

 (vector->list
   (lambda ((vector? vector) (integer? start)) list?)
   (pure))

 (vector->list
   (lambda ((vector? vector) (integer? start) (integer? end)) list?)
   (pure))
 
 (reverse-vector->list
   (lambda ((vector? vector)) list?)
   (pure))

 (reverse-vector->list
   (lambda ((vector? vector) (integer? start)) list?)
   (pure))

 (reverse-vector->list
   (lambda ((vector? vector) (integer? start) (integer? end)) list?)
   (pure))
 
 (list->vector
   (lambda ((list? list)) vector?)
   (pure))
 
 (reverse-list->vector
   (lambda ((list? list)) vector?)
   (pure))
 
 (string->vector
   (lambda ((string? string)) vector?)
   (pure))

 (string->vector
   (lambda ((string? string) (integer? start)) vector?)
   (pure))

 (string->vector
   (lambda ((string? string) (integer? start) (integer? end)) vector?)
   (pure))
 
 (vector->string
   (lambda ((vector? vector)) string?)
   (pure))

 (vector->string
   (lambda ((vector? vector) (integer? start)) string?)
   (pure))

 (vector->string
   (lambda ((vector? vector) (integer? start) (integer? end)) string?)
   (pure))
 
 )
