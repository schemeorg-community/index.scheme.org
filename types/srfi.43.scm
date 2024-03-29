(((name . "make-vector")
  (signature
   case-lambda
   (((integer? k)) vector?)
   (((integer? k) fill) vector?))
  (tags pure)
  (desc . "Creates and returns a vector of size size, optionally filling it with fill. The default value of fill is unspecified."))
 ((name . "vector") (signature lambda (x ...) vector?) (tags pure) (desc . "Creates and returns a vector whose elements are x ..."))
 ((name . "vector-unfold")
  (signature
   lambda
   ((procedure? f) (integer? length) initial-seed ...)
   vector?)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
  (tags pure)
  (desc . "The fundamental vector constructor. Creates a vector whose length is length and iterates across each index k between 0 and length, applying f at each iteration to the current index and current seeds, in that order, to receive n + 1 values: first, the element to put in the kth slot of the new vector and n new seeds for the next iteration. It is an error for the number of seeds to vary between iterations."))
 ((name . "vector-unfold-right")
  (signature
   lambda
   ((procedure? f) (integer? length) initial-seed ...)
   vector?)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
  (tags pure)
  (desc . "Like vector-unfold, but it uses f to generate elements from right-to-left, rather than left-to-right."))
 ((name . "vector-copy")
  (signature
   case-lambda
   (((vector? vec)) vector?)
   (((vector? vec) (integer? start)) vector?)
   (((vector? vec) (integer? start) (integer? end)) vector?)
   (((vector? vec) (integer? start) (integer? end) fill) vector?))
  (tags pure)
  (desc . "Allocates a new vector whose length is end - start and fills it with elements from vec, taking elements from vec starting at index start and stopping at index end. start defaults to 0 and end defaults to the value of (vector-length vec). If end extends beyond the length of vec, the slots in the new vector that obviously cannot be filled by elements from vec are filled with fill, whose default value is unspecified."))
 ((name . "vector-reverse-copy")
  (signature
   case-lambda
   (((vector? vec)) vector?)
   (((vector? vec) (integer? start)) vector?)
   (((vector? vec) (integer? start) (integer? end)) vector?))
  (tags pure)
  (desc . "Like vector-copy, but it copies the elements in the reverse order from vec."))
 ((name . "vector-append")
  (signature lambda ((vector? vec) ...) vector?)
  (tags pure)
  (desc . "Returns a newly allocated vector that contains all elements in order from the subsequent locations in vec ..."))
 ((name . "vector-concatenate")
  (signature lambda ((list? list-of-vectors) ...) vector?)
  (subsigs
    (list-of-vectors (list (vector? v))))
  (tags pure)
  (desc . "Appends each vector in list-of-vectors. This is equivalent to: (apply vector-append list-of-vectors) however, it may be implemented better."))
 ((name . "vector?") 
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Disjoint type predicate for vectors: this returns #t if x is a vector, and #f if otherwise."))
 ((name . "vector-empty?")
  (signature lambda ((vector? vec)) boolean?)
  (tags pure)
  (desc . "Returns #t if vec is empty, i.e. its length is 0, and #f if not."))
 ((name . "vector=")
  (signature lambda ((procedure? elt=?) (vector? vec) ...) boolean?)
  (subsigs (elt=? (lambda (a b) boolean?)))
  (tags pure)
  (desc . "Vector structure comparator, generalized across user-specified element comparators. Vectors a and b are considered equal by vector= iff their lengths are the same, and for each respective elements Ea and Eb, (elt=? Ea Eb) returns a true value. Elt=? is always applied to two arguments. Element comparison must be consistent with eq; that is, if (eq? Ea Eb) results in a true value, then (elt=? Ea Eb) must also result in a true value. This may be exploited to avoid unnecessary element comparisons. (The reference implementation does, but it does not consider the situation where elt=? is in fact itself eq? to avoid yet more unnecessary comparisons.)
If there are only zero or one vector arguments, #t is automatically returned. The dynamic order in which comparisons of elements and of vectors are performed is left completely unspecified; do not rely on a particular order."))
 ((name . "vector-ref")
  (signature lambda ((vector? vec) (integer? i)) *)
  (tags pure)
  (desc . "Vector element dereferencing: returns the value that the location in vec at i is mapped to in the store. Indexing is based on zero. I must be within the range [0, (vector-length vec))."))
 ((name . "vector-length")
  (signature lambda ((vector? vec)) integer?)
  (tags pure)
  (desc . "Returns the length of vec, the number of locations reachable from vec. (The careful word 'reachable' is used to allow for 'vector slices,' whereby vec refers to a larger vector that contains more locations that are unreachable from vec. This SRFI does not define vector slices, but later SRFIs may.)"))
 ((name . "vector-fold")
  (signature
   lambda
   ((procedure? kons) knil (vector? vec1) (vector? vec2) ...)
   *)
  (subsigs (kons (lambda ((integer? index) state obj1 obj2 ...) *)))
  (tags pure)
  (desc . "The fundamental vector iterator. Kons is iterated over each index in all of the vectors, stopping at the end of the shortest; kons is applied as (kons i state (vector-ref vec1 i) (vector-ref vec2 i) ···) where state is the current state value — the current state value begins with knil, and becomes whatever kons returned at the respective iteration —, and i is the current index. The iteration is strictly left-to-right."))
 ((name . "vector-fold-right")
  (signature
   lambda
   ((procedure? kons) knil (vector? vec1) (vector? vec2) ...)
   *)
  (subsigs (kons (lambda ((integer? index) state obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Similar to vector-fold, but it iterates right to left instead of left to right."))
 ((name . "vector-map")
  (signature
   lambda
   ((procedure? f) (vector? vec1) (vector? vec2) ...)
   vector?)
  (subsigs (proc (lambda ((integer? index) obj ...) *)))
  (tags pure)
  (desc . "Constructs a new vector of the shortest size of the vector arguments. Each element at index i of the new vector is mapped from the old vectors by (f i (vector-ref vec1 i) (vector-ref vec2 i) ···). The dynamic order of application of f is unspecified."))
 ((name . "vector-map!")
  (signature
   lambda
   ((procedure? f) (vector? vec1) (vector? vec2) ...)
   undefined)
  (subsigs (proc (lambda ((integer? index) obj ...) *)))
  (desc . "Similar to vector-map, but rather than mapping the new elements into a new vector, the new mapped elements are destructively inserted into vec1. Again, the dynamic order of application of f unspecified, so it is dangerous for f to apply either vector-ref or vector-set! to vec1 in f."))
 ((name . "vector-for-each")
  (signature
   lambda
   ((procedure? f) (vector? vec1) (vector? vec2) ...)
   undefined)
  (subsigs (proc (lambda ((integer? index) obj ...) undefined)))
  (desc . "Simple vector iterator: applies f to each index in the range [0, length), where length is the length of the smallest vector argument passed, and the respective list of parallel elements from vec1 vec2 ··· at that index. In contrast with vector-map, f is reliably applied to each subsequent elements, starting at index 0, in the vectors."))
 ((name . "vector-count")
  (signature
   lambda
   ((procedure? pred?) (vector? vec1) (vector? vec2) ...)
   integer?)
  (subsigs (pred? (lambda ((integer? index) obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Counts the number of parallel elements in the vectors that satisfy pred?, which is applied, for each index i in the range [0, length) — where length is the length of the smallest vector argument —, to i and each parallel element in the vectors at that index, in order."))
 ((name . "vector-index")
  (signature
   lambda
   ((procedure? pred?) (vector? vec1) (vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Finds & returns the index of the first elements in vec1 vec2 ... that satisfy pred?. If no matching element is found by the end of the shortest vector, #f is returned."))
 ((name . "vector-index-right")
  (signature
   lambda
   ((procedure? pred?) (vector? vec1) (vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Like vector-index, but it searches right-to-left, rather than left-to-right, and all of the vectors must have the same length."))
 ((name . "vector-skip")
  (signature
   lambda
   ((procedure? pred?) (vector? vec1) (vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Finds & returns the index of the first elements in vec1 vec2 ... that do not satisfy pred?. If all the values in the vectors satisfy pred? until the end of the shortest vector, this returns #f."))
 ((name . "vector-skip-right")
  (signature
   lambda
   ((procedure? pred?) (vector? vec1) (vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Like vector-skip, but it searches for a non-matching element right-to-left, rather than left-to-right, and all of the vectors must have the same length."))
 ((name . "vector-binary-search")
  (signature lambda ((vector? vec) value (procedure? cmp)) (or integer? #f))
  (subsigs (cmp (lambda (a b) integer?)))
  (tags pure)
  (desc . "Similar to vector-index and vector-index-right, but instead of searching left to right or right to left, this performs a binary search. cmp should be a procedure of two arguments and return a negative integer, which indicates that its first argument is less than its second, zero, which indicates that they are equal, or a positive integer, which indicates that the first argument is greater than the second argument."))
 ((name . "vector-any")
  (signature lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Finds the first set of elements in parallel from vec1 vec2 ... for which pred? returns a true value. If such a parallel set of elements exists, vector-any returns the value that pred? returned for that set of elements. The iteration is strictly left-to-right."))
 ((name . "vector-every")
  (signature lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "If, for every index i between 0 and the length of the shortest vector argument, the set of elements (vector-ref vec1 i) (vector-ref vec2 i) ··· satisfies pred?, vector-every returns the value that pred? returned for the last set of elements, at the last index of the shortest vector. The iteration is strictly left-to-right."))
 ((name . "vector-set!")
  (signature lambda ((vector? vector) (integer? i) obj) undefined)
  (desc . "Assigns the contents of the location at i in vec to value."))
 ((name . "vector-swap!")
  (signature lambda ((vector? vector) (integer? i) (integer? j)) undefined)
  (desc . "Swaps or exchanges the values of the locations in vec at i & j."))
 ((name . "vector-fill!")
  (signature
   case-lambda
   (((vector? vec) fill) undefined)
   (((vector? vec) fill (integer? start)) undefined)
   (((vector? vec) fill (integer? start) (integer? end)) undefined))
  (desc . "Assigns the value of every location in vec between start, which defaults to 0 and end, which defaults to the length of vec, to fill."))
 ((name . "vector-reverse!")
  (signature
   case-lambda
   (((vector? vec)) undefined)
   (((vector? vec) (integer? start)) undefined)
   (((vector? vec) (integer? start) (integer? end)) undefined))
  (desc . "Destructively reverses the contents of the sequence of locations in vec between start and end. Start defaults to 0 and end defaults to the length of vec. Note that this does not deeply reverse."))
 ((name . "vector-copy!")
  (signature
   case-lambda
   (((vector? target) (integer? tstart) (vector? source)) undefined)
   (((vector? target) (integer? tstart) (vector? source) (integer? sstart)) undefined)
   (((vector? target) (integer? tstart) (vector? source) (integer? sstart) (integer? send))
    undefined))
  (desc . "Copies a block of elements from source to target, both of which must be vectors, starting in target at tstart and starting in source at sstart, ending when send - sstart elements have been copied. It is an error for target to have a length less than tstart + (send - sstart). Sstart defaults to 0 and send defaults to the length of source."))
 ((name . "vector-reverse-copy!")
  (signature
   case-lambda
   (((vector? target) (integer? tstart) (vector? source)) undefined)
   (((vector? target) (integer? tstart) (vector? source) (integer? sstart)) undefined)
   (((vector? target) (integer? tstart) (vector? source) (integer? sstart) (integer? send))
    undefined))
  (desc . "Like vector-copy!, but this copies the elements in the reverse order. It is an error if target and source are identical vectors and the target & source ranges overlap; however, if tstart = sstart, vector-reverse-copy! behaves as (vector-reverse! target tstart send) would."))
 ((name . "vector->list")
  (signature
   case-lambda
   (((vector? vec)) list?)
   (((vector? vec) (integer? start)) list?)
   (((vector? vec) (integer? start) (integer? end)) list?))
  (tags pure)
  (desc . "Creates a list containing the elements in vec between start, which defaults to 0, and end, which defaults to the length of vec."))
 ((name . "reverse-vector->list")
  (signature
   case-lambda
   (((vector? vec)) list?)
   (((vector? vec) (integer? start)) list?)
   (((vector? vec) (integer? start) (integer? end)) list?))
  (tags pure)
  (desc . "Like vector->list, but the resulting list contains the elements in reverse between the the specified range."))
 ((name . "list->vector")
  (signature lambda ((list? list)) vector?)
  (tags pure)
  (desc . "Creates a vector of elements from proper-list."))
 ((name . "reverse-list->vector")
  (signature lambda ((list? list)) vector?)
  (tags pure)
  (desc . "Like list->vector, but the resulting list contains the elements in reverse of proper-list.")))
