(((name . "make-vector")
  (signature
   case-lambda
   (((integer? k)) vector?)
   (((integer? k) fill) vector?))
  (tags pure)
  (desc . "Creates and returns a vector of size size. If fill is specified, all the elements of the vector are initialized to fill. Otherwise, their contents are indeterminate."))
 ((name . "vector")
  (signature lambda (obj ...) vector?)
  (tags pure)
  (desc . "Creates and returns a vector whose elements are x ...."))
 ((name . "vector-unfold")
  (signature
   lambda
   ((procedure? f) (integer? length) initial-seed ...)
   vector?)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
  (tags pure)
  (desc . "The fundamental vector constructor. Creates a vector whose length is length and iterates across each index k between 0 and length, applying f at each iteration to the current index and current seeds, in that order, to receive n + 1 values: first, the element to put in the kth slot of the new vector and n new seeds for the next iteration. It is an error for the number of seeds to vary between iterations. Note that the termination condition is different from the unfold procedure of SRFI 1."))
 ((name . "vector-unfold-right")
  (signature
   lambda
   ((procedure? f) (integer? length) initial-seed ...)
   vector?)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
  (tags pure)
  (desc . "Like vector-unfold, but it uses f to generate elements from right-to-left, rather than left-to-right. The first index used is length - 1. Note that the termination condition is different from the unfold-right procedure of SRFI 1."))
 ((name . "vector-copy")
  (signature
   case-lambda
   (((vector? vector)) vector?)
   (((vector? vector) (integer? start)) vector?)
   (((vector? vector) (integer? start) (integer? end)) vector?))
  (tags pure)
  (desc . "Allocates a new vector whose length is end - start and fills it with elements from vec, taking elements from vec starting at index start and stopping at index end. Start defaults to 0 and end defaults to the value of (vector-length vec). SRFI 43 provides an optional fill argument to supply values if end is greater than the length of vec. Neither R7RS-small nor this SRFI requires support for this argument."))
 ((name . "vector-reverse-copy")
  (signature
   case-lambda
   (((vector? vector)) vector?)
   (((vector? vector) (integer? start)) vector?)
   (((vector? vector) (integer? start) (integer? end)) vector?))
  (tags pure)
  (desc . "Like vector-copy, but it copies the elements in the reverse order from vec."))
 ((name . "vector-append")
  (signature lambda ((vector? vector) ...) vector?)
  (tags pure)
  (desc . "Returns a newly allocated vector that contains all elements in order from the subsequent locations in vec ...."))
 ((name . "vector-concatenate")
  (signature lambda ((list? list-of-vectors) ...) vector?)
  (subsigs
   (list-of-vectors (list (vector? vec))))
  (tags pure)
  (desc . "Appends each vector in list-of-vectors."))
 ((name . "vector-append-subvectors")
  (signature
   lambda
   ((vector? vec1) (integer? start1) (integer? end1) ...)
   vector?)
  (tags pure)
  (desc . "Returns a vector that contains every element of each vec from start to end in the specified order. This procedure is a generalization of vector-append."))
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
  (desc . "Vector structure comparator, generalized across user-specified element comparators. Vectors a and b are considered equal by vector= iff their lengths are the same, and for each respective element Ea and Eb, (elt=? Ea Eb) returns a true value. Elt=? is always applied to two arguments.
If there are only zero or one vector arguments, #t is automatically returned. The dynamic order in which comparisons of elements and of vectors are performed is left completely unspecified; do not rely on a particular order."))
 ((name . "vector-ref")
  (signature lambda ((vector? vector) (integer? k)) *)
  (tags pure)
  (desc . "Vector element dereferencing: returns the value that the location in vec at i is mapped to in the store. Indexing is based on zero. I must be within the range [0, (vector-length vec))."))
 ((name . "vector-length")
  (signature lambda ((vector? vector)) integer?)
  (tags pure)
  (desc . "Returns the length of vec, the number of locations reachable from vec. (The careful word 'reachable' is used to allow for 'vector slices,' whereby vec refers to a larger vector that contains more locations that are unreachable from vec. This SRFI does not define vector slices, but later SRFIs may.)"))
 ((name . "vector-fold")
  (signature
   lambda
   ((procedure? kons) knil (vector? vec1) (vector? vec2) ...)
   *)
  (subsigs (kons (lambda (state obj1 obj2 ...) *)))
  (tags pure)
  (desc . "The fundamental vector iterator. Kons is iterated over each value in all of the vectors, stopping at the end of the shortest; kons is applied as (kons state (vector-ref vec1 i) (vector-ref vec2 i) ...) where state is the current state value — the current state value begins with knil, and becomes whatever kons returned on the previous iteration —, and i is the current index.
The iteration is strictly left-to-right."))
 ((name . "vector-fold-right")
  (signature
   lambda
   ((procedure? kons) knil (vector? vec1) (vector? vec2) ...)
   *)
  (subsigs (kons (lambda (state obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Similar to vector-fold, but it iterates right to left instead of left to right."))
 ((name . "vector-map")
  (signature
   lambda
   ((procedure? proc) (vector? vector1) (vector? vector2) ...)
   vector?)
  (subsigs (proc (lambda (obj ...) *)))
  (tags pure)
  (desc . "Constructs a new vector of the shortest size of the vector arguments. Each element at index i of the new vector is mapped from the old vectors by (f (vector-ref vec1 i) (vector-ref vec2 i) ...). The dynamic order of application of f is unspecified."))
 ((name . "vector-map!")
  (signature
   lambda
   ((procedure? proc) (vector? vector1) (vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) *)))
  (desc . "Similar to vector-map, but rather than mapping the new elements into a new vector, the new mapped elements are destructively inserted into vec1. Again, the dynamic order of application of f unspecified, so it is dangerous for f to apply either vector-ref or vector-set! to vec1 in f."))
 ((name . "vector-for-each")
  (signature
   lambda
   ((procedure? proc) (vector? vector1) (vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) undefined)))
  (desc . "Simple vector iterator: applies f to the corresponding list of parallel elements from vec1 vec2 ... in the range [0, length), where length is the length of the smallest vector argument passed, In contrast with vector-map, f is reliably applied to each subsequent element, starting at index 0, in the vectors."))
 ((name . "vector-count")
  (signature
   lambda
   ((procedure? pred?) (vector? vec1) (vector? vec2) ...)
   integer?)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Counts the number of parallel elements in the vectors that satisfy pred?, which is applied, for each index i in the range [0, length) where length is the length of the smallest vector argument, to each parallel element in the vectors, in order."))
 ((name . "vector-cumulate")
  (signature lambda ((procedure? f) knil (vector? vec)) vector?)
  (subsigs (f (lambda (obj1 obj2) *)))
  (tags pure)
  (desc . "Returns a newly allocated vector new with the same length as vec. Each element i of new is set to the result of invoking f on newi-1 and veci, except that for the first call on f, the first argument is knil. The new vector is returned.
Note that the order of arguments to vector-cumulate was changed by errata-3 on 2016-09-02."))
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
  (desc . "Like vector-skip, but it searches for a non-matching element right-to-left, rather than left-to-right, and it is an error if all of the vectors do not have the same length."))
 ((name . "vector-binary-search")
  (signature lambda ((vector? vec) value (procedure? cmp)) (or integer? #f))
  (subsigs (cmp (lambda (a b) integer?)))
  (tags pure)
  (desc . "Similar to vector-index and vector-index-right, but instead of searching left to right or right to left, this performs a binary search. If there is more than one element of vec that matches value in the sense of cmp, vector-binary-search may return the index of any of them.
cmp should be a procedure of two arguments and return a negative integer, which indicates that its first argument is less than its second, zero, which indicates that they are equal, or a positive integer, which indicates that the first argument is greater than the second argument."))
 ((name . "vector-any")
  (signature lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Finds the first set of elements in parallel from vec1 vec2 ... for which pred? returns a true value. If such a parallel set of elements exists, vector-any returns the value that pred? returned for that set of elements. The iteration is strictly left-to-right."))
 ((name . "vector-every")
  (signature lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "If, for every index i between 0 and the length of the shortest vector argument, the set of elements (vector-ref vec1 i) (vector-ref vec2 i) ... satisfies pred?, vector-every returns the value that pred? returned for the last set of elements, at the last index of the shortest vector. The iteration is strictly left-to-right."))
 ((name . "vector-partition")
  (signature
   lambda
   ((procedure? pred?) (vector? vec))
   (values vector? integer?))
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure)
  (desc . "A vector the same size as vec is newly allocated and filled with all the elements of vec that satisfy pred? in their original order followed by all the elements that do not satisfy pred?, also in their original order.
Two values are returned, the newly allocated vector and the index of the leftmost element that does not satisfy pred?."))
 ((name . "vector-set!")
  (signature lambda ((vector? vector) (integer? k) obj) undefined)
  (desc . "Assigns the contents of the location at i in vec to value."))
 ((name . "vector-swap!")
  (signature lambda ((vector? vector) (integer? i) (integer? j)) undefined)
  (desc . "Swaps or exchanges the values of the locations in vec at i & j."))
 ((name . "vector-fill!")
  (signature
   case-lambda
   (((vector? vector) fill) undefined)
   (((vector? vector) fill (integer? start)) undefined)
   (((vector? vector) fill (integer? start) (integer? end)) undefined))
  (desc . "Assigns the value of every location in vec between start, which defaults to 0 and end, which defaults to the length of vec, to fill."))
 ((name . "vector-reverse!")
  (signature
   case-lambda
   (((vector? vector)) undefined)
   (((vector? vector) (integer? start)) undefined)
   (((vector? vector) (integer? start) (integer? end)) undefined))
  (desc . "Destructively reverses the contents of the sequence of locations in vec between start and end. Start defaults to 0 and end defaults to the length of vec. Note that this does not deeply reverse."))
 ((name . "vector-copy!")
  (signature
   case-lambda
   (((vector? to) (integer? at) (vector? from)) undefined)
   (((vector? to) (integer? at) (vector? from) (integer? start)) undefined)
   (((vector? to) (integer? at) (vector? from) (integer? start) (integer? end))
    undefined))
  (desc . "Copies the elements of vector from between start and end to vector to, starting at at. The order in which elements are copied is unspecified, except that if the source and destination overlap, copying takes place as if the source is first copied into a temporary vector and then into the destination. This can be achieved without allocating storage by making sure to copy in the correct direction in such circumstances."))
 ((name . "vector-reverse-copy!")
  (signature
   case-lambda
   (((vector? to) (integer? at) (vector? from)) undefined)
   (((vector? to) (integer? at) (vector? from) (integer? start)) undefined)
   (((vector? to) (integer? at) (vector? from) (integer? start) (integer? end))
    undefined))
  (desc . "Like vector-copy!, but the elements appear in to in reverse order."))
 ((name . "vector-unfold!")
  (signature
   lambda
   ((procedure? f)
    (vector? vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
  (desc . "Like vector-unfold, but the elements are copied into the vector vec starting at element start rather than into a newly allocated vector. Terminates when end-start elements have been generated."))
 ((name . "vector-unfold-right!")
  (signature
   lambda
   ((procedure? f)
    (vector? vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
  (desc . "Like vector-unfold!, but the elements are copied in reverse order into the vector vec starting at the index preceding end."))
 ((name . "vector->list")
  (signature
   case-lambda
   (((vector? vector)) list?)
   (((vector? vector) (integer? start)) list?)
   (((vector? vector) (integer? start) (integer? end)) list?))
  (tags pure)
  (desc . "Creates a list containing the elements in vec between start, which defaults to 0, and end, which defaults to the length of vec."))
 ((name . "reverse-vector->list")
  (signature
   case-lambda
   (((vector? vector)) list?)
   (((vector? vector) (integer? start)) list?)
   (((vector? vector) (integer? start) (integer? end)) list?))
  (tags pure)
  (desc . "Like vector->list, but the resulting list contains the elements in reverse of vec."))
 ((name . "list->vector")
  (signature lambda ((list? list)) vector?)
  (tags pure)
  (desc . "Creates a vector of elements from proper-list."))
 ((name . "reverse-list->vector")
  (signature lambda ((list? list)) vector?)
  (tags pure)
  (desc . "Like list->vector, but the resulting vector contains the elements in reverse of proper-list."))
 ((name . "string->vector")
  (signature
   case-lambda
   (((string? string)) vector?)
   (((string? string) (integer? start)) vector?)
   (((string? string) (integer? start) (integer? end)) vector?))
  (tags pure)
  (desc . "Creates a vector containing the elements in string between start, which defaults to 0, and end, which defaults to the length of string."))
 ((name . "vector->string")
  (signature
   case-lambda
   (((vector? vector)) string?)
   (((vector? vector) (integer? start)) string?)
   (((vector? vector) (integer? start) (integer? end)) string?))
  (tags pure)
  (desc . "Creates a string containing the elements in vec between start, which defaults to 0, and end, which defaults to the length of vec. It is an error if the elements are not characters.")))
