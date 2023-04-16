(
((name . "make-s32vector")
 (signature
   case-lambda
   (((integer? size)) s32vector?)
   (((integer? size) (s32? fill)) s32vector?))
 (tags pure)
 (desc . "Returns a s32vector whose length is size. If fill is provided, all the elements of the s32vector are initialized to it."))
((name . "s32vector") 
 (signature lambda ((s32? value) ...) s32vector?)
 (tags pure)
 (desc . "Returns a s32vector initialized with values."))
((name . "s32vector?") 
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a s32vector, and #f otherwise."))
((name . "s32vector-length")
 (signature lambda ((s32vector? s32vec)) integer?)
 (tags pure)
 (desc . "Returns the length of s32vec"))
((name . "s32vector-ref")
 (signature lambda ((s32vector? s32vec) (integer? i)) s32?)
 (tags pure)
 (desc . "Returns the ith element of s32vec."))
((name . "s32vector-set!")
 (signature lambda ((s32vector? s32vec) (integer? i) (s32? value)) undefined)
 (desc . "Sets the ith element of s32vec to value."))
((name . "s32vector->list")
 (signature
   case-lambda
   (((s32vector? s32vec)) list?)
   (((s32vector? s32vec) (integer? start)) list?)
   (((s32vector? s32vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as s32vec"))
((name . "list->s32vector")
 (signature lambda ((list? proper-list)) s32vector?)
 (subsigs
   (proper-list (list s32?)))
 (tags pure)
 (desc . "Returns s32vector with same elements as list."))
((name . "s32?")
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a valid element of an s32vector (ie., signed exact integer in the range -(2^31) to (2^31)-1), and #f otherwise."))
((name . "s32vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) s32vector?)
   (((procedure? f) (integer? length) seed) s32vector?))
 (subsigs (f (lambda ((integer? index) state) (values s32? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "s32vector-copy")
 (signature
   case-lambda
   (((s32vector? s32vec)) s32vector?)
   (((s32vector? s32vec) (integer? start)) s32vector?)
   (((s32vector? s32vec) (integer? start) (integer? end)) s32vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of s32vec from start to end and returns it."))
((name . "s32vector-reverse-copy")
 (signature
   case-lambda
   (((s32vector? s32vec)) s32vector?)
   (((s32vector? s32vec) (integer? start)) s32vector?)
   (((s32vector? s32vec) (integer? start) (integer? end)) s32vector?))
 (tags pure)
 (desc . "The same as s32vector-copy, but in reverse order."))
((name . "s32vector-append")
 (signature lambda ((s32vector? s32vec) ...) s32vector?)
 (tags pure)
 (desc . "Returns a s32vector containing all the elements of the s32vecs in order."))
((name . "s32vector-concatenate")
 (signature lambda ((list? list-of-s32vectors)) s32vector?)
 (subsigs
   (list-of-s32vectors (list s32vector?)))
 (tags pure)
 (desc . "The same as s32vector-append, but takes a list of s32vectors rather than multiple arguments."))
((name . "s32vector-append-subvectors")
 (signature
   lambda
   ((s32vector? s32vec1) (integer? start1) (integer? end1) ...)
   s32vector?)
 (tags pure)
 (desc . "Concatenates the result of applying s32vector-copy to each triplet of s32vec, start, end arguments, but may be implemented more efficiently."))
((name . "s32vector-empty?")
 (signature lambda ((s32vector? s32vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if s32vec has a length of zero, and #f otherwise."))
((name . "s32vector=")
 (signature lambda ((s32vector? s32vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the s32vecs for elementwise equality, using = to do the comparisons. Returns #f unless all s32vectors are the same length."))
((group
   ((name . "s32vector-take")
    (signature lambda ((s32vector? s32vec) (integer? n)) s32vector?)
    (tags pure))
   ((name . "s32vector-take-right")
    (signature lambda ((s32vector? s32vec) (integer? n)) s32vector?)
    (tags pure)))
 (desc . "Returns a s32vector containing the first/last n elements of s32vec."))
((group
   ((name . "s32vector-drop")
    (signature lambda ((s32vector? s32vec) (integer? n)) s32vector?)
    (tags pure))
   ((name . "s32vector-drop-right")
    (signature lambda ((s32vector? s32vec) (integer? n)) s32vector?)
    (tags pure)))
 (desc . "Returns a s32vector containing all except the first/last n elements of s32vec."))
((name . "s32vector-segment")
 (signature lambda ((s32vector? s32vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of s32vectors, each of which contains n consecutive elements of s32vec. The last s32vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "s32vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (s32vector? s32vec1) (s32vector? s32vec2) ...)
      *)
    (subsigs (kons (lambda (state (s32? obj1) (s32? obj2) ...) *)))
    (tags pure))
   ((name . "s32vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (s32vector? s32vec1) (s32vector? s32vec2) ...)
      *)
    (subsigs (kons (lambda (state (s32? obj1) (s32? obj2) ...) *)))
    (tags pure)))
 (desc . "When one s32vector argument s32vec is given, folds kons over the elements of s32vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple s32vector arguments are given, kons is called with the current state value and each value from all the vectors; s32vector-fold scans elements from left to right, while s32vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "s32vector-map")
    (signature
      lambda
      ((procedure? proc) (s32vector? s32vec1) (s32vector? s32vec2) ...)
      vector?)
    (subsigs (proc (lambda ((s32? val1) ...) *)))
    (tags pure))
   ((name . "s32vector-map!")
    (signature
      lambda
      ((procedure? proc) (s32vector? s32vec1) (s32vector? s32vec2) ...)
      undefined)
    (subsigs (proc (lambda ((s32? val1) ...) *))))
   ((name . "s32vector-for-each")
    (signature
      lambda
      ((procedure? proc) (s32vector? s32vec1) (s32vector? s32vec2) ...)
      undefined)
    (subsigs (proc (lambda ((s32? val1) ...) undefined)))))
 (desc . "Iterate over the elements of s32vec and apply f to each, returning respectively a s32vector of the results, an undefined value with the results placed back in s32vec, and an undefined value with no change to s32vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For s32vector-map!, only s32vec is modified even when multiple vectors are passed.
If s32vector-map or s32vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "s32vector-count")
 (signature
   lambda
   ((procedure? pred?) (s32vector? s32vec1) (s32vector? s32vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((s32? val1) (s32? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of s32vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "s32vector-cumulate")
 (signature lambda ((procedure? f) knil (s32vector? s32vec)) s32vector?)
 (subsigs (f (lambda (state (s32? value)) *)))
 (tags pure)
 (desc . "Like s32vector-fold, but returns an s32vector of partial results rather than just the final result."))
((group
   ((name . "s32vector-take-while")
    (signature lambda ((procedure? pred?) (s32vector? s32vec)) s32vector?)
    (subsigs (pred? (lambda ((s32? value)) boolean?)))
    (tags pure))
   ((name . "s32vector-take-while-right")
    (signature lambda ((procedure? pred?) (s32vector? s32vec)) s32vector?)
    (subsigs (pred? (lambda ((s32? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of s32vec all of whose elements satisfy pred?."))
((group
   ((name . "s32vector-drop-while")
    (signature lambda ((procedure? pred?) (s32vector? s32vec)) s32vector?)
    (subsigs (pred? (lambda ((s32? value)) boolean?)))
    (tags pure))
   ((name . "s32vector-drop-while-right")
    (signature lambda ((procedure? pred?) (s32vector? s32vec)) s32vector?)
    (subsigs (pred? (lambda ((s32? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of s32vec such that all its elements satisfy pred."))
((group
   ((name . "s32vector-index")
    (signature
      lambda
      ((procedure? pred?) (s32vector? s32vec1) (s32vector? s32vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s32? value1) (s32? value2) ...) *)))
    (tags pure))
   ((name . "s32vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (s32vector? s32vec1) (s32vector? s32vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s32? value1) (s32? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of s32vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, s32vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for s32vector-index-right."))
((group
   ((name . "s32vector-skip")
    (signature
      lambda
      ((procedure? pred?) (s32vector? s32vec1) (s32vector? s32vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s32? value1) (s32? value2) ...) *)))
    (tags pure))
   ((name . "s32vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (s32vector? s32vec1) (s32vector? s32vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s32? value1) (s32? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of s32vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, s32vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for s32vector-skip-right."))
((name . "s32vector-any")
 (signature lambda ((procedure? pred?) (s32vector? s32vec1) (s32vector? s32vec2) ...) *)
 (subsigs (pred? (lambda ((s32? val1) (s32? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the s32vec, or #f if there is no such element. If s32vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "s32vector-every")
 (signature lambda ((procedure? pred?) (s32vector? s32vec1) (s32vector? s32vec2) ...) *)
 (subsigs (pred? (lambda ((s32? val1) (s32? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from s32vec satisfy pred?, return the last result of pred?. If not all do, return #f. If s32vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "s32vector-partition")
 (signature
   lambda
   ((procedure? pred?) (s32vector? s32vec))
   (values s32vector? integer?))
 (subsigs (pred? (lambda ((s32? value)) boolean?)))
 (tags pure)
 (desc . "Returns an s32vector of the same type as s32vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new s32vector and the number of elements satisfying pred?."))
((group
   ((name . "s32vector-filter")
    (signature lambda ((procedure? pred?) (s32vector? s32vec1)) s32vector?)
    (subsigs (pred? (lambda ((s32? val)) boolean?)))
    (tags pure))
   ((name . "s32vector-remove")
    (signature lambda ((procedure? pred?) (s32vector? s32vec1)) s32vector?)
    (subsigs (pred? (lambda ((s32? val)) boolean?)))
    (tags pure)))
 (desc . "Return an s32vector containing the elements of s32vec that satisfy / do not satisfy pred?."))
((name . "s32vector-swap!")
 (signature lambda ((s32vector? s32vector) (integer? i) (integer? j)) undefined))
((name . "s32vector-fill!")
 (signature
   case-lambda
   (((s32vector? s32vec) (s32? fill)) undefined)
   (((s32vector? s32vec) (s32? fill) (integer? start)) undefined)
   (((s32vector? s32vec) (s32? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of s32vec from start to end with the value fill."))
((name . "s32vector-reverse!")
 (signature
   case-lambda
   (((s32vector? s32vec)) undefined)
   (((s32vector? s32vec) (integer? start)) undefined)
   (((s32vector? s32vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of s32vec from start to end."))
((name . "s32vector-copy!")
 (signature
   case-lambda
   (((s32vector? s32to) (integer? at) (s32vector? s32from)) undefined)
   (((s32vector? s32to) (integer? at) (s32vector? s32from) (integer? start)) undefined)
   (((s32vector? s32to)
     (integer? at)
     (s32vector? s32from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of s32from from start to end onto s32to, starting at index at."))
((name . "s32vector-reverse-copy!")
 (signature
   case-lambda
   (((s32vector? s32to) (integer? at) (s32vector? s32from)) undefined)
   (((s32vector? s32to) (integer? at) (s32vector? s32from) (integer? start)) undefined)
   (((s32vector? s32to)
     (integer? at)
     (s32vector? s32from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as s32vector-copy!, but copies in reverse"))
((name . "s32vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (s32vector? s32vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like s32vector-unfold, but the elements are copied into the vector s32vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "s32vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (s32vector? s32vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as s32vector-unfold!, but initializes the s32vector from right to left."))
((name . "reverse-s32vector->list")
 (signature
   case-lambda
   (((s32vector? s32vec)) list?)
   (((s32vector? s32vec) (integer? start)) list?)
   (((s32vector? s32vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as s32vec in reverse order."))
((name . "reverse-list->s32vector")
 (signature lambda ((list? proper-list)) s32vector?)
 (tags pure)
 (desc . "Returns s32vector with same elements as list in reverse order."))
((name . "s32vector->vector")
 (signature
   case-lambda
   (((s32vector? s32vec)) vector?)
   (((s32vector? s32vec) (integer? start)) vector?)
   (((s32vector? s32vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as s32vec."))
((name . "vector->s32vector")
 (signature
   case-lambda
   (((vector? vec)) s32vector?)
   (((vector? vec) (integer? start)) s32vector?)
   (((vector? vec) (integer? start) (integer? end)) s32vector?))
 (tags pure)
 (desc . "Returns s32vector with same elements as vec."))
((name . "make-s32vector-generator")
 (signature lambda ((s32vector? s32vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? s32?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of s32vector in order. Note that the generator is finite."))
((name . "s32vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of s32vector."))
((name . "write-s32vector")
 (signature
   case-lambda
   (((s32vector s32vec)) undefined)
   (((s32vector s32vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of s32vec in the lexical syntax explained below."))
)
