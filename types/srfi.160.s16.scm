(
((name . "make-s16vector")
 (signature
   case-lambda
   (((integer? size)) s16vector?)
   (((integer? size) (s16? fill)) s16vector?))
 (tags pure)
 (desc . "Returns a s16vector whose length is size. If fill is provided, all the elements of the s16vector are initialized to it."))
((name . "s16vector") 
 (signature lambda ((s16? value) ...) s16vector?)
 (tags pure)
 (desc . "Returns a s16vector initialized with values."))
((name . "s16vector?") 
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a s16vector, and #f otherwise."))
((name . "s16vector-length")
 (signature lambda ((s16vector? s16vec)) integer?)
 (tags pure)
 (desc . "Returns the length of s16vec"))
((name . "s16vector-ref")
 (signature lambda ((s16vector? s16vec) (integer? i)) s16?)
 (tags pure)
 (desc . "Returns the ith element of s16vec."))
((name . "s16vector-set!")
 (signature lambda ((s16vector? s16vec) (integer? i) (s16? value)) undefined)
 (desc . "Sets the ith element of s16vec to value."))
((name . "s16vector->list")
 (signature
   case-lambda
   (((s16vector? s16vec)) list?)
   (((s16vector? s16vec) (integer? start)) list?)
   (((s16vector? s16vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as s16vec"))
((name . "list->s16vector")
 (signature lambda ((list? proper-list)) s16vector?)
 (subsigs
   (proper-list (list s16?)))
 (tags pure)
 (desc . "Returns s16vector with same elements as list."))
((name . "s16?")
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a valid element of an s16vector (ie., signed exact integer in the range -(2^15) to (2^15)-1), and #f otherwise."))
((name . "s16vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) s16vector?)
   (((procedure? f) (integer? length) seed) s16vector?))
 (subsigs (f (lambda ((integer? index) state) (values s16? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "s16vector-copy")
 (signature
   case-lambda
   (((s16vector? s16vec)) s16vector?)
   (((s16vector? s16vec) (integer? start)) s16vector?)
   (((s16vector? s16vec) (integer? start) (integer? end)) s16vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of s16vec from start to end and returns it."))
((name . "s16vector-reverse-copy")
 (signature
   case-lambda
   (((s16vector? s16vec)) s16vector?)
   (((s16vector? s16vec) (integer? start)) s16vector?)
   (((s16vector? s16vec) (integer? start) (integer? end)) s16vector?))
 (tags pure)
 (desc . "The same as s16vector-copy, but in reverse order."))
((name . "s16vector-append")
 (signature lambda ((s16vector? s16vec) ...) s16vector?)
 (tags pure)
 (desc . "Returns a s16vector containing all the elements of the s16vecs in order."))
((name . "s16vector-concatenate")
 (signature lambda ((list? list-of-s16vectors)) s16vector?)
 (subsigs
   (list-of-s16vectors (list s16vector?)))
 (tags pure)
 (desc . "The same as s16vector-append, but takes a list of s16vectors rather than multiple arguments."))
((name . "s16vector-append-subvectors")
 (signature
   lambda
   ((s16vector? s16vec1) (integer? start1) (integer? end1) ...)
   s16vector?)
 (tags pure)
 (desc . "Concatenates the result of applying s16vector-copy to each triplet of s16vec, start, end arguments, but may be implemented more efficiently."))
((name . "s16vector-empty?")
 (signature lambda ((s16vector? s16vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if s16vec has a length of zero, and #f otherwise."))
((name . "s16vector=")
 (signature lambda ((s16vector? s16vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the s16vecs for elementwise equality, using = to do the comparisons. Returns #f unless all s16vectors are the same length."))
((group
   ((name . "s16vector-take")
    (signature lambda ((s16vector? s16vec) (integer? n)) s16vector?)
    (tags pure))
   ((name . "s16vector-take-right")
    (signature lambda ((s16vector? s16vec) (integer? n)) s16vector?)
    (tags pure)))
 (desc . "Returns a s16vector containing the first/last n elements of s16vec."))
((group
   ((name . "s16vector-drop")
    (signature lambda ((s16vector? s16vec) (integer? n)) s16vector?)
    (tags pure))
   ((name . "s16vector-drop-right")
    (signature lambda ((s16vector? s16vec) (integer? n)) s16vector?)
    (tags pure)))
 (desc . "Returns a s16vector containing all except the first/last n elements of s16vec."))
((name . "s16vector-segment")
 (signature lambda ((s16vector? s16vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of s16vectors, each of which contains n consecutive elements of s16vec. The last s16vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "s16vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (s16vector? s16vec1) (s16vector? s16vec2) ...)
      *)
    (subsigs (kons (lambda (state (s16? obj1) (s16? obj2) ...) *)))
    (tags pure))
   ((name . "s16vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (s16vector? s16vec1) (s16vector? s16vec2) ...)
      *)
    (subsigs (kons (lambda (state (s16? obj1) (s16? obj2) ...) *)))
    (tags pure)))
 (desc . "When one s16vector argument s16vec is given, folds kons over the elements of s16vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple s16vector arguments are given, kons is called with the current state value and each value from all the vectors; s16vector-fold scans elements from left to right, while s16vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "s16vector-map")
    (signature
      lambda
      ((procedure? proc) (s16vector? s16vec1) (s16vector? s16vec2) ...)
      vector?)
    (subsigs (proc (lambda ((s16? val1) ...) *)))
    (tags pure))
   ((name . "s16vector-map!")
    (signature
      lambda
      ((procedure? proc) (s16vector? s16vec1) (s16vector? s16vec2) ...)
      undefined)
    (subsigs (proc (lambda ((s16? val1) ...) *))))
   ((name . "s16vector-for-each")
    (signature
      lambda
      ((procedure? proc) (s16vector? s16vec1) (s16vector? s16vec2) ...)
      undefined)
    (subsigs (proc (lambda ((s16? val1) ...) undefined)))))
 (desc . "Iterate over the elements of s16vec and apply f to each, returning respectively a s16vector of the results, an undefined value with the results placed back in s16vec, and an undefined value with no change to s16vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For s16vector-map!, only s16vec is modified even when multiple vectors are passed.
If s16vector-map or s16vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "s16vector-count")
 (signature
   lambda
   ((procedure? pred?) (s16vector? s16vec1) (s16vector? s16vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((s16? val1) (s16? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of s16vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "s16vector-cumulate")
 (signature lambda ((procedure? f) knil (s16vector? s16vec)) s16vector?)
 (subsigs (f (lambda (state (s16? value)) *)))
 (tags pure)
 (desc . "Like s16vector-fold, but returns an s16vector of partial results rather than just the final result."))
((group
   ((name . "s16vector-take-while")
    (signature lambda ((procedure? pred?) (s16vector? s16vec)) s16vector?)
    (subsigs (pred? (lambda ((s16? value)) boolean?)))
    (tags pure))
   ((name . "s16vector-take-while-right")
    (signature lambda ((procedure? pred?) (s16vector? s16vec)) s16vector?)
    (subsigs (pred? (lambda ((s16? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of s16vec all of whose elements satisfy pred?."))
((group
   ((name . "s16vector-drop-while")
    (signature lambda ((procedure? pred?) (s16vector? s16vec)) s16vector?)
    (subsigs (pred? (lambda ((s16? value)) boolean?)))
    (tags pure))
   ((name . "s16vector-drop-while-right")
    (signature lambda ((procedure? pred?) (s16vector? s16vec)) s16vector?)
    (subsigs (pred? (lambda ((s16? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of s16vec such that all its elements satisfy pred."))
((group
   ((name . "s16vector-index")
    (signature
      lambda
      ((procedure? pred?) (s16vector? s16vec1) (s16vector? s16vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s16? value1) (s16? value2) ...) *)))
    (tags pure))
   ((name . "s16vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (s16vector? s16vec1) (s16vector? s16vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s16? value1) (s16? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of s16vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, s16vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for s16vector-index-right."))
((group
   ((name . "s16vector-skip")
    (signature
      lambda
      ((procedure? pred?) (s16vector? s16vec1) (s16vector? s16vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s16? value1) (s16? value2) ...) *)))
    (tags pure))
   ((name . "s16vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (s16vector? s16vec1) (s16vector? s16vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s16? value1) (s16? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of s16vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, s16vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for s16vector-skip-right."))
((name . "s16vector-any")
 (signature lambda ((procedure? pred?) (s16vector? s16vec1) (s16vector? s16vec2) ...) *)
 (subsigs (pred? (lambda ((s16? val1) (s16? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the s16vec, or #f if there is no such element. If s16vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "s16vector-every")
 (signature lambda ((procedure? pred?) (s16vector? s16vec1) (s16vector? s16vec2) ...) *)
 (subsigs (pred? (lambda ((s16? val1) (s16? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from s16vec satisfy pred?, return the last result of pred?. If not all do, return #f. If s16vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "s16vector-partition")
 (signature
   lambda
   ((procedure? pred?) (s16vector? s16vec))
   (values s16vector? integer?))
 (subsigs (pred? (lambda ((s16? value)) boolean?)))
 (tags pure)
 (desc . "Returns an s16vector of the same type as s16vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new s16vector and the number of elements satisfying pred?."))
((group
   ((name . "s16vector-filter")
    (signature lambda ((procedure? pred?) (s16vector? s16vec1)) s16vector?)
    (subsigs (pred? (lambda ((s16? val)) boolean?)))
    (tags pure))
   ((name . "s16vector-remove")
    (signature lambda ((procedure? pred?) (s16vector? s16vec1)) s16vector?)
    (subsigs (pred? (lambda ((s16? val)) boolean?)))
    (tags pure)))
 (desc . "Return an s16vector containing the elements of s16vec that satisfy / do not satisfy pred?."))
((name . "s16vector-swap!")
 (signature lambda ((s16vector? s16vec) (integer? i) (integer? j)) undefined)
 (desc . "Interchanges the ith and jth elements of s16vec."))
((name . "s16vector-fill!")
 (signature
   case-lambda
   (((s16vector? s16vec) (s16? fill)) undefined)
   (((s16vector? s16vec) (s16? fill) (integer? start)) undefined)
   (((s16vector? s16vec) (s16? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of s16vec from start to end with the value fill."))
((name . "s16vector-reverse!")
 (signature
   case-lambda
   (((s16vector? s16vec)) undefined)
   (((s16vector? s16vec) (integer? start)) undefined)
   (((s16vector? s16vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of s16vec from start to end."))
((name . "s16vector-copy!")
 (signature
   case-lambda
   (((s16vector? s16to) (integer? at) (s16vector? s16from)) undefined)
   (((s16vector? s16to) (integer? at) (s16vector? s16from) (integer? start)) undefined)
   (((s16vector? s16to)
     (integer? at)
     (s16vector? s16from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of s16from from start to end onto s16to, starting at index at."))
((name . "s16vector-reverse-copy!")
 (signature
   case-lambda
   (((s16vector? s16to) (integer? at) (s16vector? s16from)) undefined)
   (((s16vector? s16to) (integer? at) (s16vector? s16from) (integer? start)) undefined)
   (((s16vector? s16to)
     (integer? at)
     (s16vector? s16from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as s16vector-copy!, but copies in reverse"))
((name . "s16vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (s16vector? s16vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like s16vector-unfold, but the elements are copied into the vector s16vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "s16vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (s16vector? s16vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as s16vector-unfold!, but initializes the s16vector from right to left."))
((name . "reverse-s16vector->list")
 (signature
   case-lambda
   (((s16vector? s16vec)) list?)
   (((s16vector? s16vec) (integer? start)) list?)
   (((s16vector? s16vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as s16vec in reverse order."))
((name . "reverse-list->s16vector")
 (signature lambda ((list? proper-list)) s16vector?)
 (tags pure)
 (desc . "Returns s16vector with same elements as list in reverse order."))
((name . "s16vector->vector")
 (signature
   case-lambda
   (((s16vector? s16vec)) vector?)
   (((s16vector? s16vec) (integer? start)) vector?)
   (((s16vector? s16vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as s16vec."))
((name . "vector->s16vector")
 (signature
   case-lambda
   (((vector? vec)) s16vector?)
   (((vector? vec) (integer? start)) s16vector?)
   (((vector? vec) (integer? start) (integer? end)) s16vector?))
 (tags pure)
 (desc . "Returns s16vector with same elements as vec."))
((name . "make-s16vector-generator")
 (signature lambda ((s16vector? s16vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? s16?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of s16vector in order. Note that the generator is finite."))
((name . "s16vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of s16vector."))
((name . "write-s16vector")
 (signature
   case-lambda
   (((s16vector s16vec)) undefined)
   (((s16vector s16vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of s16vec in the lexical syntax explained below."))
)
