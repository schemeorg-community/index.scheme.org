(
((name . "make-s8vector")
 (signature
   case-lambda
   (((integer? size)) s8vector?)
   (((integer? size) (s8? fill)) s8vector?))
 (tags pure)
 (desc . "Returns a s8vector whose length is size. If fill is provided, all the elements of the s8vector are initialized to it."))
((name . "s8vector") 
 (signature lambda ((s8? value) ...) s8vector?)
 (tags pure)
 (desc . "Returns a s8vector initialized with values."))
((name . "s8vector?") 
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a s8vector, and #f otherwise."))
((name . "s8vector-length")
 (signature lambda ((s8vector? s8vec)) integer?)
 (tags pure)
 (desc . "Returns the length of s8vec"))
((name . "s8vector-ref")
 (signature lambda ((s8vector? s8vec) (integer? i)) s8?)
 (tags pure)
 (desc . "Returns the ith element of s8vec."))
((name . "s8vector-set!")
 (signature lambda ((s8vector? s8vec) (integer? i) (s8? value)) undefined)
 (desc . "Sets the ith element of s8vec to value."))
((name . "s8vector->list")
 (signature
   case-lambda
   (((s8vector? s8vec)) list?)
   (((s8vector? s8vec) (integer? start)) list?)
   (((s8vector? s8vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as s8vec"))
((name . "list->s8vector")
 (signature lambda ((list? proper-list)) s8vector?)
 (subsigs
   (proper-list (list s8?)))
 (tags pure)
 (desc . "Returns s8vector with same elements as list."))
((name . "s8?")
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a valid element of an s8vector (ie., signed exact integer in the range -(2^7) to (2^7)-1), and #f otherwise."))
((name . "s8vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) s8vector?)
   (((procedure? f) (integer? length) seed) s8vector?))
 (subsigs (f (lambda ((integer? index) state) (values s8? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "s8vector-copy")
 (signature
   case-lambda
   (((s8vector? s8vec)) s8vector?)
   (((s8vector? s8vec) (integer? start)) s8vector?)
   (((s8vector? s8vec) (integer? start) (integer? end)) s8vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of s8vec from start to end and returns it."))
((name . "s8vector-reverse-copy")
 (signature
   case-lambda
   (((s8vector? s8vec)) s8vector?)
   (((s8vector? s8vec) (integer? start)) s8vector?)
   (((s8vector? s8vec) (integer? start) (integer? end)) s8vector?))
 (tags pure)
 (desc . "The same as s8vector-copy, but in reverse order."))
((name . "s8vector-append")
 (signature lambda ((s8vector? s8vec) ...) s8vector?)
 (tags pure)
 (desc . "Returns a s8vector containing all the elements of the s8vecs in order."))
((name . "s8vector-concatenate")
 (signature lambda ((list? list-of-s8vectors)) s8vector?)
 (subsigs
   (list-of-s8vectors (list s8vector?)))
 (tags pure)
 (desc . "The same as s8vector-append, but takes a list of s8vectors rather than multiple arguments."))
((name . "s8vector-append-subvectors")
 (signature
   lambda
   ((s8vector? s8vec1) (integer? start1) (integer? end1) ...)
   s8vector?)
 (tags pure)
 (desc . "Concatenates the result of applying s8vector-copy to each triplet of s8vec, start, end arguments, but may be implemented more efficiently."))
((name . "s8vector-empty?")
 (signature lambda ((s8vector? s8vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if s8vec has a length of zero, and #f otherwise."))
((name . "s8vector=")
 (signature lambda ((s8vector? s8vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the s8vecs for elementwise equality, using = to do the comparisons. Returns #f unless all s8vectors are the same length."))
((group
   ((name . "s8vector-take")
    (signature lambda ((s8vector? s8vec) (integer? n)) s8vector?)
    (tags pure))
   ((name . "s8vector-take-right")
    (signature lambda ((s8vector? s8vec) (integer? n)) s8vector?)
    (tags pure)))
 (desc . "Returns a s8vector containing the first/last n elements of s8vec."))
((group
   ((name . "s8vector-drop")
    (signature lambda ((s8vector? s8vec) (integer? n)) s8vector?)
    (tags pure))
   ((name . "s8vector-drop-right")
    (signature lambda ((s8vector? s8vec) (integer? n)) s8vector?)
    (tags pure)))
 (desc . "Returns a s8vector containing all except the first/last n elements of s8vec."))
((name . "s8vector-segment")
 (signature lambda ((s8vector? s8vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of s8vectors, each of which contains n consecutive elements of s8vec. The last s8vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "s8vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (s8vector? s8vec1) (s8vector? s8vec2) ...)
      *)
    (subsigs (kons (lambda (state (s8? obj1) (s8? obj2) ...) *)))
    (tags pure))
   ((name . "s8vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (s8vector? s8vec1) (s8vector? s8vec2) ...)
      *)
    (subsigs (kons (lambda (state (s8? obj1) (s8? obj2) ...) *)))
    (tags pure)))
 (desc . "When one s8vector argument s8vec is given, folds kons over the elements of s8vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple s8vector arguments are given, kons is called with the current state value and each value from all the vectors; s8vector-fold scans elements from left to right, while s8vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "s8vector-map")
    (signature
      lambda
      ((procedure? proc) (s8vector? s8vec1) (s8vector? s8vec2) ...)
      vector?)
    (subsigs (proc (lambda ((s8? val1) ...) *)))
    (tags pure))
   ((name . "s8vector-map!")
    (signature
      lambda
      ((procedure? proc) (s8vector? s8vec1) (s8vector? s8vec2) ...)
      undefined)
    (subsigs (proc (lambda ((s8? val1) ...) *))))
   ((name . "s8vector-for-each")
    (signature
      lambda
      ((procedure? proc) (s8vector? s8vec1) (s8vector? s8vec2) ...)
      undefined)
    (subsigs (proc (lambda ((s8? val1) ...) undefined)))))
 (desc . "Iterate over the elements of s8vec and apply f to each, returning respectively a s8vector of the results, an undefined value with the results placed back in s8vec, and an undefined value with no change to s8vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For s8vector-map!, only s8vec is modified even when multiple vectors are passed.
If s8vector-map or s8vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "s8vector-count")
 (signature
   lambda
   ((procedure? pred?) (s8vector? s8vec1) (s8vector? s8vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((s8? val1) (s8? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of s8vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "s8vector-cumulate")
 (signature lambda ((procedure? f) knil (s8vector? s8vec)) s8vector?)
 (subsigs (f (lambda (state (s8? value)) *)))
 (tags pure)
 (desc . "Like s8vector-fold, but returns an s8vector of partial results rather than just the final result."))
((group
   ((name . "s8vector-take-while")
    (signature lambda ((procedure? pred?) (s8vector? s8vec)) s8vector?)
    (subsigs (pred? (lambda ((s8? value)) boolean?)))
    (tags pure))
   ((name . "s8vector-take-while-right")
    (signature lambda ((procedure? pred?) (s8vector? s8vec)) s8vector?)
    (subsigs (pred? (lambda ((s8? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of s8vec all of whose elements satisfy pred?."))
((group
   ((name . "s8vector-drop-while")
    (signature lambda ((procedure? pred?) (s8vector? s8vec)) s8vector?)
    (subsigs (pred? (lambda ((s8? value)) boolean?)))
    (tags pure))
   ((name . "s8vector-drop-while-right")
    (signature lambda ((procedure? pred?) (s8vector? s8vec)) s8vector?)
    (subsigs (pred? (lambda ((s8? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of s8vec such that all its elements satisfy pred."))
((group
   ((name . "s8vector-index")
    (signature
      lambda
      ((procedure? pred?) (s8vector? s8vec1) (s8vector? s8vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s8? value1) (s8? value2) ...) *)))
    (tags pure))
   ((name . "s8vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (s8vector? s8vec1) (s8vector? s8vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s8? value1) (s8? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of s8vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, s8vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for s8vector-index-right."))
((group
   ((name . "s8vector-skip")
    (signature
      lambda
      ((procedure? pred?) (s8vector? s8vec1) (s8vector? s8vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s8? value1) (s8? value2) ...) *)))
    (tags pure))
   ((name . "s8vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (s8vector? s8vec1) (s8vector? s8vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s8? value1) (s8? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of s8vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, s8vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for s8vector-skip-right."))
((name . "s8vector-any")
 (signature lambda ((procedure? pred?) (s8vector? s8vec1) (s8vector? s8vec2) ...) *)
 (subsigs (pred? (lambda ((s8? val1) (s8? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the s8vec, or #f if there is no such element. If s8vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "s8vector-every")
 (signature lambda ((procedure? pred?) (s8vector? s8vec1) (s8vector? s8vec2) ...) *)
 (subsigs (pred? (lambda ((s8? val1) (s8? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from s8vec satisfy pred?, return the last result of pred?. If not all do, return #f. If s8vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "s8vector-partition")
 (signature
   lambda
   ((procedure? pred?) (s8vector? s8vec))
   (values s8vector? integer?))
 (subsigs (pred? (lambda ((s8? value)) boolean?)))
 (tags pure)
 (desc . "Returns an s8vector of the same type as s8vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new s8vector and the number of elements satisfying pred?."))
((group
   ((name . "s8vector-filter")
    (signature lambda ((procedure? pred?) (s8vector? s8vec1)) s8vector?)
    (subsigs (pred? (lambda ((s8? val)) boolean?)))
    (tags pure))
   ((name . "s8vector-remove")
    (signature lambda ((procedure? pred?) (s8vector? s8vec1)) s8vector?)
    (subsigs (pred? (lambda ((s8? val)) boolean?)))
    (tags pure)))
 (desc . "Return an s8vector containing the elements of s8vec that satisfy / do not satisfy pred?."))
((name . "s8vector-swap!")
 (signature lambda ((s8vector? s8vector) (integer? i) (integer? j)) undefined))
((name . "s8vector-fill!")
 (signature
   case-lambda
   (((s8vector? s8vec) (s8? fill)) undefined)
   (((s8vector? s8vec) (s8? fill) (integer? start)) undefined)
   (((s8vector? s8vec) (s8? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of s8vec from start to end with the value fill."))
((name . "s8vector-reverse!")
 (signature
   case-lambda
   (((s8vector? s8vec)) undefined)
   (((s8vector? s8vec) (integer? start)) undefined)
   (((s8vector? s8vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of s8vec from start to end."))
((name . "s8vector-copy!")
 (signature
   case-lambda
   (((s8vector? s8to) (integer? at) (s8vector? s8from)) undefined)
   (((s8vector? s8to) (integer? at) (s8vector? s8from) (integer? start)) undefined)
   (((s8vector? s8to)
     (integer? at)
     (s8vector? s8from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of s8from from start to end onto s8to, starting at index at."))
((name . "s8vector-reverse-copy!")
 (signature
   case-lambda
   (((s8vector? s8to) (integer? at) (s8vector? s8from)) undefined)
   (((s8vector? s8to) (integer? at) (s8vector? s8from) (integer? start)) undefined)
   (((s8vector? s8to)
     (integer? at)
     (s8vector? s8from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as s8vector-copy!, but copies in reverse"))
((name . "s8vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (s8vector? s8vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like s8vector-unfold, but the elements are copied into the vector s8vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "s8vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (s8vector? s8vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as s8vector-unfold!, but initializes the s8vector from right to left."))
((name . "reverse-s8vector->list")
 (signature
   case-lambda
   (((s8vector? s8vec)) list?)
   (((s8vector? s8vec) (integer? start)) list?)
   (((s8vector? s8vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as s8vec in reverse order."))
((name . "reverse-list->s8vector")
 (signature lambda ((list? proper-list)) s8vector?)
 (tags pure)
 (desc . "Returns s8vector with same elements as list in reverse order."))
((name . "s8vector->vector")
 (signature
   case-lambda
   (((s8vector? s8vec)) vector?)
   (((s8vector? s8vec) (integer? start)) vector?)
   (((s8vector? s8vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as s8vec."))
((name . "vector->s8vector")
 (signature
   case-lambda
   (((vector? vec)) s8vector?)
   (((vector? vec) (integer? start)) s8vector?)
   (((vector? vec) (integer? start) (integer? end)) s8vector?))
 (tags pure)
 (desc . "Returns s8vector with same elements as vec."))
((name . "make-s8vector-generator")
 (signature lambda ((s8vector? s8vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? s8?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of s8vector in order. Note that the generator is finite."))
((name . "s8vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of s8vector."))
((name . "write-s8vector")
 (signature
   case-lambda
   (((s8vector s8vec)) undefined)
   (((s8vector s8vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of s8vec in the lexical syntax explained below."))
)
