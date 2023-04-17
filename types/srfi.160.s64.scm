(
((name . "make-s64vector")
 (signature
   case-lambda
   (((integer? size)) s64vector?)
   (((integer? size) (s64? fill)) s64vector?))
 (tags pure)
 (desc . "Returns a s64vector whose length is size. If fill is provided, all the elements of the s64vector are initialized to it."))
((name . "s64vector") 
 (signature lambda ((s64? value) ...) s64vector?)
 (tags pure)
 (desc . "Returns a s64vector initialized with values."))
((name . "s64vector?") 
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a s64vector, and #f otherwise."))
((name . "s64vector-length")
 (signature lambda ((s64vector? s64vec)) integer?)
 (tags pure)
 (desc . "Returns the length of s64vec"))
((name . "s64vector-ref")
 (signature lambda ((s64vector? s64vec) (integer? i)) s64?)
 (tags pure)
 (desc . "Returns the ith element of s64vec."))
((name . "s64vector-set!")
 (signature lambda ((s64vector? s64vec) (integer? i) (s64? value)) undefined)
 (desc . "Sets the ith element of s64vec to value."))
((name . "s64vector->list")
 (signature
   case-lambda
   (((s64vector? s64vec)) list?)
   (((s64vector? s64vec) (integer? start)) list?)
   (((s64vector? s64vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as s64vec"))
((name . "list->s64vector")
 (signature lambda ((list? proper-list)) s64vector?)
 (subsigs
   (proper-list (list s64?)))
 (tags pure)
 (desc . "Returns s64vector with same elements as list."))
((name . "s64?")
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a valid element of an s64vector (ie., signed exact integer in the range -(2^63) to (2^63)-1), and #f otherwise."))
((name . "s64vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) s64vector?)
   (((procedure? f) (integer? length) seed) s64vector?))
 (subsigs (f (lambda ((integer? index) state) (values s64? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "s64vector-copy")
 (signature
   case-lambda
   (((s64vector? s64vec)) s64vector?)
   (((s64vector? s64vec) (integer? start)) s64vector?)
   (((s64vector? s64vec) (integer? start) (integer? end)) s64vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of s64vec from start to end and returns it."))
((name . "s64vector-reverse-copy")
 (signature
   case-lambda
   (((s64vector? s64vec)) s64vector?)
   (((s64vector? s64vec) (integer? start)) s64vector?)
   (((s64vector? s64vec) (integer? start) (integer? end)) s64vector?))
 (tags pure)
 (desc . "The same as s64vector-copy, but in reverse order."))
((name . "s64vector-append")
 (signature lambda ((s64vector? s64vec) ...) s64vector?)
 (tags pure)
 (desc . "Returns a s64vector containing all the elements of the s64vecs in order."))
((name . "s64vector-concatenate")
 (signature lambda ((list? list-of-s64vectors)) s64vector?)
 (subsigs
   (list-of-s64vectors (list s64vector?)))
 (tags pure)
 (desc . "The same as s64vector-append, but takes a list of s64vectors rather than multiple arguments."))
((name . "s64vector-append-subvectors")
 (signature
   lambda
   ((s64vector? s64vec1) (integer? start1) (integer? end1) ...)
   s64vector?)
 (tags pure)
 (desc . "Concatenates the result of applying s64vector-copy to each triplet of s64vec, start, end arguments, but may be implemented more efficiently."))
((name . "s64vector-empty?")
 (signature lambda ((s64vector? s64vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if s64vec has a length of zero, and #f otherwise."))
((name . "s64vector=")
 (signature lambda ((s64vector? s64vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the s64vecs for elementwise equality, using = to do the comparisons. Returns #f unless all s64vectors are the same length."))
((group
   ((name . "s64vector-take")
    (signature lambda ((s64vector? s64vec) (integer? n)) s64vector?)
    (tags pure))
   ((name . "s64vector-take-right")
    (signature lambda ((s64vector? s64vec) (integer? n)) s64vector?)
    (tags pure)))
 (desc . "Returns a s64vector containing the first/last n elements of s64vec."))
((group
   ((name . "s64vector-drop")
    (signature lambda ((s64vector? s64vec) (integer? n)) s64vector?)
    (tags pure))
   ((name . "s64vector-drop-right")
    (signature lambda ((s64vector? s64vec) (integer? n)) s64vector?)
    (tags pure)))
 (desc . "Returns a s64vector containing all except the first/last n elements of s64vec."))
((name . "s64vector-segment")
 (signature lambda ((s64vector? s64vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of s64vectors, each of which contains n consecutive elements of s64vec. The last s64vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "s64vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (s64vector? s64vec1) (s64vector? s64vec2) ...)
      *)
    (subsigs (kons (lambda (state (s64? obj1) (s64? obj2) ...) *)))
    (tags pure))
   ((name . "s64vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (s64vector? s64vec1) (s64vector? s64vec2) ...)
      *)
    (subsigs (kons (lambda (state (s64? obj1) (s64? obj2) ...) *)))
    (tags pure)))
 (desc . "When one s64vector argument s64vec is given, folds kons over the elements of s64vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple s64vector arguments are given, kons is called with the current state value and each value from all the vectors; s64vector-fold scans elements from left to right, while s64vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "s64vector-map")
    (signature
      lambda
      ((procedure? proc) (s64vector? s64vec1) (s64vector? s64vec2) ...)
      vector?)
    (subsigs (proc (lambda ((s64? val1) ...) *)))
    (tags pure))
   ((name . "s64vector-map!")
    (signature
      lambda
      ((procedure? proc) (s64vector? s64vec1) (s64vector? s64vec2) ...)
      undefined)
    (subsigs (proc (lambda ((s64? val1) ...) *))))
   ((name . "s64vector-for-each")
    (signature
      lambda
      ((procedure? proc) (s64vector? s64vec1) (s64vector? s64vec2) ...)
      undefined)
    (subsigs (proc (lambda ((s64? val1) ...) undefined)))))
 (desc . "Iterate over the elements of s64vec and apply f to each, returning respectively a s64vector of the results, an undefined value with the results placed back in s64vec, and an undefined value with no change to s64vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For s64vector-map!, only s64vec is modified even when multiple vectors are passed.
If s64vector-map or s64vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "s64vector-count")
 (signature
   lambda
   ((procedure? pred?) (s64vector? s64vec1) (s64vector? s64vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((s64? val1) (s64? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of s64vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "s64vector-cumulate")
 (signature lambda ((procedure? f) knil (s64vector? s64vec)) s64vector?)
 (subsigs (f (lambda (state (s64? value)) *)))
 (tags pure)
 (desc . "Like s64vector-fold, but returns an s64vector of partial results rather than just the final result."))
((group
   ((name . "s64vector-take-while")
    (signature lambda ((procedure? pred?) (s64vector? s64vec)) s64vector?)
    (subsigs (pred? (lambda ((s64? value)) boolean?)))
    (tags pure))
   ((name . "s64vector-take-while-right")
    (signature lambda ((procedure? pred?) (s64vector? s64vec)) s64vector?)
    (subsigs (pred? (lambda ((s64? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of s64vec all of whose elements satisfy pred?."))
((group
   ((name . "s64vector-drop-while")
    (signature lambda ((procedure? pred?) (s64vector? s64vec)) s64vector?)
    (subsigs (pred? (lambda ((s64? value)) boolean?)))
    (tags pure))
   ((name . "s64vector-drop-while-right")
    (signature lambda ((procedure? pred?) (s64vector? s64vec)) s64vector?)
    (subsigs (pred? (lambda ((s64? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of s64vec such that all its elements satisfy pred."))
((group
   ((name . "s64vector-index")
    (signature
      lambda
      ((procedure? pred?) (s64vector? s64vec1) (s64vector? s64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s64? value1) (s64? value2) ...) *)))
    (tags pure))
   ((name . "s64vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (s64vector? s64vec1) (s64vector? s64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s64? value1) (s64? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of s64vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, s64vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for s64vector-index-right."))
((group
   ((name . "s64vector-skip")
    (signature
      lambda
      ((procedure? pred?) (s64vector? s64vec1) (s64vector? s64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s64? value1) (s64? value2) ...) *)))
    (tags pure))
   ((name . "s64vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (s64vector? s64vec1) (s64vector? s64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((s64? value1) (s64? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of s64vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, s64vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for s64vector-skip-right."))
((name . "s64vector-any")
 (signature lambda ((procedure? pred?) (s64vector? s64vec1) (s64vector? s64vec2) ...) *)
 (subsigs (pred? (lambda ((s64? val1) (s64? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the s64vec, or #f if there is no such element. If s64vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "s64vector-every")
 (signature lambda ((procedure? pred?) (s64vector? s64vec1) (s64vector? s64vec2) ...) *)
 (subsigs (pred? (lambda ((s64? val1) (s64? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from s64vec satisfy pred?, return the last result of pred?. If not all do, return #f. If s64vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "s64vector-partition")
 (signature
   lambda
   ((procedure? pred?) (s64vector? s64vec))
   (values s64vector? integer?))
 (subsigs (pred? (lambda ((s64? value)) boolean?)))
 (tags pure)
 (desc . "Returns an s64vector of the same type as s64vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new s64vector and the number of elements satisfying pred?."))
((group
   ((name . "s64vector-filter")
    (signature lambda ((procedure? pred?) (s64vector? s64vec1)) s64vector?)
    (subsigs (pred? (lambda ((s64? val)) boolean?)))
    (tags pure))
   ((name . "s64vector-remove")
    (signature lambda ((procedure? pred?) (s64vector? s64vec1)) s64vector?)
    (subsigs (pred? (lambda ((s64? val)) boolean?)))
    (tags pure)))
 (desc . "Return an s64vector containing the elements of s64vec that satisfy / do not satisfy pred?."))
((name . "s64vector-swap!")
 (signature lambda ((s64vector? s64vec) (integer? i) (integer? j)) undefined)
 (desc . "Interchanges the ith and jth elements of s64vec."))
((name . "s64vector-fill!")
 (signature
   case-lambda
   (((s64vector? s64vec) (s64? fill)) undefined)
   (((s64vector? s64vec) (s64? fill) (integer? start)) undefined)
   (((s64vector? s64vec) (s64? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of s64vec from start to end with the value fill."))
((name . "s64vector-reverse!")
 (signature
   case-lambda
   (((s64vector? s64vec)) undefined)
   (((s64vector? s64vec) (integer? start)) undefined)
   (((s64vector? s64vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of s64vec from start to end."))
((name . "s64vector-copy!")
 (signature
   case-lambda
   (((s64vector? s64to) (integer? at) (s64vector? s64from)) undefined)
   (((s64vector? s64to) (integer? at) (s64vector? s64from) (integer? start)) undefined)
   (((s64vector? s64to)
     (integer? at)
     (s64vector? s64from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of s64from from start to end onto s64to, starting at index at."))
((name . "s64vector-reverse-copy!")
 (signature
   case-lambda
   (((s64vector? s64to) (integer? at) (s64vector? s64from)) undefined)
   (((s64vector? s64to) (integer? at) (s64vector? s64from) (integer? start)) undefined)
   (((s64vector? s64to)
     (integer? at)
     (s64vector? s64from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as s64vector-copy!, but copies in reverse"))
((name . "s64vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (s64vector? s64vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like s64vector-unfold, but the elements are copied into the vector s64vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "s64vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (s64vector? s64vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as s64vector-unfold!, but initializes the s64vector from right to left."))
((name . "reverse-s64vector->list")
 (signature
   case-lambda
   (((s64vector? s64vec)) list?)
   (((s64vector? s64vec) (integer? start)) list?)
   (((s64vector? s64vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as s64vec in reverse order."))
((name . "reverse-list->s64vector")
 (signature lambda ((list? proper-list)) s64vector?)
 (tags pure)
 (desc . "Returns s64vector with same elements as list in reverse order."))
((name . "s64vector->vector")
 (signature
   case-lambda
   (((s64vector? s64vec)) vector?)
   (((s64vector? s64vec) (integer? start)) vector?)
   (((s64vector? s64vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as s64vec."))
((name . "vector->s64vector")
 (signature
   case-lambda
   (((vector? vec)) s64vector?)
   (((vector? vec) (integer? start)) s64vector?)
   (((vector? vec) (integer? start) (integer? end)) s64vector?))
 (tags pure)
 (desc . "Returns s64vector with same elements as vec."))
((name . "make-s64vector-generator")
 (signature lambda ((s64vector? s64vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? s64?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of s64vector in order. Note that the generator is finite."))
((name . "s64vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of s64vector."))
((name . "write-s64vector")
 (signature
   case-lambda
   (((s64vector s64vec)) undefined)
   (((s64vector s64vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of s64vec in the lexical syntax explained below."))
)
