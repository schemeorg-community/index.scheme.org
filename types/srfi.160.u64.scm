(
((name . "make-u64vector")
 (signature
   case-lambda
   (((integer? size)) u64vector?)
   (((integer? size) (u64? fill)) u64vector?))
 (tags pure)
 (desc . "Returns a u64vector whose length is size. If fill is provided, all the elements of the u64vector are initialized to it."))
((name . "u64vector") 
 (signature lambda ((u64? value) ...) u64vector?)
 (tags pure)
 (desc . "Returns a u64vector initialized with values."))
((name . "u64vector?") 
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a u64vector, and #f otherwise."))
((name . "u64vector-length")
 (signature lambda ((u64vector? u64vec)) integer?)
 (tags pure)
 (desc . "Returns the length of u64vec"))
((name . "u64vector-ref")
 (signature lambda ((u64vector? u64vec) (integer? i)) u64?)
 (tags pure)
 (desc . "Returns the ith element of u64vec."))
((name . "u64vector-set!")
 (signature lambda ((u64vector? u64vec) (integer? i) (u64? value)) undefined)
 (desc . "Sets the ith element of u64vec to value."))
((name . "u64vector->list")
 (signature
   case-lambda
   (((u64vector? u64vec)) list?)
   (((u64vector? u64vec) (integer? start)) list?)
   (((u64vector? u64vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as u64vec"))
((name . "list->u64vector")
 (signature lambda ((list? proper-list)) u64vector?)
 (subsigs
   (proper-list (list u64?)))
 (tags pure)
 (desc . "Returns u64vector with same elements as list."))
((name . "u64?")
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a valid element of an u64vector (ie., unsigned exact integer in the range 0 to (2^64)-1), and #f otherwise."))
((name . "u64vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) u64vector?)
   (((procedure? f) (integer? length) seed) u64vector?))
 (subsigs (f (lambda ((integer? index) state) (values u64? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "u64vector-copy")
 (signature
   case-lambda
   (((u64vector? u64vec)) u64vector?)
   (((u64vector? u64vec) (integer? start)) u64vector?)
   (((u64vector? u64vec) (integer? start) (integer? end)) u64vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of u64vec from start to end and returns it."))
((name . "u64vector-reverse-copy")
 (signature
   case-lambda
   (((u64vector? u64vec)) u64vector?)
   (((u64vector? u64vec) (integer? start)) u64vector?)
   (((u64vector? u64vec) (integer? start) (integer? end)) u64vector?))
 (tags pure)
 (desc . "The same as u64vector-copy, but in reverse order."))
((name . "u64vector-append")
 (signature lambda ((u64vector? u64vec) ...) u64vector?)
 (tags pure)
 (desc . "Returns a u64vector containing all the elements of the u64vecs in order."))
((name . "u64vector-concatenate")
 (signature lambda ((list? list-of-u64vectors)) u64vector?)
 (subsigs
   (list-of-u64vectors (list u64vector?)))
 (tags pure)
 (desc . "The same as u64vector-append, but takes a list of u64vectors rather than multiple arguments."))
((name . "u64vector-append-subvectors")
 (signature
   lambda
   ((u64vector? u64vec1) (integer? start1) (integer? end1) ...)
   u64vector?)
 (tags pure)
 (desc . "Concatenates the result of applying u64vector-copy to each triplet of u64vec, start, end arguments, but may be implemented more efficiently."))
((name . "u64vector-empty?")
 (signature lambda ((u64vector? u64vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if u64vec has a length of zero, and #f otherwise."))
((name . "u64vector=")
 (signature lambda ((u64vector? u64vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the u64vecs for elementwise equality, using = to do the comparisons. Returns #f unless all u64vectors are the same length."))
((group
   ((name . "u64vector-take")
    (signature lambda ((u64vector? u64vec) (integer? n)) u64vector?)
    (tags pure))
   ((name . "u64vector-take-right")
    (signature lambda ((u64vector? u64vec) (integer? n)) u64vector?)
    (tags pure)))
 (desc . "Returns a u64vector containing the first/last n elements of u64vec."))
((group
   ((name . "u64vector-drop")
    (signature lambda ((u64vector? u64vec) (integer? n)) u64vector?)
    (tags pure))
   ((name . "u64vector-drop-right")
    (signature lambda ((u64vector? u64vec) (integer? n)) u64vector?)
    (tags pure)))
 (desc . "Returns a u64vector containing all except the first/last n elements of u64vec."))
((name . "u64vector-segment")
 (signature lambda ((u64vector? u64vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of u64vectors, each of which contains n consecutive elements of u64vec. The last u64vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "u64vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (u64vector? u64vec1) (u64vector? u64vec2) ...)
      *)
    (subsigs (kons (lambda (state (u64? obj1) (u64? obj2) ...) *)))
    (tags pure))
   ((name . "u64vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (u64vector? u64vec1) (u64vector? u64vec2) ...)
      *)
    (subsigs (kons (lambda (state (u64? obj1) (u64? obj2) ...) *)))
    (tags pure)))
 (desc . "When one u64vector argument u64vec is given, folds kons over the elements of u64vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple u64vector arguments are given, kons is called with the current state value and each value from all the vectors; u64vector-fold scans elements from left to right, while u64vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "u64vector-map")
    (signature
      lambda
      ((procedure? proc) (u64vector? u64vec1) (u64vector? u64vec2) ...)
      vector?)
    (subsigs (proc (lambda ((u64? val1) ...) *)))
    (tags pure))
   ((name . "u64vector-map!")
    (signature
      lambda
      ((procedure? proc) (u64vector? u64vec1) (u64vector? u64vec2) ...)
      undefined)
    (subsigs (proc (lambda ((u64? val1) ...) *))))
   ((name . "u64vector-for-each")
    (signature
      lambda
      ((procedure? proc) (u64vector? u64vec1) (u64vector? u64vec2) ...)
      undefined)
    (subsigs (proc (lambda ((u64? val1) ...) undefined)))))
 (desc . "Iterate over the elements of u64vec and apply f to each, returning respectively a u64vector of the results, an undefined value with the results placed back in u64vec, and an undefined value with no change to u64vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For u64vector-map!, only u64vec is modified even when multiple vectors are passed.
If u64vector-map or u64vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "u64vector-count")
 (signature
   lambda
   ((procedure? pred?) (u64vector? u64vec1) (u64vector? u64vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((u64? val1) (u64? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of u64vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "u64vector-cumulate")
 (signature lambda ((procedure? f) knil (u64vector? u64vec)) u64vector?)
 (subsigs (f (lambda (state (u64? value)) *)))
 (tags pure)
 (desc . "Like u64vector-fold, but returns an u64vector of partial results rather than just the final result."))
((group
   ((name . "u64vector-take-while")
    (signature lambda ((procedure? pred?) (u64vector? u64vec)) u64vector?)
    (subsigs (pred? (lambda ((u64? value)) boolean?)))
    (tags pure))
   ((name . "u64vector-take-while-right")
    (signature lambda ((procedure? pred?) (u64vector? u64vec)) u64vector?)
    (subsigs (pred? (lambda ((u64? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of u64vec all of whose elements satisfy pred?."))
((group
   ((name . "u64vector-drop-while")
    (signature lambda ((procedure? pred?) (u64vector? u64vec)) u64vector?)
    (subsigs (pred? (lambda ((u64? value)) boolean?)))
    (tags pure))
   ((name . "u64vector-drop-while-right")
    (signature lambda ((procedure? pred?) (u64vector? u64vec)) u64vector?)
    (subsigs (pred? (lambda ((u64? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of u64vec such that all its elements satisfy pred."))
((group
   ((name . "u64vector-index")
    (signature
      lambda
      ((procedure? pred?) (u64vector? u64vec1) (u64vector? u64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u64? value1) (u64? value2) ...) *)))
    (tags pure))
   ((name . "u64vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (u64vector? u64vec1) (u64vector? u64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u64? value1) (u64? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of u64vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, u64vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for u64vector-index-right."))
((group
   ((name . "u64vector-skip")
    (signature
      lambda
      ((procedure? pred?) (u64vector? u64vec1) (u64vector? u64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u64? value1) (u64? value2) ...) *)))
    (tags pure))
   ((name . "u64vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (u64vector? u64vec1) (u64vector? u64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u64? value1) (u64? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of u64vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, u64vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for u64vector-skip-right."))
((name . "u64vector-any")
 (signature lambda ((procedure? pred?) (u64vector? u64vec1) (u64vector? u64vec2) ...) *)
 (subsigs (pred? (lambda ((u64? val1) (u64? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the u64vec, or #f if there is no such element. If u64vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "u64vector-every")
 (signature lambda ((procedure? pred?) (u64vector? u64vec1) (u64vector? u64vec2) ...) *)
 (subsigs (pred? (lambda ((u64? val1) (u64? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from u64vec satisfy pred?, return the last result of pred?. If not all do, return #f. If u64vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "u64vector-partition")
 (signature
   lambda
   ((procedure? pred?) (u64vector? u64vec))
   (values u64vector? integer?))
 (subsigs (pred? (lambda ((u64? value)) boolean?)))
 (tags pure)
 (desc . "Returns an u64vector of the same type as u64vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new u64vector and the number of elements satisfying pred?."))
((group
   ((name . "u64vector-filter")
    (signature lambda ((procedure? pred?) (u64vector? u64vec1)) u64vector?)
    (subsigs (pred? (lambda ((u64? val)) boolean?)))
    (tags pure))
   ((name . "u64vector-remove")
    (signature lambda ((procedure? pred?) (u64vector? u64vec1)) u64vector?)
    (subsigs (pred? (lambda ((u64? val)) boolean?)))
    (tags pure)))
 (desc . "Return an u64vector containing the elements of u64vec that satisfy / do not satisfy pred?."))
((name . "u64vector-swap!")
 (signature lambda ((u64vector? u64vec) (integer? i) (integer? j)) undefined)
 (desc . "Interchanges the ith and jth elements of u64vec."))
((name . "u64vector-fill!")
 (signature
   case-lambda
   (((u64vector? u64vec) (u64? fill)) undefined)
   (((u64vector? u64vec) (u64? fill) (integer? start)) undefined)
   (((u64vector? u64vec) (u64? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of u64vec from start to end with the value fill."))
((name . "u64vector-reverse!")
 (signature
   case-lambda
   (((u64vector? u64vec)) undefined)
   (((u64vector? u64vec) (integer? start)) undefined)
   (((u64vector? u64vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of u64vec from start to end."))
((name . "u64vector-copy!")
 (signature
   case-lambda
   (((u64vector? u64to) (integer? at) (u64vector? u64from)) undefined)
   (((u64vector? u64to) (integer? at) (u64vector? u64from) (integer? start)) undefined)
   (((u64vector? u64to)
     (integer? at)
     (u64vector? u64from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of u64from from start to end onto u64to, starting at index at."))
((name . "u64vector-reverse-copy!")
 (signature
   case-lambda
   (((u64vector? u64to) (integer? at) (u64vector? u64from)) undefined)
   (((u64vector? u64to) (integer? at) (u64vector? u64from) (integer? start)) undefined)
   (((u64vector? u64to)
     (integer? at)
     (u64vector? u64from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as u64vector-copy!, but copies in reverse"))
((name . "u64vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (u64vector? u64vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like u64vector-unfold, but the elements are copied into the vector u64vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "u64vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (u64vector? u64vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as u64vector-unfold!, but initializes the u64vector from right to left."))
((name . "reverse-u64vector->list")
 (signature
   case-lambda
   (((u64vector? u64vec)) list?)
   (((u64vector? u64vec) (integer? start)) list?)
   (((u64vector? u64vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as u64vec in reverse order."))
((name . "reverse-list->u64vector")
 (signature lambda ((list? proper-list)) u64vector?)
 (tags pure)
 (desc . "Returns u64vector with same elements as list in reverse order."))
((name . "u64vector->vector")
 (signature
   case-lambda
   (((u64vector? u64vec)) vector?)
   (((u64vector? u64vec) (integer? start)) vector?)
   (((u64vector? u64vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as u64vec."))
((name . "vector->u64vector")
 (signature
   case-lambda
   (((vector? vec)) u64vector?)
   (((vector? vec) (integer? start)) u64vector?)
   (((vector? vec) (integer? start) (integer? end)) u64vector?))
 (tags pure)
 (desc . "Returns u64vector with same elements as vec."))
((name . "make-u64vector-generator")
 (signature lambda ((u64vector? u64vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? u64?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of u64vector in order. Note that the generator is finite."))
((name . "u64vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of u64vector."))
((name . "write-u64vector")
 (signature
   case-lambda
   (((u64vector u64vec)) undefined)
   (((u64vector u64vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of u64vec in the lexical syntax explained below."))
)
