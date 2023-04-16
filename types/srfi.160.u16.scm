(
((name . "make-u16vector")
 (signature
   case-lambda
   (((integer? size)) u16vector?)
   (((integer? size) (u16? fill)) u16vector?))
 (tags pure)
 (desc . "Returns a u16vector whose length is size. If fill is provided, all the elements of the u16vector are initialized to it."))
((name . "u16vector") 
 (signature lambda ((u16? value) ...) u16vector?)
 (tags pure)
 (desc . "Returns a u16vector initialized with values."))
((name . "u16vector?") 
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a u16vector, and #f otherwise."))
((name . "u16vector-length")
 (signature lambda ((u16vector? u16vec)) integer?)
 (tags pure)
 (desc . "Returns the length of u16vec"))
((name . "u16vector-ref")
 (signature lambda ((u16vector? u16vec) (integer? i)) u16?)
 (tags pure)
 (desc . "Returns the ith element of u16vec."))
((name . "u16vector-set!")
 (signature lambda ((u16vector? u16vec) (integer? i) (u16? value)) undefined)
 (desc . "Sets the ith element of u16vec to value."))
((name . "u16vector->list")
 (signature
   case-lambda
   (((u16vector? u16vec)) list?)
   (((u16vector? u16vec) (integer? start)) list?)
   (((u16vector? u16vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as u16vec"))
((name . "list->u16vector")
 (signature lambda ((list? proper-list)) u16vector?)
 (subsigs
   (proper-list (list u16?)))
 (tags pure)
 (desc . "Returns u16vector with same elements as list."))
((name . "u16?")
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a valid element of an u16vector (ie., unsigned exact integer in the range 0 to (2^16)-1), and #f otherwise."))
((name . "u16vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) u16vector?)
   (((procedure? f) (integer? length) seed) u16vector?))
 (subsigs (f (lambda ((integer? index) state) (values u16? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "u16vector-copy")
 (signature
   case-lambda
   (((u16vector? u16vec)) u16vector?)
   (((u16vector? u16vec) (integer? start)) u16vector?)
   (((u16vector? u16vec) (integer? start) (integer? end)) u16vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of u16vec from start to end and returns it."))
((name . "u16vector-reverse-copy")
 (signature
   case-lambda
   (((u16vector? u16vec)) u16vector?)
   (((u16vector? u16vec) (integer? start)) u16vector?)
   (((u16vector? u16vec) (integer? start) (integer? end)) u16vector?))
 (tags pure)
 (desc . "The same as u16vector-copy, but in reverse order."))
((name . "u16vector-append")
 (signature lambda ((u16vector? u16vec) ...) u16vector?)
 (tags pure)
 (desc . "Returns a u16vector containing all the elements of the u16vecs in order."))
((name . "u16vector-concatenate")
 (signature lambda ((list? list-of-u16vectors)) u16vector?)
 (subsigs
   (list-of-u16vectors (list u16vector?)))
 (tags pure)
 (desc . "The same as u16vector-append, but takes a list of u16vectors rather than multiple arguments."))
((name . "u16vector-append-subvectors")
 (signature
   lambda
   ((u16vector? u16vec1) (integer? start1) (integer? end1) ...)
   u16vector?)
 (tags pure)
 (desc . "Concatenates the result of applying u16vector-copy to each triplet of u16vec, start, end arguments, but may be implemented more efficiently."))
((name . "u16vector-empty?")
 (signature lambda ((u16vector? u16vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if u16vec has a length of zero, and #f otherwise."))
((name . "u16vector=")
 (signature lambda ((u16vector? u16vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the u16vecs for elementwise equality, using = to do the comparisons. Returns #f unless all u16vectors are the same length."))
((group
   ((name . "u16vector-take")
    (signature lambda ((u16vector? u16vec) (integer? n)) u16vector?)
    (tags pure))
   ((name . "u16vector-take-right")
    (signature lambda ((u16vector? u16vec) (integer? n)) u16vector?)
    (tags pure)))
 (desc . "Returns a u16vector containing the first/last n elements of u16vec."))
((group
   ((name . "u16vector-drop")
    (signature lambda ((u16vector? u16vec) (integer? n)) u16vector?)
    (tags pure))
   ((name . "u16vector-drop-right")
    (signature lambda ((u16vector? u16vec) (integer? n)) u16vector?)
    (tags pure)))
 (desc . "Returns a u16vector containing all except the first/last n elements of u16vec."))
((name . "u16vector-segment")
 (signature lambda ((u16vector? u16vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of u16vectors, each of which contains n consecutive elements of u16vec. The last u16vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "u16vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (u16vector? u16vec1) (u16vector? u16vec2) ...)
      *)
    (subsigs (kons (lambda (state (u16? obj1) (u16? obj2) ...) *)))
    (tags pure))
   ((name . "u16vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (u16vector? u16vec1) (u16vector? u16vec2) ...)
      *)
    (subsigs (kons (lambda (state (u16? obj1) (u16? obj2) ...) *)))
    (tags pure)))
 (desc . "When one u16vector argument u16vec is given, folds kons over the elements of u16vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple u16vector arguments are given, kons is called with the current state value and each value from all the vectors; u16vector-fold scans elements from left to right, while u16vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "u16vector-map")
    (signature
      lambda
      ((procedure? proc) (u16vector? u16vec1) (u16vector? u16vec2) ...)
      vector?)
    (subsigs (proc (lambda ((u16? val1) ...) *)))
    (tags pure))
   ((name . "u16vector-map!")
    (signature
      lambda
      ((procedure? proc) (u16vector? u16vec1) (u16vector? u16vec2) ...)
      undefined)
    (subsigs (proc (lambda ((u16? val1) ...) *))))
   ((name . "u16vector-for-each")
    (signature
      lambda
      ((procedure? proc) (u16vector? u16vec1) (u16vector? u16vec2) ...)
      undefined)
    (subsigs (proc (lambda ((u16? val1) ...) undefined)))))
 (desc . "Iterate over the elements of u16vec and apply f to each, returning respectively a u16vector of the results, an undefined value with the results placed back in u16vec, and an undefined value with no change to u16vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For u16vector-map!, only u16vec is modified even when multiple vectors are passed.
If u16vector-map or u16vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "u16vector-count")
 (signature
   lambda
   ((procedure? pred?) (u16vector? u16vec1) (u16vector? u16vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((u16? val1) (u16? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of u16vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "u16vector-cumulate")
 (signature lambda ((procedure? f) knil (u16vector? u16vec)) u16vector?)
 (subsigs (f (lambda (state (u16? value)) *)))
 (tags pure)
 (desc . "Like u16vector-fold, but returns an u16vector of partial results rather than just the final result."))
((group
   ((name . "u16vector-take-while")
    (signature lambda ((procedure? pred?) (u16vector? u16vec)) u16vector?)
    (subsigs (pred? (lambda ((u16? value)) boolean?)))
    (tags pure))
   ((name . "u16vector-take-while-right")
    (signature lambda ((procedure? pred?) (u16vector? u16vec)) u16vector?)
    (subsigs (pred? (lambda ((u16? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of u16vec all of whose elements satisfy pred?."))
((group
   ((name . "u16vector-drop-while")
    (signature lambda ((procedure? pred?) (u16vector? u16vec)) u16vector?)
    (subsigs (pred? (lambda ((u16? value)) boolean?)))
    (tags pure))
   ((name . "u16vector-drop-while-right")
    (signature lambda ((procedure? pred?) (u16vector? u16vec)) u16vector?)
    (subsigs (pred? (lambda ((u16? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of u16vec such that all its elements satisfy pred."))
((group
   ((name . "u16vector-index")
    (signature
      lambda
      ((procedure? pred?) (u16vector? u16vec1) (u16vector? u16vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u16? value1) (u16? value2) ...) *)))
    (tags pure))
   ((name . "u16vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (u16vector? u16vec1) (u16vector? u16vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u16? value1) (u16? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of u16vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, u16vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for u16vector-index-right."))
((group
   ((name . "u16vector-skip")
    (signature
      lambda
      ((procedure? pred?) (u16vector? u16vec1) (u16vector? u16vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u16? value1) (u16? value2) ...) *)))
    (tags pure))
   ((name . "u16vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (u16vector? u16vec1) (u16vector? u16vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u16? value1) (u16? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of u16vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, u16vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for u16vector-skip-right."))
((name . "u16vector-any")
 (signature lambda ((procedure? pred?) (u16vector? u16vec1) (u16vector? u16vec2) ...) *)
 (subsigs (pred? (lambda ((u16? val1) (u16? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the u16vec, or #f if there is no such element. If u16vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "u16vector-every")
 (signature lambda ((procedure? pred?) (u16vector? u16vec1) (u16vector? u16vec2) ...) *)
 (subsigs (pred? (lambda ((u16? val1) (u16? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from u16vec satisfy pred?, return the last result of pred?. If not all do, return #f. If u16vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "u16vector-partition")
 (signature
   lambda
   ((procedure? pred?) (u16vector? u16vec))
   (values u16vector? integer?))
 (subsigs (pred? (lambda ((u16? value)) boolean?)))
 (tags pure)
 (desc . "Returns an u16vector of the same type as u16vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new u16vector and the number of elements satisfying pred?."))
((group
   ((name . "u16vector-filter")
    (signature lambda ((procedure? pred?) (u16vector? u16vec1)) u16vector?)
    (subsigs (pred? (lambda ((u16? val)) boolean?)))
    (tags pure))
   ((name . "u16vector-remove")
    (signature lambda ((procedure? pred?) (u16vector? u16vec1)) u16vector?)
    (subsigs (pred? (lambda ((u16? val)) boolean?)))
    (tags pure)))
 (desc . "Return an u16vector containing the elements of u16vec that satisfy / do not satisfy pred?."))
((name . "u16vector-swap!")
 (signature lambda ((u16vector? u16vector) (integer? i) (integer? j)) undefined))
((name . "u16vector-fill!")
 (signature
   case-lambda
   (((u16vector? u16vec) (u16? fill)) undefined)
   (((u16vector? u16vec) (u16? fill) (integer? start)) undefined)
   (((u16vector? u16vec) (u16? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of u16vec from start to end with the value fill."))
((name . "u16vector-reverse!")
 (signature
   case-lambda
   (((u16vector? u16vec)) undefined)
   (((u16vector? u16vec) (integer? start)) undefined)
   (((u16vector? u16vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of u16vec from start to end."))
((name . "u16vector-copy!")
 (signature
   case-lambda
   (((u16vector? u16to) (integer? at) (u16vector? u16from)) undefined)
   (((u16vector? u16to) (integer? at) (u16vector? u16from) (integer? start)) undefined)
   (((u16vector? u16to)
     (integer? at)
     (u16vector? u16from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of u16from from start to end onto u16to, starting at index at."))
((name . "u16vector-reverse-copy!")
 (signature
   case-lambda
   (((u16vector? u16to) (integer? at) (u16vector? u16from)) undefined)
   (((u16vector? u16to) (integer? at) (u16vector? u16from) (integer? start)) undefined)
   (((u16vector? u16to)
     (integer? at)
     (u16vector? u16from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as u16vector-copy!, but copies in reverse"))
((name . "u16vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (u16vector? u16vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like u16vector-unfold, but the elements are copied into the vector u16vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "u16vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (u16vector? u16vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as u16vector-unfold!, but initializes the u16vector from right to left."))
((name . "reverse-u16vector->list")
 (signature
   case-lambda
   (((u16vector? u16vec)) list?)
   (((u16vector? u16vec) (integer? start)) list?)
   (((u16vector? u16vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as u16vec in reverse order."))
((name . "reverse-list->u16vector")
 (signature lambda ((list? proper-list)) u16vector?)
 (tags pure)
 (desc . "Returns u16vector with same elements as list in reverse order."))
((name . "u16vector->vector")
 (signature
   case-lambda
   (((u16vector? u16vec)) vector?)
   (((u16vector? u16vec) (integer? start)) vector?)
   (((u16vector? u16vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as u16vec."))
((name . "vector->u16vector")
 (signature
   case-lambda
   (((vector? vec)) u16vector?)
   (((vector? vec) (integer? start)) u16vector?)
   (((vector? vec) (integer? start) (integer? end)) u16vector?))
 (tags pure)
 (desc . "Returns u16vector with same elements as vec."))
((name . "make-u16vector-generator")
 (signature lambda ((u16vector? u16vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? u16?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of u16vector in order. Note that the generator is finite."))
((name . "u16vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of u16vector."))
((name . "write-u16vector")
 (signature
   case-lambda
   (((u16vector u16vec)) undefined)
   (((u16vector u16vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of u16vec in the lexical syntax explained below."))
)
