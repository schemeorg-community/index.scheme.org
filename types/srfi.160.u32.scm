(
((name . "make-u32vector")
 (signature
   case-lambda
   (((integer? size)) u32vector?)
   (((integer? size) (u32? fill)) u32vector?))
 (tags pure)
 (desc . "Returns a u32vector whose length is size. If fill is provided, all the elements of the u32vector are initialized to it."))
((name . "u32vector") 
 (signature lambda ((u32? value) ...) u32vector?)
 (tags pure)
 (desc . "Returns a u32vector initialized with values."))
((name . "u32vector?") 
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a u32vector, and #f otherwise."))
((name . "u32vector-length")
 (signature lambda ((u32vector? u32vec)) integer?)
 (tags pure)
 (desc . "Returns the length of u32vec"))
((name . "u32vector-ref")
 (signature lambda ((u32vector? u32vec) (integer? i)) u32?)
 (tags pure)
 (desc . "Returns the ith element of u32vec."))
((name . "u32vector-set!")
 (signature lambda ((u32vector? u32vec) (integer? i) (u32? value)) undefined)
 (desc . "Sets the ith element of u32vec to value."))
((name . "u32vector->list")
 (signature
   case-lambda
   (((u32vector? u32vec)) list?)
   (((u32vector? u32vec) (integer? start)) list?)
   (((u32vector? u32vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as u32vec"))
((name . "list->u32vector")
 (signature lambda ((list? proper-list)) u32vector?)
 (subsigs
   (proper-list (list u32?)))
 (tags pure)
 (desc . "Returns u32vector with same elements as list."))
((name . "u32?")
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a valid element of an u32vector (ie., unsigned exact integer in the range 0 to (2^32)-1), and #f otherwise."))
((name . "u32vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) u32vector?)
   (((procedure? f) (integer? length) seed) u32vector?))
 (subsigs (f (lambda ((integer? index) state) (values u32? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "u32vector-copy")
 (signature
   case-lambda
   (((u32vector? u32vec)) u32vector?)
   (((u32vector? u32vec) (integer? start)) u32vector?)
   (((u32vector? u32vec) (integer? start) (integer? end)) u32vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of u32vec from start to end and returns it."))
((name . "u32vector-reverse-copy")
 (signature
   case-lambda
   (((u32vector? u32vec)) u32vector?)
   (((u32vector? u32vec) (integer? start)) u32vector?)
   (((u32vector? u32vec) (integer? start) (integer? end)) u32vector?))
 (tags pure)
 (desc . "The same as u32vector-copy, but in reverse order."))
((name . "u32vector-append")
 (signature lambda ((u32vector? u32vec) ...) u32vector?)
 (tags pure)
 (desc . "Returns a u32vector containing all the elements of the u32vecs in order."))
((name . "u32vector-concatenate")
 (signature lambda ((list? list-of-u32vectors)) u32vector?)
 (subsigs
   (list-of-u32vectors (list u32vector?)))
 (tags pure)
 (desc . "The same as u32vector-append, but takes a list of u32vectors rather than multiple arguments."))
((name . "u32vector-append-subvectors")
 (signature
   lambda
   ((u32vector? u32vec1) (integer? start1) (integer? end1) ...)
   u32vector?)
 (tags pure)
 (desc . "Concatenates the result of applying u32vector-copy to each triplet of u32vec, start, end arguments, but may be implemented more efficiently."))
((name . "u32vector-empty?")
 (signature lambda ((u32vector? u32vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if u32vec has a length of zero, and #f otherwise."))
((name . "u32vector=")
 (signature lambda ((u32vector? u32vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the u32vecs for elementwise equality, using = to do the comparisons. Returns #f unless all u32vectors are the same length."))
((group
   ((name . "u32vector-take")
    (signature lambda ((u32vector? u32vec) (integer? n)) u32vector?)
    (tags pure))
   ((name . "u32vector-take-right")
    (signature lambda ((u32vector? u32vec) (integer? n)) u32vector?)
    (tags pure)))
 (desc . "Returns a u32vector containing the first/last n elements of u32vec."))
((group
   ((name . "u32vector-drop")
    (signature lambda ((u32vector? u32vec) (integer? n)) u32vector?)
    (tags pure))
   ((name . "u32vector-drop-right")
    (signature lambda ((u32vector? u32vec) (integer? n)) u32vector?)
    (tags pure)))
 (desc . "Returns a u32vector containing all except the first/last n elements of u32vec."))
((name . "u32vector-segment")
 (signature lambda ((u32vector? u32vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of u32vectors, each of which contains n consecutive elements of u32vec. The last u32vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "u32vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (u32vector? u32vec1) (u32vector? u32vec2) ...)
      *)
    (subsigs (kons (lambda (state (u32? obj1) (u32? obj2) ...) *)))
    (tags pure))
   ((name . "u32vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (u32vector? u32vec1) (u32vector? u32vec2) ...)
      *)
    (subsigs (kons (lambda (state (u32? obj1) (u32? obj2) ...) *)))
    (tags pure)))
 (desc . "When one u32vector argument u32vec is given, folds kons over the elements of u32vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple u32vector arguments are given, kons is called with the current state value and each value from all the vectors; u32vector-fold scans elements from left to right, while u32vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "u32vector-map")
    (signature
      lambda
      ((procedure? proc) (u32vector? u32vec1) (u32vector? u32vec2) ...)
      vector?)
    (subsigs (proc (lambda ((u32? val1) ...) *)))
    (tags pure))
   ((name . "u32vector-map!")
    (signature
      lambda
      ((procedure? proc) (u32vector? u32vec1) (u32vector? u32vec2) ...)
      undefined)
    (subsigs (proc (lambda ((u32? val1) ...) *))))
   ((name . "u32vector-for-each")
    (signature
      lambda
      ((procedure? proc) (u32vector? u32vec1) (u32vector? u32vec2) ...)
      undefined)
    (subsigs (proc (lambda ((u32? val1) ...) undefined)))))
 (desc . "Iterate over the elements of u32vec and apply f to each, returning respectively a u32vector of the results, an undefined value with the results placed back in u32vec, and an undefined value with no change to u32vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For u32vector-map!, only u32vec is modified even when multiple vectors are passed.
If u32vector-map or u32vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "u32vector-count")
 (signature
   lambda
   ((procedure? pred?) (u32vector? u32vec1) (u32vector? u32vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((u32? val1) (u32? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of u32vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "u32vector-cumulate")
 (signature lambda ((procedure? f) knil (u32vector? u32vec)) u32vector?)
 (subsigs (f (lambda (state (u32? value)) *)))
 (tags pure)
 (desc . "Like u32vector-fold, but returns an u32vector of partial results rather than just the final result."))
((group
   ((name . "u32vector-take-while")
    (signature lambda ((procedure? pred?) (u32vector? u32vec)) u32vector?)
    (subsigs (pred? (lambda ((u32? value)) boolean?)))
    (tags pure))
   ((name . "u32vector-take-while-right")
    (signature lambda ((procedure? pred?) (u32vector? u32vec)) u32vector?)
    (subsigs (pred? (lambda ((u32? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of u32vec all of whose elements satisfy pred?."))
((group
   ((name . "u32vector-drop-while")
    (signature lambda ((procedure? pred?) (u32vector? u32vec)) u32vector?)
    (subsigs (pred? (lambda ((u32? value)) boolean?)))
    (tags pure))
   ((name . "u32vector-drop-while-right")
    (signature lambda ((procedure? pred?) (u32vector? u32vec)) u32vector?)
    (subsigs (pred? (lambda ((u32? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of u32vec such that all its elements satisfy pred."))
((group
   ((name . "u32vector-index")
    (signature
      lambda
      ((procedure? pred?) (u32vector? u32vec1) (u32vector? u32vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u32? value1) (u32? value2) ...) *)))
    (tags pure))
   ((name . "u32vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (u32vector? u32vec1) (u32vector? u32vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u32? value1) (u32? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of u32vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, u32vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for u32vector-index-right."))
((group
   ((name . "u32vector-skip")
    (signature
      lambda
      ((procedure? pred?) (u32vector? u32vec1) (u32vector? u32vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u32? value1) (u32? value2) ...) *)))
    (tags pure))
   ((name . "u32vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (u32vector? u32vec1) (u32vector? u32vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u32? value1) (u32? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of u32vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, u32vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for u32vector-skip-right."))
((name . "u32vector-any")
 (signature lambda ((procedure? pred?) (u32vector? u32vec1) (u32vector? u32vec2) ...) *)
 (subsigs (pred? (lambda ((u32? val1) (u32? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the u32vec, or #f if there is no such element. If u32vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "u32vector-every")
 (signature lambda ((procedure? pred?) (u32vector? u32vec1) (u32vector? u32vec2) ...) *)
 (subsigs (pred? (lambda ((u32? val1) (u32? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from u32vec satisfy pred?, return the last result of pred?. If not all do, return #f. If u32vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "u32vector-partition")
 (signature
   lambda
   ((procedure? pred?) (u32vector? u32vec))
   (values u32vector? integer?))
 (subsigs (pred? (lambda ((u32? value)) boolean?)))
 (tags pure)
 (desc . "Returns an u32vector of the same type as u32vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new u32vector and the number of elements satisfying pred?."))
((group
   ((name . "u32vector-filter")
    (signature lambda ((procedure? pred?) (u32vector? u32vec1)) u32vector?)
    (subsigs (pred? (lambda ((u32? val)) boolean?)))
    (tags pure))
   ((name . "u32vector-remove")
    (signature lambda ((procedure? pred?) (u32vector? u32vec1)) u32vector?)
    (subsigs (pred? (lambda ((u32? val)) boolean?)))
    (tags pure)))
 (desc . "Return an u32vector containing the elements of u32vec that satisfy / do not satisfy pred?."))
((name . "u32vector-swap!")
 (signature lambda ((u32vector? u32vec) (integer? i) (integer? j)) undefined)
 (desc . "Interchanges the ith and jth elements of u32vec."))
((name . "u32vector-fill!")
 (signature
   case-lambda
   (((u32vector? u32vec) (u32? fill)) undefined)
   (((u32vector? u32vec) (u32? fill) (integer? start)) undefined)
   (((u32vector? u32vec) (u32? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of u32vec from start to end with the value fill."))
((name . "u32vector-reverse!")
 (signature
   case-lambda
   (((u32vector? u32vec)) undefined)
   (((u32vector? u32vec) (integer? start)) undefined)
   (((u32vector? u32vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of u32vec from start to end."))
((name . "u32vector-copy!")
 (signature
   case-lambda
   (((u32vector? u32to) (integer? at) (u32vector? u32from)) undefined)
   (((u32vector? u32to) (integer? at) (u32vector? u32from) (integer? start)) undefined)
   (((u32vector? u32to)
     (integer? at)
     (u32vector? u32from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of u32from from start to end onto u32to, starting at index at."))
((name . "u32vector-reverse-copy!")
 (signature
   case-lambda
   (((u32vector? u32to) (integer? at) (u32vector? u32from)) undefined)
   (((u32vector? u32to) (integer? at) (u32vector? u32from) (integer? start)) undefined)
   (((u32vector? u32to)
     (integer? at)
     (u32vector? u32from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as u32vector-copy!, but copies in reverse"))
((name . "u32vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (u32vector? u32vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like u32vector-unfold, but the elements are copied into the vector u32vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "u32vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (u32vector? u32vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as u32vector-unfold!, but initializes the u32vector from right to left."))
((name . "reverse-u32vector->list")
 (signature
   case-lambda
   (((u32vector? u32vec)) list?)
   (((u32vector? u32vec) (integer? start)) list?)
   (((u32vector? u32vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as u32vec in reverse order."))
((name . "reverse-list->u32vector")
 (signature lambda ((list? proper-list)) u32vector?)
 (tags pure)
 (desc . "Returns u32vector with same elements as list in reverse order."))
((name . "u32vector->vector")
 (signature
   case-lambda
   (((u32vector? u32vec)) vector?)
   (((u32vector? u32vec) (integer? start)) vector?)
   (((u32vector? u32vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as u32vec."))
((name . "vector->u32vector")
 (signature
   case-lambda
   (((vector? vec)) u32vector?)
   (((vector? vec) (integer? start)) u32vector?)
   (((vector? vec) (integer? start) (integer? end)) u32vector?))
 (tags pure)
 (desc . "Returns u32vector with same elements as vec."))
((name . "make-u32vector-generator")
 (signature lambda ((u32vector? u32vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? u32?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of u32vector in order. Note that the generator is finite."))
((name . "u32vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of u32vector."))
((name . "write-u32vector")
 (signature
   case-lambda
   (((u32vector u32vec)) undefined)
   (((u32vector u32vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of u32vec in the lexical syntax explained below."))
)
