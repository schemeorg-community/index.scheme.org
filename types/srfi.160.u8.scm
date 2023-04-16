(
((name . "make-u8vector")
 (signature
   case-lambda
   (((integer? size)) u8vector?)
   (((integer? size) (u8? fill)) u8vector?))
 (tags pure)
 (desc . "Returns a u8vector whose length is size. If fill is provided, all the elements of the u8vector are initialized to it."))
((name . "u8vector") 
 (signature lambda ((u8? value) ...) u8vector?)
 (tags pure)
 (desc . "Returns a u8vector initialized with values."))
((name . "u8vector?") 
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a u8vector, and #f otherwise."))
((name . "u8vector-length")
 (signature lambda ((u8vector? u8vec)) integer?)
 (tags pure)
 (desc . "Returns the length of u8vec"))
((name . "u8vector-ref")
 (signature lambda ((u8vector? u8vec) (integer? i)) u8?)
 (tags pure)
 (desc . "Returns the ith element of u8vec."))
((name . "u8vector-set!")
 (signature lambda ((u8vector? u8vec) (integer? i) (u8? value)) undefined)
 (desc . "Sets the ith element of u8vec to value."))
((name . "u8vector->list")
 (signature
   case-lambda
   (((u8vector? u8vec)) list?)
   (((u8vector? u8vec) (integer? start)) list?)
   (((u8vector? u8vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as u8vec"))
((name . "list->u8vector")
 (signature lambda ((list? proper-list)) u8vector?)
 (subsigs
   (proper-list (list u8?)))
 (tags pure)
 (desc . "Returns u8vector with same elements as list."))
((name . "u8?")
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a valid element of an u8vector (ie., unsigned exact integer in the range 0 to (2^8)-1), and #f otherwise."))
((name . "u8vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) u8vector?)
   (((procedure? f) (integer? length) seed) u8vector?))
 (subsigs (f (lambda ((integer? index) state) (values u8? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "u8vector-copy")
 (signature
   case-lambda
   (((u8vector? u8vec)) u8vector?)
   (((u8vector? u8vec) (integer? start)) u8vector?)
   (((u8vector? u8vec) (integer? start) (integer? end)) u8vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of u8vec from start to end and returns it."))
((name . "u8vector-reverse-copy")
 (signature
   case-lambda
   (((u8vector? u8vec)) u8vector?)
   (((u8vector? u8vec) (integer? start)) u8vector?)
   (((u8vector? u8vec) (integer? start) (integer? end)) u8vector?))
 (tags pure)
 (desc . "The same as u8vector-copy, but in reverse order."))
((name . "u8vector-append")
 (signature lambda ((u8vector? u8vec) ...) u8vector?)
 (tags pure)
 (desc . "Returns a u8vector containing all the elements of the u8vecs in order."))
((name . "u8vector-concatenate")
 (signature lambda ((list? list-of-u8vectors)) u8vector?)
 (subsigs
   (list-of-u8vectors (list u8vector?)))
 (tags pure)
 (desc . "The same as u8vector-append, but takes a list of u8vectors rather than multiple arguments."))
((name . "u8vector-append-subvectors")
 (signature
   lambda
   ((u8vector? u8vec1) (integer? start1) (integer? end1) ...)
   u8vector?)
 (tags pure)
 (desc . "Concatenates the result of applying u8vector-copy to each triplet of u8vec, start, end arguments, but may be implemented more efficiently."))
((name . "u8vector-empty?")
 (signature lambda ((u8vector? u8vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if u8vec has a length of zero, and #f otherwise."))
((name . "u8vector=")
 (signature lambda ((u8vector? u8vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the u8vecs for elementwise equality, using = to do the comparisons. Returns #f unless all u8vectors are the same length."))
((group
   ((name . "u8vector-take")
    (signature lambda ((u8vector? u8vec) (integer? n)) u8vector?)
    (tags pure))
   ((name . "u8vector-take-right")
    (signature lambda ((u8vector? u8vec) (integer? n)) u8vector?)
    (tags pure)))
 (desc . "Returns a u8vector containing the first/last n elements of u8vec."))
((group
   ((name . "u8vector-drop")
    (signature lambda ((u8vector? u8vec) (integer? n)) u8vector?)
    (tags pure))
   ((name . "u8vector-drop-right")
    (signature lambda ((u8vector? u8vec) (integer? n)) u8vector?)
    (tags pure)))
 (desc . "Returns a u8vector containing all except the first/last n elements of u8vec."))
((name . "u8vector-segment")
 (signature lambda ((u8vector? u8vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of u8vectors, each of which contains n consecutive elements of u8vec. The last u8vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "u8vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (u8vector? u8vec1) (u8vector? u8vec2) ...)
      *)
    (subsigs (kons (lambda (state (u8? obj1) (u8? obj2) ...) *)))
    (tags pure))
   ((name . "u8vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (u8vector? u8vec1) (u8vector? u8vec2) ...)
      *)
    (subsigs (kons (lambda (state (u8? obj1) (u8? obj2) ...) *)))
    (tags pure)))
 (desc . "When one u8vector argument u8vec is given, folds kons over the elements of u8vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple u8vector arguments are given, kons is called with the current state value and each value from all the vectors; u8vector-fold scans elements from left to right, while u8vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "u8vector-map")
    (signature
      lambda
      ((procedure? proc) (u8vector? u8vec1) (u8vector? u8vec2) ...)
      vector?)
    (subsigs (proc (lambda ((u8? val1) ...) *)))
    (tags pure))
   ((name . "u8vector-map!")
    (signature
      lambda
      ((procedure? proc) (u8vector? u8vec1) (u8vector? u8vec2) ...)
      undefined)
    (subsigs (proc (lambda ((u8? val1) ...) *))))
   ((name . "u8vector-for-each")
    (signature
      lambda
      ((procedure? proc) (u8vector? u8vec1) (u8vector? u8vec2) ...)
      undefined)
    (subsigs (proc (lambda ((u8? val1) ...) undefined)))))
 (desc . "Iterate over the elements of u8vec and apply f to each, returning respectively a u8vector of the results, an undefined value with the results placed back in u8vec, and an undefined value with no change to u8vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For u8vector-map!, only u8vec is modified even when multiple vectors are passed.
If u8vector-map or u8vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "u8vector-count")
 (signature
   lambda
   ((procedure? pred?) (u8vector? u8vec1) (u8vector? u8vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((u8? val1) (u8? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of u8vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "u8vector-cumulate")
 (signature lambda ((procedure? f) knil (u8vector? u8vec)) u8vector?)
 (subsigs (f (lambda (state (u8? value)) *)))
 (tags pure)
 (desc . "Like u8vector-fold, but returns an u8vector of partial results rather than just the final result."))
((group
   ((name . "u8vector-take-while")
    (signature lambda ((procedure? pred?) (u8vector? u8vec)) u8vector?)
    (subsigs (pred? (lambda ((u8? value)) boolean?)))
    (tags pure))
   ((name . "u8vector-take-while-right")
    (signature lambda ((procedure? pred?) (u8vector? u8vec)) u8vector?)
    (subsigs (pred? (lambda ((u8? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of u8vec all of whose elements satisfy pred?."))
((group
   ((name . "u8vector-drop-while")
    (signature lambda ((procedure? pred?) (u8vector? u8vec)) u8vector?)
    (subsigs (pred? (lambda ((u8? value)) boolean?)))
    (tags pure))
   ((name . "u8vector-drop-while-right")
    (signature lambda ((procedure? pred?) (u8vector? u8vec)) u8vector?)
    (subsigs (pred? (lambda ((u8? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of u8vec such that all its elements satisfy pred."))
((group
   ((name . "u8vector-index")
    (signature
      lambda
      ((procedure? pred?) (u8vector? u8vec1) (u8vector? u8vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u8? value1) (u8? value2) ...) *)))
    (tags pure))
   ((name . "u8vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (u8vector? u8vec1) (u8vector? u8vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u8? value1) (u8? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of u8vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, u8vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for u8vector-index-right."))
((group
   ((name . "u8vector-skip")
    (signature
      lambda
      ((procedure? pred?) (u8vector? u8vec1) (u8vector? u8vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u8? value1) (u8? value2) ...) *)))
    (tags pure))
   ((name . "u8vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (u8vector? u8vec1) (u8vector? u8vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((u8? value1) (u8? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of u8vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, u8vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for u8vector-skip-right."))
((name . "u8vector-any")
 (signature lambda ((procedure? pred?) (u8vector? u8vec1) (u8vector? u8vec2) ...) *)
 (subsigs (pred? (lambda ((u8? val1) (u8? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the u8vec, or #f if there is no such element. If u8vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "u8vector-every")
 (signature lambda ((procedure? pred?) (u8vector? u8vec1) (u8vector? u8vec2) ...) *)
 (subsigs (pred? (lambda ((u8? val1) (u8? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from u8vec satisfy pred?, return the last result of pred?. If not all do, return #f. If u8vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "u8vector-partition")
 (signature
   lambda
   ((procedure? pred?) (u8vector? u8vec))
   (values u8vector? integer?))
 (subsigs (pred? (lambda ((u8? value)) boolean?)))
 (tags pure)
 (desc . "Returns an u8vector of the same type as u8vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new u8vector and the number of elements satisfying pred?."))
((group
   ((name . "u8vector-filter")
    (signature lambda ((procedure? pred?) (u8vector? u8vec1)) u8vector?)
    (subsigs (pred? (lambda ((u8? val)) boolean?)))
    (tags pure))
   ((name . "u8vector-remove")
    (signature lambda ((procedure? pred?) (u8vector? u8vec1)) u8vector?)
    (subsigs (pred? (lambda ((u8? val)) boolean?)))
    (tags pure)))
 (desc . "Return an u8vector containing the elements of u8vec that satisfy / do not satisfy pred?."))
((name . "u8vector-swap!")
 (signature lambda ((u8vector? u8vector) (integer? i) (integer? j)) undefined))
((name . "u8vector-fill!")
 (signature
   case-lambda
   (((u8vector? u8vec) (u8? fill)) undefined)
   (((u8vector? u8vec) (u8? fill) (integer? start)) undefined)
   (((u8vector? u8vec) (u8? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of u8vec from start to end with the value fill."))
((name . "u8vector-reverse!")
 (signature
   case-lambda
   (((u8vector? u8vec)) undefined)
   (((u8vector? u8vec) (integer? start)) undefined)
   (((u8vector? u8vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of u8vec from start to end."))
((name . "u8vector-copy!")
 (signature
   case-lambda
   (((u8vector? u8to) (integer? at) (u8vector? u8from)) undefined)
   (((u8vector? u8to) (integer? at) (u8vector? u8from) (integer? start)) undefined)
   (((u8vector? u8to)
     (integer? at)
     (u8vector? u8from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of u8from from start to end onto u8to, starting at index at."))
((name . "u8vector-reverse-copy!")
 (signature
   case-lambda
   (((u8vector? u8to) (integer? at) (u8vector? u8from)) undefined)
   (((u8vector? u8to) (integer? at) (u8vector? u8from) (integer? start)) undefined)
   (((u8vector? u8to)
     (integer? at)
     (u8vector? u8from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as u8vector-copy!, but copies in reverse"))
((name . "u8vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (u8vector? u8vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like u8vector-unfold, but the elements are copied into the vector u8vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "u8vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (u8vector? u8vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as u8vector-unfold!, but initializes the u8vector from right to left."))
((name . "reverse-u8vector->list")
 (signature
   case-lambda
   (((u8vector? u8vec)) list?)
   (((u8vector? u8vec) (integer? start)) list?)
   (((u8vector? u8vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as u8vec in reverse order."))
((name . "reverse-list->u8vector")
 (signature lambda ((list? proper-list)) u8vector?)
 (tags pure)
 (desc . "Returns u8vector with same elements as list in reverse order."))
((name . "u8vector->vector")
 (signature
   case-lambda
   (((u8vector? u8vec)) vector?)
   (((u8vector? u8vec) (integer? start)) vector?)
   (((u8vector? u8vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as u8vec."))
((name . "vector->u8vector")
 (signature
   case-lambda
   (((vector? vec)) u8vector?)
   (((vector? vec) (integer? start)) u8vector?)
   (((vector? vec) (integer? start) (integer? end)) u8vector?))
 (tags pure)
 (desc . "Returns u8vector with same elements as vec."))
((name . "make-u8vector-generator")
 (signature lambda ((u8vector? u8vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? u8?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of u8vector in order. Note that the generator is finite."))
((name . "u8vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of u8vector."))
((name . "write-u8vector")
 (signature
   case-lambda
   (((u8vector u8vec)) undefined)
   (((u8vector u8vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of u8vec in the lexical syntax explained below."))
)
