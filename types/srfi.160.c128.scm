(
((name . "make-c128vector")
 (signature
   case-lambda
   (((integer? size)) c128vector?)
   (((integer? size) (c128? fill)) c128vector?))
 (tags pure)
 (desc . "Returns a c128vector whose length is size. If fill is provided, all the elements of the c128vector are initialized to it."))
((name . "c128vector") 
 (signature lambda ((c128? value) ...) c128vector?)
 (tags pure)
 (desc . "Returns a c128vector initialized with values."))
((name . "c128vector?") 
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a c128vector, and #f otherwise."))
((name . "c128vector-length")
 (signature lambda ((c128vector? c128vec)) integer?)
 (tags pure)
 (desc . "Returns the length of c128vec"))
((name . "c128vector-ref")
 (signature lambda ((c128vector? c128vec) (integer? i)) c128?)
 (tags pure)
 (desc . "Returns the ith element of c128vec."))
((name . "c128vector-set!")
 (signature lambda ((c128vector? c128vec) (integer? i) (c128? value)) undefined)
 (desc . "Sets the ith element of c128vec to value."))
((name . "c128vector->list")
 (signature
   case-lambda
   (((c128vector? c128vec)) list?)
   (((c128vector? c128vec) (integer? start)) list?)
   (((c128vector? c128vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as c128vec"))
((name . "list->c128vector")
 (signature lambda ((list? proper-list)) c128vector?)
 (subsigs
   (proper-list (list c128?)))
 (tags pure)
 (desc . "Returns c128vector with same elements as list."))
((name . "c128?")
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a valid element of an c128vector (ie., inexact complex), and #f otherwise."))
((name . "c128vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) c128vector?)
   (((procedure? f) (integer? length) seed) c128vector?))
 (subsigs (f (lambda ((integer? index) state) (values c128? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "c128vector-copy")
 (signature
   case-lambda
   (((c128vector? c128vec)) c128vector?)
   (((c128vector? c128vec) (integer? start)) c128vector?)
   (((c128vector? c128vec) (integer? start) (integer? end)) c128vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of c128vec from start to end and returns it."))
((name . "c128vector-reverse-copy")
 (signature
   case-lambda
   (((c128vector? c128vec)) c128vector?)
   (((c128vector? c128vec) (integer? start)) c128vector?)
   (((c128vector? c128vec) (integer? start) (integer? end)) c128vector?))
 (tags pure)
 (desc . "The same as c128vector-copy, but in reverse order."))
((name . "c128vector-append")
 (signature lambda ((c128vector? c128vec) ...) c128vector?)
 (tags pure)
 (desc . "Returns a c128vector containing all the elements of the c128vecs in order."))
((name . "c128vector-concatenate")
 (signature lambda ((list? list-of-c128vectors)) c128vector?)
 (subsigs
   (list-of-c128vectors (list c128vector?)))
 (tags pure)
 (desc . "The same as c128vector-append, but takes a list of c128vectors rather than multiple arguments."))
((name . "c128vector-append-subvectors")
 (signature
   lambda
   ((c128vector? c128vec1) (integer? start1) (integer? end1) ...)
   c128vector?)
 (tags pure)
 (desc . "Concatenates the result of applying c128vector-copy to each triplet of c128vec, start, end arguments, but may be implemented more efficiently."))
((name . "c128vector-empty?")
 (signature lambda ((c128vector? c128vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if c128vec has a length of zero, and #f otherwise."))
((name . "c128vector=")
 (signature lambda ((c128vector? c128vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the c128vecs for elementwise equality, using = to do the comparisons. Returns #f unless all c128vectors are the same length."))
((group
   ((name . "c128vector-take")
    (signature lambda ((c128vector? c128vec) (integer? n)) c128vector?)
    (tags pure))
   ((name . "c128vector-take-right")
    (signature lambda ((c128vector? c128vec) (integer? n)) c128vector?)
    (tags pure)))
 (desc . "Returns a c128vector containing the first/last n elements of c128vec."))
((group
   ((name . "c128vector-drop")
    (signature lambda ((c128vector? c128vec) (integer? n)) c128vector?)
    (tags pure))
   ((name . "c128vector-drop-right")
    (signature lambda ((c128vector? c128vec) (integer? n)) c128vector?)
    (tags pure)))
 (desc . "Returns a c128vector containing all except the first/last n elements of c128vec."))
((name . "c128vector-segment")
 (signature lambda ((c128vector? c128vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of c128vectors, each of which contains n consecutive elements of c128vec. The last c128vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "c128vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (c128vector? c128vec1) (c128vector? c128vec2) ...)
      *)
    (subsigs (kons (lambda (state (c128? obj1) (c128? obj2) ...) *)))
    (tags pure))
   ((name . "c128vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (c128vector? c128vec1) (c128vector? c128vec2) ...)
      *)
    (subsigs (kons (lambda (state (c128? obj1) (c128? obj2) ...) *)))
    (tags pure)))
 (desc . "When one c128vector argument c128vec is given, folds kons over the elements of c128vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple c128vector arguments are given, kons is called with the current state value and each value from all the vectors; c128vector-fold scans elements from left to right, while c128vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "c128vector-map")
    (signature
      lambda
      ((procedure? proc) (c128vector? c128vec1) (c128vector? c128vec2) ...)
      vector?)
    (subsigs (proc (lambda ((c128? val1) ...) *)))
    (tags pure))
   ((name . "c128vector-map!")
    (signature
      lambda
      ((procedure? proc) (c128vector? c128vec1) (c128vector? c128vec2) ...)
      undefined)
    (subsigs (proc (lambda ((c128? val1) ...) *))))
   ((name . "c128vector-for-each")
    (signature
      lambda
      ((procedure? proc) (c128vector? c128vec1) (c128vector? c128vec2) ...)
      undefined)
    (subsigs (proc (lambda ((c128? val1) ...) undefined)))))
 (desc . "Iterate over the elements of c128vec and apply f to each, returning respectively a c128vector of the results, an undefined value with the results placed back in c128vec, and an undefined value with no change to c128vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For c128vector-map!, only c128vec is modified even when multiple vectors are passed.
If c128vector-map or c128vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "c128vector-count")
 (signature
   lambda
   ((procedure? pred?) (c128vector? c128vec1) (c128vector? c128vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((c128? val1) (c128? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of c128vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "c128vector-cumulate")
 (signature lambda ((procedure? f) knil (c128vector? c128vec)) c128vector?)
 (subsigs (f (lambda (state (c128? value)) *)))
 (tags pure)
 (desc . "Like c128vector-fold, but returns an c128vector of partial results rather than just the final result."))
((group
   ((name . "c128vector-take-while")
    (signature lambda ((procedure? pred?) (c128vector? c128vec)) c128vector?)
    (subsigs (pred? (lambda ((c128? value)) boolean?)))
    (tags pure))
   ((name . "c128vector-take-while-right")
    (signature lambda ((procedure? pred?) (c128vector? c128vec)) c128vector?)
    (subsigs (pred? (lambda ((c128? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of c128vec all of whose elements satisfy pred?."))
((group
   ((name . "c128vector-drop-while")
    (signature lambda ((procedure? pred?) (c128vector? c128vec)) c128vector?)
    (subsigs (pred? (lambda ((c128? value)) boolean?)))
    (tags pure))
   ((name . "c128vector-drop-while-right")
    (signature lambda ((procedure? pred?) (c128vector? c128vec)) c128vector?)
    (subsigs (pred? (lambda ((c128? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of c128vec such that all its elements satisfy pred."))
((group
   ((name . "c128vector-index")
    (signature
      lambda
      ((procedure? pred?) (c128vector? c128vec1) (c128vector? c128vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((c128? value1) (c128? value2) ...) *)))
    (tags pure))
   ((name . "c128vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (c128vector? c128vec1) (c128vector? c128vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((c128? value1) (c128? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of c128vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, c128vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for c128vector-index-right."))
((group
   ((name . "c128vector-skip")
    (signature
      lambda
      ((procedure? pred?) (c128vector? c128vec1) (c128vector? c128vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((c128? value1) (c128? value2) ...) *)))
    (tags pure))
   ((name . "c128vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (c128vector? c128vec1) (c128vector? c128vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((c128? value1) (c128? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of c128vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, c128vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for c128vector-skip-right."))
((name . "c128vector-any")
 (signature lambda ((procedure? pred?) (c128vector? c128vec1) (c128vector? c128vec2) ...) *)
 (subsigs (pred? (lambda ((c128? val1) (c128? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the c128vec, or #f if there is no such element. If c128vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "c128vector-every")
 (signature lambda ((procedure? pred?) (c128vector? c128vec1) (c128vector? c128vec2) ...) *)
 (subsigs (pred? (lambda ((c128? val1) (c128? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from c128vec satisfy pred?, return the last result of pred?. If not all do, return #f. If c128vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "c128vector-partition")
 (signature
   lambda
   ((procedure? pred?) (c128vector? c128vec))
   (values c128vector? integer?))
 (subsigs (pred? (lambda ((c128? value)) boolean?)))
 (tags pure)
 (desc . "Returns an c128vector of the same type as c128vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new c128vector and the number of elements satisfying pred?."))
((group
   ((name . "c128vector-filter")
    (signature lambda ((procedure? pred?) (c128vector? c128vec1)) c128vector?)
    (subsigs (pred? (lambda ((c128? val)) boolean?)))
    (tags pure))
   ((name . "c128vector-remove")
    (signature lambda ((procedure? pred?) (c128vector? c128vec1)) c128vector?)
    (subsigs (pred? (lambda ((c128? val)) boolean?)))
    (tags pure)))
 (desc . "Return an c128vector containing the elements of c128vec that satisfy / do not satisfy pred?."))
((name . "c128vector-swap!")
 (signature lambda ((c128vector? c128vec) (integer? i) (integer? j)) undefined)
 (desc . "Interchanges the ith and jth elements of c128vec."))
((name . "c128vector-fill!")
 (signature
   case-lambda
   (((c128vector? c128vec) (c128? fill)) undefined)
   (((c128vector? c128vec) (c128? fill) (integer? start)) undefined)
   (((c128vector? c128vec) (c128? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of c128vec from start to end with the value fill."))
((name . "c128vector-reverse!")
 (signature
   case-lambda
   (((c128vector? c128vec)) undefined)
   (((c128vector? c128vec) (integer? start)) undefined)
   (((c128vector? c128vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of c128vec from start to end."))
((name . "c128vector-copy!")
 (signature
   case-lambda
   (((c128vector? c128to) (integer? at) (c128vector? c128from)) undefined)
   (((c128vector? c128to) (integer? at) (c128vector? c128from) (integer? start)) undefined)
   (((c128vector? c128to)
     (integer? at)
     (c128vector? c128from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of c128from from start to end onto c128to, starting at index at."))
((name . "c128vector-reverse-copy!")
 (signature
   case-lambda
   (((c128vector? c128to) (integer? at) (c128vector? c128from)) undefined)
   (((c128vector? c128to) (integer? at) (c128vector? c128from) (integer? start)) undefined)
   (((c128vector? c128to)
     (integer? at)
     (c128vector? c128from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as c128vector-copy!, but copies in reverse"))
((name . "c128vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (c128vector? c128vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like c128vector-unfold, but the elements are copied into the vector c128vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "c128vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (c128vector? c128vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as c128vector-unfold!, but initializes the c128vector from right to left."))
((name . "reverse-c128vector->list")
 (signature
   case-lambda
   (((c128vector? c128vec)) list?)
   (((c128vector? c128vec) (integer? start)) list?)
   (((c128vector? c128vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as c128vec in reverse order."))
((name . "reverse-list->c128vector")
 (signature lambda ((list? proper-list)) c128vector?)
 (tags pure)
 (desc . "Returns c128vector with same elements as list in reverse order."))
((name . "c128vector->vector")
 (signature
   case-lambda
   (((c128vector? c128vec)) vector?)
   (((c128vector? c128vec) (integer? start)) vector?)
   (((c128vector? c128vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as c128vec."))
((name . "vector->c128vector")
 (signature
   case-lambda
   (((vector? vec)) c128vector?)
   (((vector? vec) (integer? start)) c128vector?)
   (((vector? vec) (integer? start) (integer? end)) c128vector?))
 (tags pure)
 (desc . "Returns c128vector with same elements as vec."))
((name . "make-c128vector-generator")
 (signature lambda ((c128vector? c128vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? c128?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of c128vector in order. Note that the generator is finite."))
((name . "c128vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of c128vector."))
((name . "write-c128vector")
 (signature
   case-lambda
   (((c128vector c128vec)) undefined)
   (((c128vector c128vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of c128vec in the lexical syntax explained below."))
)
