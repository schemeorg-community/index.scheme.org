(
((name . "make-c64vector")
 (signature
   case-lambda
   (((integer? size)) c64vector?)
   (((integer? size) (c64? fill)) c64vector?))
 (tags pure)
 (desc . "Returns a c64vector whose length is size. If fill is provided, all the elements of the c64vector are initialized to it."))
((name . "c64vector") 
 (signature lambda ((c64? value) ...) c64vector?)
 (tags pure)
 (desc . "Returns a c64vector initialized with values."))
((name . "c64vector?") 
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a c64vector, and #f otherwise."))
((name . "c64vector-length")
 (signature lambda ((c64vector? c64vec)) integer?)
 (tags pure)
 (desc . "Returns the length of c64vec"))
((name . "c64vector-ref")
 (signature lambda ((c64vector? c64vec) (integer? i)) c64?)
 (tags pure)
 (desc . "Returns the ith element of c64vec."))
((name . "c64vector-set!")
 (signature lambda ((c64vector? c64vec) (integer? i) (c64? value)) undefined)
 (desc . "Sets the ith element of c64vec to value."))
((name . "c64vector->list")
 (signature
   case-lambda
   (((c64vector? c64vec)) list?)
   (((c64vector? c64vec) (integer? start)) list?)
   (((c64vector? c64vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as c64vec"))
((name . "list->c64vector")
 (signature lambda ((list? proper-list)) c64vector?)
 (subsigs
   (proper-list (list c64?)))
 (tags pure)
 (desc . "Returns c64vector with same elements as list."))
((name . "c64?")
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a valid element of an c64vector (ie., inexact complex), and #f otherwise."))
((name . "c64vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) c64vector?)
   (((procedure? f) (integer? length) seed) c64vector?))
 (subsigs (f (lambda ((integer? index) state) (values c64? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "c64vector-copy")
 (signature
   case-lambda
   (((c64vector? c64vec)) c64vector?)
   (((c64vector? c64vec) (integer? start)) c64vector?)
   (((c64vector? c64vec) (integer? start) (integer? end)) c64vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of c64vec from start to end and returns it."))
((name . "c64vector-reverse-copy")
 (signature
   case-lambda
   (((c64vector? c64vec)) c64vector?)
   (((c64vector? c64vec) (integer? start)) c64vector?)
   (((c64vector? c64vec) (integer? start) (integer? end)) c64vector?))
 (tags pure)
 (desc . "The same as c64vector-copy, but in reverse order."))
((name . "c64vector-append")
 (signature lambda ((c64vector? c64vec) ...) c64vector?)
 (tags pure)
 (desc . "Returns a c64vector containing all the elements of the c64vecs in order."))
((name . "c64vector-concatenate")
 (signature lambda ((list? list-of-c64vectors)) c64vector?)
 (subsigs
   (list-of-c64vectors (list c64vector?)))
 (tags pure)
 (desc . "The same as c64vector-append, but takes a list of c64vectors rather than multiple arguments."))
((name . "c64vector-append-subvectors")
 (signature
   lambda
   ((c64vector? c64vec1) (integer? start1) (integer? end1) ...)
   c64vector?)
 (tags pure)
 (desc . "Concatenates the result of applying c64vector-copy to each triplet of c64vec, start, end arguments, but may be implemented more efficiently."))
((name . "c64vector-empty?")
 (signature lambda ((c64vector? c64vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if c64vec has a length of zero, and #f otherwise."))
((name . "c64vector=")
 (signature lambda ((c64vector? c64vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the c64vecs for elementwise equality, using = to do the comparisons. Returns #f unless all c64vectors are the same length."))
((group
   ((name . "c64vector-take")
    (signature lambda ((c64vector? c64vec) (integer? n)) c64vector?)
    (tags pure))
   ((name . "c64vector-take-right")
    (signature lambda ((c64vector? c64vec) (integer? n)) c64vector?)
    (tags pure)))
 (desc . "Returns a c64vector containing the first/last n elements of c64vec."))
((group
   ((name . "c64vector-drop")
    (signature lambda ((c64vector? c64vec) (integer? n)) c64vector?)
    (tags pure))
   ((name . "c64vector-drop-right")
    (signature lambda ((c64vector? c64vec) (integer? n)) c64vector?)
    (tags pure)))
 (desc . "Returns a c64vector containing all except the first/last n elements of c64vec."))
((name . "c64vector-segment")
 (signature lambda ((c64vector? c64vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of c64vectors, each of which contains n consecutive elements of c64vec. The last c64vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "c64vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (c64vector? c64vec1) (c64vector? c64vec2) ...)
      *)
    (subsigs (kons (lambda (state (c64? obj1) (c64? obj2) ...) *)))
    (tags pure))
   ((name . "c64vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (c64vector? c64vec1) (c64vector? c64vec2) ...)
      *)
    (subsigs (kons (lambda (state (c64? obj1) (c64? obj2) ...) *)))
    (tags pure)))
 (desc . "When one c64vector argument c64vec is given, folds kons over the elements of c64vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple c64vector arguments are given, kons is called with the current state value and each value from all the vectors; c64vector-fold scans elements from left to right, while c64vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "c64vector-map")
    (signature
      lambda
      ((procedure? proc) (c64vector? c64vec1) (c64vector? c64vec2) ...)
      vector?)
    (subsigs (proc (lambda ((c64? val1) ...) *)))
    (tags pure))
   ((name . "c64vector-map!")
    (signature
      lambda
      ((procedure? proc) (c64vector? c64vec1) (c64vector? c64vec2) ...)
      undefined)
    (subsigs (proc (lambda ((c64? val1) ...) *))))
   ((name . "c64vector-for-each")
    (signature
      lambda
      ((procedure? proc) (c64vector? c64vec1) (c64vector? c64vec2) ...)
      undefined)
    (subsigs (proc (lambda ((c64? val1) ...) undefined)))))
 (desc . "Iterate over the elements of c64vec and apply f to each, returning respectively a c64vector of the results, an undefined value with the results placed back in c64vec, and an undefined value with no change to c64vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For c64vector-map!, only c64vec is modified even when multiple vectors are passed.
If c64vector-map or c64vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "c64vector-count")
 (signature
   lambda
   ((procedure? pred?) (c64vector? c64vec1) (c64vector? c64vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((c64? val1) (c64? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of c64vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "c64vector-cumulate")
 (signature lambda ((procedure? f) knil (c64vector? c64vec)) c64vector?)
 (subsigs (f (lambda (state (c64? value)) *)))
 (tags pure)
 (desc . "Like c64vector-fold, but returns an c64vector of partial results rather than just the final result."))
((group
   ((name . "c64vector-take-while")
    (signature lambda ((procedure? pred?) (c64vector? c64vec)) c64vector?)
    (subsigs (pred? (lambda ((c64? value)) boolean?)))
    (tags pure))
   ((name . "c64vector-take-while-right")
    (signature lambda ((procedure? pred?) (c64vector? c64vec)) c64vector?)
    (subsigs (pred? (lambda ((c64? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of c64vec all of whose elements satisfy pred?."))
((group
   ((name . "c64vector-drop-while")
    (signature lambda ((procedure? pred?) (c64vector? c64vec)) c64vector?)
    (subsigs (pred? (lambda ((c64? value)) boolean?)))
    (tags pure))
   ((name . "c64vector-drop-while-right")
    (signature lambda ((procedure? pred?) (c64vector? c64vec)) c64vector?)
    (subsigs (pred? (lambda ((c64? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of c64vec such that all its elements satisfy pred."))
((group
   ((name . "c64vector-index")
    (signature
      lambda
      ((procedure? pred?) (c64vector? c64vec1) (c64vector? c64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((c64? value1) (c64? value2) ...) *)))
    (tags pure))
   ((name . "c64vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (c64vector? c64vec1) (c64vector? c64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((c64? value1) (c64? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of c64vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, c64vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for c64vector-index-right."))
((group
   ((name . "c64vector-skip")
    (signature
      lambda
      ((procedure? pred?) (c64vector? c64vec1) (c64vector? c64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((c64? value1) (c64? value2) ...) *)))
    (tags pure))
   ((name . "c64vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (c64vector? c64vec1) (c64vector? c64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((c64? value1) (c64? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of c64vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, c64vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for c64vector-skip-right."))
((name . "c64vector-any")
 (signature lambda ((procedure? pred?) (c64vector? c64vec1) (c64vector? c64vec2) ...) *)
 (subsigs (pred? (lambda ((c64? val1) (c64? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the c64vec, or #f if there is no such element. If c64vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "c64vector-every")
 (signature lambda ((procedure? pred?) (c64vector? c64vec1) (c64vector? c64vec2) ...) *)
 (subsigs (pred? (lambda ((c64? val1) (c64? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from c64vec satisfy pred?, return the last result of pred?. If not all do, return #f. If c64vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "c64vector-partition")
 (signature
   lambda
   ((procedure? pred?) (c64vector? c64vec))
   (values c64vector? integer?))
 (subsigs (pred? (lambda ((c64? value)) boolean?)))
 (tags pure)
 (desc . "Returns an c64vector of the same type as c64vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new c64vector and the number of elements satisfying pred?."))
((group
   ((name . "c64vector-filter")
    (signature lambda ((procedure? pred?) (c64vector? c64vec1)) c64vector?)
    (subsigs (pred? (lambda ((c64? val)) boolean?)))
    (tags pure))
   ((name . "c64vector-remove")
    (signature lambda ((procedure? pred?) (c64vector? c64vec1)) c64vector?)
    (subsigs (pred? (lambda ((c64? val)) boolean?)))
    (tags pure)))
 (desc . "Return an c64vector containing the elements of c64vec that satisfy / do not satisfy pred?."))
((name . "c64vector-swap!")
 (signature lambda ((c64vector? c64vec) (integer? i) (integer? j)) undefined)
 (desc . "Interchanges the ith and jth elements of c64vec."))
((name . "c64vector-fill!")
 (signature
   case-lambda
   (((c64vector? c64vec) (c64? fill)) undefined)
   (((c64vector? c64vec) (c64? fill) (integer? start)) undefined)
   (((c64vector? c64vec) (c64? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of c64vec from start to end with the value fill."))
((name . "c64vector-reverse!")
 (signature
   case-lambda
   (((c64vector? c64vec)) undefined)
   (((c64vector? c64vec) (integer? start)) undefined)
   (((c64vector? c64vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of c64vec from start to end."))
((name . "c64vector-copy!")
 (signature
   case-lambda
   (((c64vector? c64to) (integer? at) (c64vector? c64from)) undefined)
   (((c64vector? c64to) (integer? at) (c64vector? c64from) (integer? start)) undefined)
   (((c64vector? c64to)
     (integer? at)
     (c64vector? c64from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of c64from from start to end onto c64to, starting at index at."))
((name . "c64vector-reverse-copy!")
 (signature
   case-lambda
   (((c64vector? c64to) (integer? at) (c64vector? c64from)) undefined)
   (((c64vector? c64to) (integer? at) (c64vector? c64from) (integer? start)) undefined)
   (((c64vector? c64to)
     (integer? at)
     (c64vector? c64from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as c64vector-copy!, but copies in reverse"))
((name . "c64vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (c64vector? c64vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like c64vector-unfold, but the elements are copied into the vector c64vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "c64vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (c64vector? c64vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as c64vector-unfold!, but initializes the c64vector from right to left."))
((name . "reverse-c64vector->list")
 (signature
   case-lambda
   (((c64vector? c64vec)) list?)
   (((c64vector? c64vec) (integer? start)) list?)
   (((c64vector? c64vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as c64vec in reverse order."))
((name . "reverse-list->c64vector")
 (signature lambda ((list? proper-list)) c64vector?)
 (tags pure)
 (desc . "Returns c64vector with same elements as list in reverse order."))
((name . "c64vector->vector")
 (signature
   case-lambda
   (((c64vector? c64vec)) vector?)
   (((c64vector? c64vec) (integer? start)) vector?)
   (((c64vector? c64vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as c64vec."))
((name . "vector->c64vector")
 (signature
   case-lambda
   (((vector? vec)) c64vector?)
   (((vector? vec) (integer? start)) c64vector?)
   (((vector? vec) (integer? start) (integer? end)) c64vector?))
 (tags pure)
 (desc . "Returns c64vector with same elements as vec."))
((name . "make-c64vector-generator")
 (signature lambda ((c64vector? c64vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? c64?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of c64vector in order. Note that the generator is finite."))
((name . "c64vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of c64vector."))
((name . "write-c64vector")
 (signature
   case-lambda
   (((c64vector c64vec)) undefined)
   (((c64vector c64vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of c64vec in the lexical syntax explained below."))
)
