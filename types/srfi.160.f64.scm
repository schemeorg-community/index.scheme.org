(
((name . "make-f64vector")
 (signature
   case-lambda
   (((integer? size)) f64vector?)
   (((integer? size) (f64? fill)) f64vector?))
 (tags pure)
 (desc . "Returns a f64vector whose length is size. If fill is provided, all the elements of the f64vector are initialized to it."))
((name . "f64vector") 
 (signature lambda ((f64? value) ...) f64vector?)
 (tags pure)
 (desc . "Returns a f64vector initialized with values."))
((name . "f64vector?") 
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a f64vector, and #f otherwise."))
((name . "f64vector-length")
 (signature lambda ((f64vector? f64vec)) integer?)
 (tags pure)
 (desc . "Returns the length of f64vec"))
((name . "f64vector-ref")
 (signature lambda ((f64vector? f64vec) (integer? i)) f64?)
 (tags pure)
 (desc . "Returns the ith element of f64vec."))
((name . "f64vector-set!")
 (signature lambda ((f64vector? f64vec) (integer? i) (f64? value)) undefined)
 (desc . "Sets the ith element of f64vec to value."))
((name . "f64vector->list")
 (signature
   case-lambda
   (((f64vector? f64vec)) list?)
   (((f64vector? f64vec) (integer? start)) list?)
   (((f64vector? f64vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as f64vec"))
((name . "list->f64vector")
 (signature lambda ((list? proper-list)) f64vector?)
 (subsigs
   (proper-list (list f64?)))
 (tags pure)
 (desc . "Returns f64vector with same elements as list."))
((name . "f64?")
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a valid element of an f64vector (ie., inexact real), and #f otherwise."))
((name . "f64vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) f64vector?)
   (((procedure? f) (integer? length) seed) f64vector?))
 (subsigs (f (lambda ((integer? index) state) (values f64? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "f64vector-copy")
 (signature
   case-lambda
   (((f64vector? f64vec)) f64vector?)
   (((f64vector? f64vec) (integer? start)) f64vector?)
   (((f64vector? f64vec) (integer? start) (integer? end)) f64vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of f64vec from start to end and returns it."))
((name . "f64vector-reverse-copy")
 (signature
   case-lambda
   (((f64vector? f64vec)) f64vector?)
   (((f64vector? f64vec) (integer? start)) f64vector?)
   (((f64vector? f64vec) (integer? start) (integer? end)) f64vector?))
 (tags pure)
 (desc . "The same as f64vector-copy, but in reverse order."))
((name . "f64vector-append")
 (signature lambda ((f64vector? f64vec) ...) f64vector?)
 (tags pure)
 (desc . "Returns a f64vector containing all the elements of the f64vecs in order."))
((name . "f64vector-concatenate")
 (signature lambda ((list? list-of-f64vectors)) f64vector?)
 (subsigs
   (list-of-f64vectors (list f64vector?)))
 (tags pure)
 (desc . "The same as f64vector-append, but takes a list of f64vectors rather than multiple arguments."))
((name . "f64vector-append-subvectors")
 (signature
   lambda
   ((f64vector? f64vec1) (integer? start1) (integer? end1) ...)
   f64vector?)
 (tags pure)
 (desc . "Concatenates the result of applying f64vector-copy to each triplet of f64vec, start, end arguments, but may be implemented more efficiently."))
((name . "f64vector-empty?")
 (signature lambda ((f64vector? f64vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if f64vec has a length of zero, and #f otherwise."))
((name . "f64vector=")
 (signature lambda ((f64vector? f64vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the f64vecs for elementwise equality, using = to do the comparisons. Returns #f unless all f64vectors are the same length."))
((group
   ((name . "f64vector-take")
    (signature lambda ((f64vector? f64vec) (integer? n)) f64vector?)
    (tags pure))
   ((name . "f64vector-take-right")
    (signature lambda ((f64vector? f64vec) (integer? n)) f64vector?)
    (tags pure)))
 (desc . "Returns a f64vector containing the first/last n elements of f64vec."))
((group
   ((name . "f64vector-drop")
    (signature lambda ((f64vector? f64vec) (integer? n)) f64vector?)
    (tags pure))
   ((name . "f64vector-drop-right")
    (signature lambda ((f64vector? f64vec) (integer? n)) f64vector?)
    (tags pure)))
 (desc . "Returns a f64vector containing all except the first/last n elements of f64vec."))
((name . "f64vector-segment")
 (signature lambda ((f64vector? f64vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of f64vectors, each of which contains n consecutive elements of f64vec. The last f64vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "f64vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (f64vector? f64vec1) (f64vector? f64vec2) ...)
      *)
    (subsigs (kons (lambda (state (f64? obj1) (f64? obj2) ...) *)))
    (tags pure))
   ((name . "f64vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (f64vector? f64vec1) (f64vector? f64vec2) ...)
      *)
    (subsigs (kons (lambda (state (f64? obj1) (f64? obj2) ...) *)))
    (tags pure)))
 (desc . "When one f64vector argument f64vec is given, folds kons over the elements of f64vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple f64vector arguments are given, kons is called with the current state value and each value from all the vectors; f64vector-fold scans elements from left to right, while f64vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "f64vector-map")
    (signature
      lambda
      ((procedure? proc) (f64vector? f64vec1) (f64vector? f64vec2) ...)
      vector?)
    (subsigs (proc (lambda ((f64? val1) ...) *)))
    (tags pure))
   ((name . "f64vector-map!")
    (signature
      lambda
      ((procedure? proc) (f64vector? f64vec1) (f64vector? f64vec2) ...)
      undefined)
    (subsigs (proc (lambda ((f64? val1) ...) *))))
   ((name . "f64vector-for-each")
    (signature
      lambda
      ((procedure? proc) (f64vector? f64vec1) (f64vector? f64vec2) ...)
      undefined)
    (subsigs (proc (lambda ((f64? val1) ...) undefined)))))
 (desc . "Iterate over the elements of f64vec and apply f to each, returning respectively a f64vector of the results, an undefined value with the results placed back in f64vec, and an undefined value with no change to f64vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For f64vector-map!, only f64vec is modified even when multiple vectors are passed.
If f64vector-map or f64vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "f64vector-count")
 (signature
   lambda
   ((procedure? pred?) (f64vector? f64vec1) (f64vector? f64vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((f64? val1) (f64? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of f64vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "f64vector-cumulate")
 (signature lambda ((procedure? f) knil (f64vector? f64vec)) f64vector?)
 (subsigs (f (lambda (state (f64? value)) *)))
 (tags pure)
 (desc . "Like f64vector-fold, but returns an f64vector of partial results rather than just the final result."))
((group
   ((name . "f64vector-take-while")
    (signature lambda ((procedure? pred?) (f64vector? f64vec)) f64vector?)
    (subsigs (pred? (lambda ((f64? value)) boolean?)))
    (tags pure))
   ((name . "f64vector-take-while-right")
    (signature lambda ((procedure? pred?) (f64vector? f64vec)) f64vector?)
    (subsigs (pred? (lambda ((f64? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of f64vec all of whose elements satisfy pred?."))
((group
   ((name . "f64vector-drop-while")
    (signature lambda ((procedure? pred?) (f64vector? f64vec)) f64vector?)
    (subsigs (pred? (lambda ((f64? value)) boolean?)))
    (tags pure))
   ((name . "f64vector-drop-while-right")
    (signature lambda ((procedure? pred?) (f64vector? f64vec)) f64vector?)
    (subsigs (pred? (lambda ((f64? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of f64vec such that all its elements satisfy pred."))
((group
   ((name . "f64vector-index")
    (signature
      lambda
      ((procedure? pred?) (f64vector? f64vec1) (f64vector? f64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((f64? value1) (f64? value2) ...) *)))
    (tags pure))
   ((name . "f64vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (f64vector? f64vec1) (f64vector? f64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((f64? value1) (f64? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of f64vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, f64vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for f64vector-index-right."))
((group
   ((name . "f64vector-skip")
    (signature
      lambda
      ((procedure? pred?) (f64vector? f64vec1) (f64vector? f64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((f64? value1) (f64? value2) ...) *)))
    (tags pure))
   ((name . "f64vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (f64vector? f64vec1) (f64vector? f64vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((f64? value1) (f64? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of f64vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, f64vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for f64vector-skip-right."))
((name . "f64vector-any")
 (signature lambda ((procedure? pred?) (f64vector? f64vec1) (f64vector? f64vec2) ...) *)
 (subsigs (pred? (lambda ((f64? val1) (f64? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the f64vec, or #f if there is no such element. If f64vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "f64vector-every")
 (signature lambda ((procedure? pred?) (f64vector? f64vec1) (f64vector? f64vec2) ...) *)
 (subsigs (pred? (lambda ((f64? val1) (f64? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from f64vec satisfy pred?, return the last result of pred?. If not all do, return #f. If f64vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "f64vector-partition")
 (signature
   lambda
   ((procedure? pred?) (f64vector? f64vec))
   (values f64vector? integer?))
 (subsigs (pred? (lambda ((f64? value)) boolean?)))
 (tags pure)
 (desc . "Returns an f64vector of the same type as f64vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new f64vector and the number of elements satisfying pred?."))
((group
   ((name . "f64vector-filter")
    (signature lambda ((procedure? pred?) (f64vector? f64vec1)) f64vector?)
    (subsigs (pred? (lambda ((f64? val)) boolean?)))
    (tags pure))
   ((name . "f64vector-remove")
    (signature lambda ((procedure? pred?) (f64vector? f64vec1)) f64vector?)
    (subsigs (pred? (lambda ((f64? val)) boolean?)))
    (tags pure)))
 (desc . "Return an f64vector containing the elements of f64vec that satisfy / do not satisfy pred?."))
((name . "f64vector-swap!")
 (signature lambda ((f64vector? f64vec) (integer? i) (integer? j)) undefined)
 (desc . "Interchanges the ith and jth elements of f64vec."))
((name . "f64vector-fill!")
 (signature
   case-lambda
   (((f64vector? f64vec) (f64? fill)) undefined)
   (((f64vector? f64vec) (f64? fill) (integer? start)) undefined)
   (((f64vector? f64vec) (f64? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of f64vec from start to end with the value fill."))
((name . "f64vector-reverse!")
 (signature
   case-lambda
   (((f64vector? f64vec)) undefined)
   (((f64vector? f64vec) (integer? start)) undefined)
   (((f64vector? f64vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of f64vec from start to end."))
((name . "f64vector-copy!")
 (signature
   case-lambda
   (((f64vector? f64to) (integer? at) (f64vector? f64from)) undefined)
   (((f64vector? f64to) (integer? at) (f64vector? f64from) (integer? start)) undefined)
   (((f64vector? f64to)
     (integer? at)
     (f64vector? f64from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of f64from from start to end onto f64to, starting at index at."))
((name . "f64vector-reverse-copy!")
 (signature
   case-lambda
   (((f64vector? f64to) (integer? at) (f64vector? f64from)) undefined)
   (((f64vector? f64to) (integer? at) (f64vector? f64from) (integer? start)) undefined)
   (((f64vector? f64to)
     (integer? at)
     (f64vector? f64from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as f64vector-copy!, but copies in reverse"))
((name . "f64vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (f64vector? f64vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like f64vector-unfold, but the elements are copied into the vector f64vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "f64vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (f64vector? f64vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as f64vector-unfold!, but initializes the f64vector from right to left."))
((name . "reverse-f64vector->list")
 (signature
   case-lambda
   (((f64vector? f64vec)) list?)
   (((f64vector? f64vec) (integer? start)) list?)
   (((f64vector? f64vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as f64vec in reverse order."))
((name . "reverse-list->f64vector")
 (signature lambda ((list? proper-list)) f64vector?)
 (tags pure)
 (desc . "Returns f64vector with same elements as list in reverse order."))
((name . "f64vector->vector")
 (signature
   case-lambda
   (((f64vector? f64vec)) vector?)
   (((f64vector? f64vec) (integer? start)) vector?)
   (((f64vector? f64vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as f64vec."))
((name . "vector->f64vector")
 (signature
   case-lambda
   (((vector? vec)) f64vector?)
   (((vector? vec) (integer? start)) f64vector?)
   (((vector? vec) (integer? start) (integer? end)) f64vector?))
 (tags pure)
 (desc . "Returns f64vector with same elements as vec."))
((name . "make-f64vector-generator")
 (signature lambda ((f64vector? f64vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? f64?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of f64vector in order. Note that the generator is finite."))
((name . "f64vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of f64vector."))
((name . "write-f64vector")
 (signature
   case-lambda
   (((f64vector f64vec)) undefined)
   (((f64vector f64vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of f64vec in the lexical syntax explained below."))
)
