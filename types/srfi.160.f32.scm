(
((name . "make-f32vector")
 (signature
   case-lambda
   (((integer? size)) f32vector?)
   (((integer? size) (f32? fill)) f32vector?))
 (tags pure)
 (desc . "Returns a f32vector whose length is size. If fill is provided, all the elements of the f32vector are initialized to it."))
((name . "f32vector") 
 (signature lambda ((f32? value) ...) f32vector?)
 (tags pure)
 (desc . "Returns a f32vector initialized with values."))
((name . "f32vector?") 
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a f32vector, and #f otherwise."))
((name . "f32vector-length")
 (signature lambda ((f32vector? f32vec)) integer?)
 (tags pure)
 (desc . "Returns the length of f32vec"))
((name . "f32vector-ref")
 (signature lambda ((f32vector? f32vec) (integer? i)) f32?)
 (tags pure)
 (desc . "Returns the ith element of f32vec."))
((name . "f32vector-set!")
 (signature lambda ((f32vector? f32vec) (integer? i) (f32? value)) undefined)
 (desc . "Sets the ith element of f32vec to value."))
((name . "f32vector->list")
 (signature
   case-lambda
   (((f32vector? f32vec)) list?)
   (((f32vector? f32vec) (integer? start)) list?)
   (((f32vector? f32vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as f32vec"))
((name . "list->f32vector")
 (signature lambda ((list? proper-list)) f32vector?)
 (subsigs
   (proper-list (list f32?)))
 (tags pure)
 (desc . "Returns f32vector with same elements as list."))
((name . "f32?")
 (signature lambda (obj) boolean?)
 (tags pure predicate)
 (desc . "Returns #t if obj is a valid element of an f32vector (ie., inexact real), and #f otherwise."))
((name . "f32vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) f32vector?)
   (((procedure? f) (integer? length) seed) f32vector?))
 (subsigs (f (lambda ((integer? index) state) (values f32? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "f32vector-copy")
 (signature
   case-lambda
   (((f32vector? f32vec)) f32vector?)
   (((f32vector? f32vec) (integer? start)) f32vector?)
   (((f32vector? f32vec) (integer? start) (integer? end)) f32vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of f32vec from start to end and returns it."))
((name . "f32vector-reverse-copy")
 (signature
   case-lambda
   (((f32vector? f32vec)) f32vector?)
   (((f32vector? f32vec) (integer? start)) f32vector?)
   (((f32vector? f32vec) (integer? start) (integer? end)) f32vector?))
 (tags pure)
 (desc . "The same as f32vector-copy, but in reverse order."))
((name . "f32vector-append")
 (signature lambda ((f32vector? f32vec) ...) f32vector?)
 (tags pure)
 (desc . "Returns a f32vector containing all the elements of the f32vecs in order."))
((name . "f32vector-concatenate")
 (signature lambda ((list? list-of-f32vectors)) f32vector?)
 (subsigs
   (list-of-f32vectors (list f32vector?)))
 (tags pure)
 (desc . "The same as f32vector-append, but takes a list of f32vectors rather than multiple arguments."))
((name . "f32vector-append-subvectors")
 (signature
   lambda
   ((f32vector? f32vec1) (integer? start1) (integer? end1) ...)
   f32vector?)
 (tags pure)
 (desc . "Concatenates the result of applying f32vector-copy to each triplet of f32vec, start, end arguments, but may be implemented more efficiently."))
((name . "f32vector-empty?")
 (signature lambda ((f32vector? f32vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if f32vec has a length of zero, and #f otherwise."))
((name . "f32vector=")
 (signature lambda ((f32vector? f32vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the f32vecs for elementwise equality, using = to do the comparisons. Returns #f unless all f32vectors are the same length."))
((group
   ((name . "f32vector-take")
    (signature lambda ((f32vector? f32vec) (integer? n)) f32vector?)
    (tags pure))
   ((name . "f32vector-take-right")
    (signature lambda ((f32vector? f32vec) (integer? n)) f32vector?)
    (tags pure)))
 (desc . "Returns a f32vector containing the first/last n elements of f32vec."))
((group
   ((name . "f32vector-drop")
    (signature lambda ((f32vector? f32vec) (integer? n)) f32vector?)
    (tags pure))
   ((name . "f32vector-drop-right")
    (signature lambda ((f32vector? f32vec) (integer? n)) f32vector?)
    (tags pure)))
 (desc . "Returns a f32vector containing all except the first/last n elements of f32vec."))
((name . "f32vector-segment")
 (signature lambda ((f32vector? f32vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of f32vectors, each of which contains n consecutive elements of f32vec. The last f32vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "f32vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (f32vector? f32vec1) (f32vector? f32vec2) ...)
      *)
    (subsigs (kons (lambda (state (f32? obj1) (f32? obj2) ...) *)))
    (tags pure))
   ((name . "f32vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (f32vector? f32vec1) (f32vector? f32vec2) ...)
      *)
    (subsigs (kons (lambda (state (f32? obj1) (f32? obj2) ...) *)))
    (tags pure)))
 (desc . "When one f32vector argument f32vec is given, folds kons over the elements of f32vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple f32vector arguments are given, kons is called with the current state value and each value from all the vectors; f32vector-fold scans elements from left to right, while f32vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "f32vector-map")
    (signature
      lambda
      ((procedure? proc) (f32vector? f32vec1) (f32vector? f32vec2) ...)
      vector?)
    (subsigs (proc (lambda ((f32? val1) ...) *)))
    (tags pure))
   ((name . "f32vector-map!")
    (signature
      lambda
      ((procedure? proc) (f32vector? f32vec1) (f32vector? f32vec2) ...)
      undefined)
    (subsigs (proc (lambda ((f32? val1) ...) *))))
   ((name . "f32vector-for-each")
    (signature
      lambda
      ((procedure? proc) (f32vector? f32vec1) (f32vector? f32vec2) ...)
      undefined)
    (subsigs (proc (lambda ((f32? val1) ...) undefined)))))
 (desc . "Iterate over the elements of f32vec and apply f to each, returning respectively a f32vector of the results, an undefined value with the results placed back in f32vec, and an undefined value with no change to f32vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For f32vector-map!, only f32vec is modified even when multiple vectors are passed.
If f32vector-map or f32vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "f32vector-count")
 (signature
   lambda
   ((procedure? pred?) (f32vector? f32vec1) (f32vector? f32vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((f32? val1) (f32? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of f32vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "f32vector-cumulate")
 (signature lambda ((procedure? f) knil (f32vector? f32vec)) f32vector?)
 (subsigs (f (lambda (state (f32? value)) *)))
 (tags pure)
 (desc . "Like f32vector-fold, but returns an f32vector of partial results rather than just the final result."))
((group
   ((name . "f32vector-take-while")
    (signature lambda ((procedure? pred?) (f32vector? f32vec)) f32vector?)
    (subsigs (pred? (lambda ((f32? value)) boolean?)))
    (tags pure))
   ((name . "f32vector-take-while-right")
    (signature lambda ((procedure? pred?) (f32vector? f32vec)) f32vector?)
    (subsigs (pred? (lambda ((f32? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of f32vec all of whose elements satisfy pred?."))
((group
   ((name . "f32vector-drop-while")
    (signature lambda ((procedure? pred?) (f32vector? f32vec)) f32vector?)
    (subsigs (pred? (lambda ((f32? value)) boolean?)))
    (tags pure))
   ((name . "f32vector-drop-while-right")
    (signature lambda ((procedure? pred?) (f32vector? f32vec)) f32vector?)
    (subsigs (pred? (lambda ((f32? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of f32vec such that all its elements satisfy pred."))
((group
   ((name . "f32vector-index")
    (signature
      lambda
      ((procedure? pred?) (f32vector? f32vec1) (f32vector? f32vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((f32? value1) (f32? value2) ...) *)))
    (tags pure))
   ((name . "f32vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (f32vector? f32vec1) (f32vector? f32vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((f32? value1) (f32? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of f32vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, f32vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for f32vector-index-right."))
((group
   ((name . "f32vector-skip")
    (signature
      lambda
      ((procedure? pred?) (f32vector? f32vec1) (f32vector? f32vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((f32? value1) (f32? value2) ...) *)))
    (tags pure))
   ((name . "f32vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (f32vector? f32vec1) (f32vector? f32vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((f32? value1) (f32? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of f32vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, f32vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for f32vector-skip-right."))
((name . "f32vector-any")
 (signature lambda ((procedure? pred?) (f32vector? f32vec1) (f32vector? f32vec2) ...) *)
 (subsigs (pred? (lambda ((f32? val1) (f32? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the f32vec, or #f if there is no such element. If f32vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "f32vector-every")
 (signature lambda ((procedure? pred?) (f32vector? f32vec1) (f32vector? f32vec2) ...) *)
 (subsigs (pred? (lambda ((f32? val1) (f32? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from f32vec satisfy pred?, return the last result of pred?. If not all do, return #f. If f32vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "f32vector-partition")
 (signature
   lambda
   ((procedure? pred?) (f32vector? f32vec))
   (values f32vector? integer?))
 (subsigs (pred? (lambda ((f32? value)) boolean?)))
 (tags pure)
 (desc . "Returns an f32vector of the same type as f32vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new f32vector and the number of elements satisfying pred?."))
((group
   ((name . "f32vector-filter")
    (signature lambda ((procedure? pred?) (f32vector? f32vec1)) f32vector?)
    (subsigs (pred? (lambda ((f32? val)) boolean?)))
    (tags pure))
   ((name . "f32vector-remove")
    (signature lambda ((procedure? pred?) (f32vector? f32vec1)) f32vector?)
    (subsigs (pred? (lambda ((f32? val)) boolean?)))
    (tags pure)))
 (desc . "Return an f32vector containing the elements of f32vec that satisfy / do not satisfy pred?."))
((name . "f32vector-swap!")
 (signature lambda ((f32vector? f32vector) (integer? i) (integer? j)) undefined))
((name . "f32vector-fill!")
 (signature
   case-lambda
   (((f32vector? f32vec) (f32? fill)) undefined)
   (((f32vector? f32vec) (f32? fill) (integer? start)) undefined)
   (((f32vector? f32vec) (f32? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of f32vec from start to end with the value fill."))
((name . "f32vector-reverse!")
 (signature
   case-lambda
   (((f32vector? f32vec)) undefined)
   (((f32vector? f32vec) (integer? start)) undefined)
   (((f32vector? f32vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of f32vec from start to end."))
((name . "f32vector-copy!")
 (signature
   case-lambda
   (((f32vector? f32to) (integer? at) (f32vector? f32from)) undefined)
   (((f32vector? f32to) (integer? at) (f32vector? f32from) (integer? start)) undefined)
   (((f32vector? f32to)
     (integer? at)
     (f32vector? f32from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of f32from from start to end onto f32to, starting at index at."))
((name . "f32vector-reverse-copy!")
 (signature
   case-lambda
   (((f32vector? f32to) (integer? at) (f32vector? f32from)) undefined)
   (((f32vector? f32to) (integer? at) (f32vector? f32from) (integer? start)) undefined)
   (((f32vector? f32to)
     (integer? at)
     (f32vector? f32from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as f32vector-copy!, but copies in reverse"))
((name . "f32vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (f32vector? f32vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like f32vector-unfold, but the elements are copied into the vector f32vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "f32vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (f32vector? f32vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as f32vector-unfold!, but initializes the f32vector from right to left."))
((name . "reverse-f32vector->list")
 (signature
   case-lambda
   (((f32vector? f32vec)) list?)
   (((f32vector? f32vec) (integer? start)) list?)
   (((f32vector? f32vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as f32vec in reverse order."))
((name . "reverse-list->f32vector")
 (signature lambda ((list? proper-list)) f32vector?)
 (tags pure)
 (desc . "Returns f32vector with same elements as list in reverse order."))
((name . "f32vector->vector")
 (signature
   case-lambda
   (((f32vector? f32vec)) vector?)
   (((f32vector? f32vec) (integer? start)) vector?)
   (((f32vector? f32vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as f32vec."))
((name . "vector->f32vector")
 (signature
   case-lambda
   (((vector? vec)) f32vector?)
   (((vector? vec) (integer? start)) f32vector?)
   (((vector? vec) (integer? start) (integer? end)) f32vector?))
 (tags pure)
 (desc . "Returns f32vector with same elements as vec."))
((name . "make-f32vector-generator")
 (signature lambda ((f32vector? f32vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? f32?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of f32vector in order. Note that the generator is finite."))
((name . "f32vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of f32vector."))
((name . "write-f32vector")
 (signature
   case-lambda
   (((f32vector f32vec)) undefined)
   (((f32vector f32vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of f32vec in the lexical syntax explained below."))
)
