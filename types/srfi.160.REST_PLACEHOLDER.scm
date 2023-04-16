((name . "@vector-unfold")
 (signature
   case-lambda
   (((procedure? f) (integer? length) seed) @vector?)
   (((procedure? f) (integer? length) seed) @vector?))
 (subsigs (f (lambda ((integer? index) state) (values @? *))))
 (tags pure)
 (desc . "Creates a vector whose length is length and iterates across each index k between 0 and length - 1, applying f at each iteration to the current index and current state, in that order, to receive two values: the element to put in the kth slot of the new vector and a new state for the next iteration. On the first call to f, the state's value is seed."))
((name . "@vector-copy")
 (signature
   case-lambda
   (((@vector? @vec)) @vector?)
   (((@vector? @vec) (integer? start)) @vector?)
   (((@vector? @vec) (integer? start) (integer? end)) @vector?))
 (tags pure)
 (desc . "Makes a copy of the portion of @vec from start to end and returns it."))
((name . "@vector-reverse-copy")
 (signature
   case-lambda
   (((@vector? @vec)) @vector?)
   (((@vector? @vec) (integer? start)) @vector?)
   (((@vector? @vec) (integer? start) (integer? end)) @vector?))
 (tags pure)
 (desc . "The same as @vector-copy, but in reverse order."))
((name . "@vector-append")
 (signature lambda ((@vector? @vec) ...) @vector?)
 (tags pure)
 (desc . "Returns a @vector containing all the elements of the @vecs in order."))
((name . "@vector-concatenate")
 (signature lambda ((list? list-of-@vectors)) @vector?)
 (subsigs
   (list-of-@vectors (list @vector?)))
 (tags pure)
 (desc . "The same as @vector-append, but takes a list of @vectors rather than multiple arguments."))
((name . "@vector-append-subvectors")
 (signature
   lambda
   ((@vector? @vec1) (integer? start1) (integer? end1) ...)
   @vector?)
 (tags pure)
 (desc . "Concatenates the result of applying @vector-copy to each triplet of @vec, start, end arguments, but may be implemented more efficiently."))
((name . "@vector-empty?")
 (signature lambda ((@vector? @vec)) boolean?)
 (tags pure)
 (desc . "Returns #t if @vec has a length of zero, and #f otherwise."))
((name . "@vector=")
 (signature lambda ((@vector? @vec) ...) boolean?)
 (tags pure)
 (desc . "Compares the @vecs for elementwise equality, using = to do the comparisons. Returns #f unless all @vectors are the same length."))
((group
   ((name . "@vector-take")
    (signature lambda ((@vector? @vec) (integer? n)) @vector?)
    (tags pure))
   ((name . "@vector-take-right")
    (signature lambda ((@vector? @vec) (integer? n)) @vector?)
    (tags pure)))
 (desc . "Returns a @vector containing the first/last n elements of @vec."))
((group
   ((name . "@vector-drop")
    (signature lambda ((@vector? @vec) (integer? n)) @vector?)
    (tags pure))
   ((name . "@vector-drop-right")
    (signature lambda ((@vector? @vec) (integer? n)) @vector?)
    (tags pure)))
 (desc . "Returns a @vector containing all except the first/last n elements of @vec."))
((name . "@vector-segment")
 (signature lambda ((@vector? @vec) (integer? n)) list?)
 (tags pure)
 (desc . "Returns a list of @vectors, each of which contains n consecutive elements of @vec. The last @vector may be shorter than n. It is an error if n is not an exact positive integer."))
((group
   ((name . "@vector-fold")
    (signature
      lambda
      ((procedure? kons) knil (@vector? @vec1) (@vector? @vec2) ...)
      *)
    (subsigs (kons (lambda (state (@? obj1) (@? obj2) ...) *)))
    (tags pure))
   ((name . "@vector-fold-right")
    (signature
      lambda
      ((procedure? kons) knil (@vector? @vec1) (@vector? @vec2) ...)
      *)
    (subsigs (kons (lambda (state (@? obj1) (@? obj2) ...) *)))
    (tags pure)))
 (desc . "When one @vector argument @vec is given, folds kons over the elements of @vec in increasing/decreasing order using knil as the initial value. The kons procedure is called with the state first and the element second, as in SRFIs 43 and 133 (heterogeneous vectors). This is the opposite order to that used in SRFI 1 (lists) and the various string SRFIs.
When multiple @vector arguments are given, kons is called with the current state value and each value from all the vectors; @vector-fold scans elements from left to right, while @vector-fold-right does from right to left. If the lengths of vectors differ, only the portion of each vector up to the length of the shortest vector is scanned. "))
((group
   ((name . "@vector-map")
    (signature
      lambda
      ((procedure? proc) (@vector? @vec1) (@vector? @vec2) ...)
      vector?)
    (subsigs (proc (lambda ((@? val1) ...) *)))
    (tags pure))
   ((name . "@vector-map!")
    (signature
      lambda
      ((procedure? proc) (@vector? @vec1) (@vector? @vec2) ...)
      undefined)
    (subsigs (proc (lambda ((@? val1) ...) *))))
   ((name . "@vector-for-each")
    (signature
      lambda
      ((procedure? proc) (@vector? @vec1) (@vector? @vec2) ...)
      undefined)
    (subsigs (proc (lambda ((@? val1) ...) undefined)))))
 (desc . "Iterate over the elements of @vec and apply f to each, returning respectively a @vector of the results, an undefined value with the results placed back in @vec, and an undefined value with no change to @vec.
If more than one vector is passed, f gets one element from each vector as arguments. If the lengths of the vectors differ, iteration stops at the end of the shortest vector. For @vector-map!, only @vec is modified even when multiple vectors are passed.
If @vector-map or @vector-map! returns more than once (i.e. because of a continuation captured by f), the values returned or stored by earlier returns may be mutated."))
((name . "@vector-count")
 (signature
   lambda
   ((procedure? pred?) (@vector? @vec1) (@vector? @vec2) ...)
   integer?)
 (subsigs (pred? (lambda ((@? val1) (@? val2) ...) *)))
 (tags pure)
 (desc . "Call pred? on each element of @vec and return the number of calls that return true.
When multiple vectors are given, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are given for each iteration, which stops at the end of the shortest vector."))
((name . "@vector-cumulate")
 (signature lambda ((procedure? f) knil (@vector? @vec)) @vector?)
 (subsigs (f (lambda (state (@? value)) *)))
 (tags pure)
 (desc . "Like @vector-fold, but returns an @vector of partial results rather than just the final result."))
((group
   ((name . "@vector-take-while")
    (signature lambda ((procedure? pred?) (@vector? @vec)) @vector?)
    (subsigs (pred? (lambda ((@? value)) boolean?)))
    (tags pure))
   ((name . "@vector-take-while-right")
    (signature lambda ((procedure? pred?) (@vector? @vec)) @vector?)
    (subsigs (pred? (lambda ((@? value)) boolean?)))
    (tags pure)))
 (desc . "Return the shortest prefix/suffix of @vec all of whose elements satisfy pred?."))
((group
   ((name . "@vector-drop-while")
    (signature lambda ((procedure? pred?) (@vector? @vec)) @vector?)
    (subsigs (pred? (lambda ((@? value)) boolean?)))
    (tags pure))
   ((name . "@vector-drop-while-right")
    (signature lambda ((procedure? pred?) (@vector? @vec)) @vector?)
    (subsigs (pred? (lambda ((@? value)) boolean?)))
    (tags pure)))
 (desc . "Drops the longest initial prefix/suffix of @vec such that all its elements satisfy pred."))
((group
   ((name . "@vector-index")
    (signature
      lambda
      ((procedure? pred?) (@vector? @vec1) (@vector? @vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((@? value1) (@? value2) ...) *)))
    (tags pure))
   ((name . "@vector-index-right")
    (signature
      lambda
      ((procedure? pred?) (@vector? @vec1) (@vector? @vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((@? value1) (@? value2) ...) *)))
    (tags pure)))
 (desc . "Return the index of the first/last element of @vec that satisfies pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, @vector-index stops iteration at the end of the shortest one. Lengths of vectors must be the same for @vector-index-right."))
((group
   ((name . "@vector-skip")
    (signature
      lambda
      ((procedure? pred?) (@vector? @vec1) (@vector? @vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((@? value1) (@? value2) ...) *)))
    (tags pure))
   ((name . "@vector-skip-right")
    (signature
      lambda
      ((procedure? pred?) (@vector? @vec1) (@vector? @vec2) ...)
      (or integer? #f))
    (subsigs (pred? (lambda ((@? value1) (@? value2) ...) *)))
    (tags pure)))
 (desc . "Returns the index of the first/last element of @vec that does not satisfy pred?.
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, @vector-skip stops iteration at the end of the shortest one. Lengths of vectors must be the same for @vector-skip-right."))
((name . "@vector-any")
 (signature lambda ((procedure? pred?) (@vector? @vec1) (@vector? @vec2) ...) *)
 (subsigs (pred? (lambda ((@? val1) (@? val2) ...) *)))
 (tags pure)
 (desc . "Returns first non-false result of applying pred? on a element from the @vec, or #f if there is no such element. If @vec is empty, returns #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector are passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "@vector-every")
 (signature lambda ((procedure? pred?) (@vector? @vec1) (@vector? @vec2) ...) *)
 (subsigs (pred? (lambda ((@? val1) (@? val2) ...) *)))
 (tags pure)
 (desc . "If all elements from @vec satisfy pred?, return the last result of pred?. If not all do, return #f. If @vec is empty, return #t
When multiple vectors are passed, pred? must take the same number of arguments as the number of vectors, and corresponding elements from each vector is passed for each iteration. If the lengths of vectors differ, it stops at the end of the shortest one."))
((name . "@vector-partition")
 (signature
   lambda
   ((procedure? pred?) (@vector? @vec))
   (values @vector? integer?))
 (subsigs (pred? (lambda ((@? value)) boolean?)))
 (tags pure)
 (desc . "Returns an @vector of the same type as @vec, but with all elements satisfying pred? in the leftmost part of the vector and the other elements in the remaining part. The order of elements is otherwise preserved. Returns two values, the new @vector and the number of elements satisfying pred?."))
((group
   ((name . "@vector-filter")
    (signature lambda ((procedure? pred?) (@vector? @vec1)) @vector?)
    (subsigs (pred? (lambda ((@? val)) boolean?)))
    (tags pure))
   ((name . "@vector-remove")
    (signature lambda ((procedure? pred?) (@vector? @vec1)) @vector?)
    (subsigs (pred? (lambda ((@? val)) boolean?)))
    (tags pure)))
 (desc . "Return an @vector containing the elements of @vec that satisfy / do not satisfy pred?."))
((name . "@vector-swap!")
 (signature lambda ((@vector? @vector) (integer? i) (integer? j)) undefined))
((name . "@vector-fill!")
 (signature
   case-lambda
   (((@vector? @vec) (@? fill)) undefined)
   (((@vector? @vec) (@? fill) (integer? start)) undefined)
   (((@vector? @vec) (@? fill) (integer? start) (integer? end)) undefined))
 (desc . "Fills the portion of @vec from start to end with the value fill."))
((name . "@vector-reverse!")
 (signature
   case-lambda
   (((@vector? @vec)) undefined)
   (((@vector? @vec) (integer? start)) undefined)
   (((@vector? @vec) (integer? start) (integer? end)) undefined))
 (desc . "Reverses the portion of @vec from start to end."))
((name . "@vector-copy!")
 (signature
   case-lambda
   (((@vector? @to) (integer? at) (@vector? @from)) undefined)
   (((@vector? @to) (integer? at) (@vector? @from) (integer? start)) undefined)
   (((@vector? @to)
     (integer? at)
     (@vector? @from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "Copies the portion of @from from start to end onto @to, starting at index at."))
((name . "@vector-reverse-copy!")
 (signature
   case-lambda
   (((@vector? @to) (integer? at) (@vector? @from)) undefined)
   (((@vector? @to) (integer? at) (@vector? @from) (integer? start)) undefined)
   (((@vector? @to)
     (integer? at)
     (@vector? @from)
     (integer? start)
     (integer? end))
    undefined))
 (desc . "The same as @vector-copy!, but copies in reverse"))
((name . "@vector-unfold!")
 (signature
   lambda
   ((procedure? f)
    (@vector? @vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "Like @vector-unfold, but the elements are copied into the vector @vec starting at element start rather than into a newly allocated vector. Terminates when end - start elements have been generated."))
((name . "@vector-unfold-right!")
 (signature
   lambda
   ((procedure? f)
    (@vector? @vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
 (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
 (desc . "The same as @vector-unfold!, but initializes the @vector from right to left."))
((name . "reverse-@vector->list")
 (signature
   case-lambda
   (((@vector? @vec)) list?)
   (((@vector? @vec) (integer? start)) list?)
   (((@vector? @vec) (integer? start) (integer? end)) list?))
 (tags pure)
 (desc . "Returns a list with same elements as @vec in reverse order."))
((name . "reverse-list->@vector")
 (signature lambda ((list? proper-list)) @vector?)
 (tags pure)
 (desc . "Returns @vector with same elements as list in reverse order."))
((name . "@vector->vector")
 (signature
   case-lambda
   (((@vector? @vec)) vector?)
   (((@vector? @vec) (integer? start)) vector?)
   (((@vector? @vec) (integer? start) (integer? end)) vector?))
 (tags pure)
 (desc . "Returns vector with same elements as @vec."))
((name . "vector->@vector")
 (signature
   case-lambda
   (((vector? vec)) @vector?)
   (((vector? vec) (integer? start)) @vector?)
   (((vector? vec) (integer? start) (integer? end)) @vector?))
 (tags pure)
 (desc . "Returns @vector with same elements as vec."))
((name . "make-@vector-generator")
 (signature lambda ((@vector? @vector)) procedure?)
 (subsigs (return (lambda () (or eof-object? @?))))
 (desc . "Returns a SRFI 121 generator that generates all the values of @vector in order. Note that the generator is finite."))
((name . "@vector-comparator") 
 (signature value comparator?)
 (desc . "Variable containing a SRFI 128 comparator whose components provide ordering and hashing of @vector."))
((name . "write-@vector")
 (signature
   case-lambda
   (((@vector @vec)) undefined)
   (((@vector @vec) (output-port? port)) undefined))
 (desc . "Prints to port (the current output port by default) a representation of @vec in the lexical syntax explained below."))
