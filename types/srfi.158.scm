(((name . "generator")
  (signature lambda (arg ...) procedure?)
  (subsigs (return (lambda () *)))
  (desc . "The simplest finite generator. Generates each of its arguments in turn. When no arguments are provided, it returns an empty generator that generates no values."))
 ((name . "circular-generator")
  (signature lambda (arg ...) procedure?)
  (subsigs (return (lambda () *)))
  (desc . "The simplest infinite generator. Generates each of its arguments in turn, then generates them again in turn, and so on forever."))
 ((name . "make-iota-generator")
  (signature
   case-lambda
   (((integer? count)) procedure?)
   (((integer? count) (real? start)) procedure?)
   (((integer? count) (real? start) (real? step)) procedure?))
  (subsigs (return (lambda () (or real? eof-object?))))
  (desc . "Creates a finite generator of a sequence of count numbers. The sequence begins with start (which defaults to 0) and increases by step (which defaults to 1). If both start and step are exact, it generates exact numbers; otherwise it generates inexact numbers. The exactness of count doesn't affect the exactness of the results."))
 ((name . "make-range-generator")
  (signature
   case-lambda
   (((real? start)) procedure?)
   (((real? start) (real? end)) procedure?)
   (((real? start) (real? end) (real? step)) procedure?))
  (subsigs (return (lambda () (or real? eof-object?))))
  (desc . "Creates a generator of a sequence of numbers. The sequence begins with start, increases by step (default 1), and continues while the number is less than end, or forever if end is omitted. If both start and step are exact, it generates exact numbers; otherwise it generates inexact numbers. The exactness of end doesn't affect the exactness of the results."))
 ((name . "make-coroutine-generator")
  (signature lambda ((procedure? proc)) procedure?)
  (subsigs
   (proc (lambda ((procedure? yield)) undefined))
   (yield (lambda (value) undefined))
   (return (lambda () *)))
  (desc . "Creates a generator from a coroutine.
The proc argument is a procedure that takes one argument, yield. When called, make-coroutine-generator immediately returns a generator g. When g is called, proc runs until it calls yield. Calling yield causes the execution of proc to be suspended, and g returns the value passed to yield.
Whether this generator is finite or infinite depends on the behavior of proc. If proc returns, it is the end of the sequence — g returns an end-of-file object from then on. The return value of proc is ignored."))
 ((name . "list->generator")
  (signature lambda ((list? lis)) procedure?)
  (subsigs (return (lambda () *)))
  (desc . "Returns generator that yields each element of the given argument. Mutating the underlying object will affect the results of the generator. "))
 ((name . "vector->generator")
  (signature
   case-lambda
   (((vector? vec)) procedure?)
   (((vector? vec) (integer? start)) procedure?)
   (((vector? vec) (integer? start) (integer? end)) procedure?))
  (subsigs (return (lambda () *)))
  (desc . "Returns generator that yields each element of the given argument. Mutating the underlying object will affect the results of the generator. "))
 ((name . "reverse-vector->generator")
  (signature
   case-lambda
   (((vector? vec)) procedure?)
   (((vector? vec) (integer? start)) procedure?)
   (((vector? vec) (integer? start) (integer? end)) procedure?))
  (subsigs (return (lambda () *)))
  (desc . "Returns generator that yields each element of the given argument. Mutating the underlying object will affect the results of the generator. "))
 ((name . "string->generator")
  (signature
   case-lambda
   (((string? str)) procedure?)
   (((string? str) (integer? start)) procedure?)
   (((string? str) (integer? start) (integer? end)) procedure?))
  (subsigs (return (lambda () (or char? eof-object?))))
  (desc . "Returns generator that yields each element of the given argument. Mutating the underlying object will affect the results of the generator. "))
 ((name . "bytevector->generator")
  (signature
   case-lambda
   (((bytevector? bytevector)) procedure?)
   (((bytevector? bytevector) (integer? start)) procedure?)
   (((bytevector? bytevector) (integer? start) (integer? end)) procedure?))
  (subsigs (return (lambda () (or integer? eof-object?))))
  (desc . "Returns generator that yields each element of the given argument. Mutating the underlying object will affect the results of the generator. "))
 ((name . "make-for-each-generator")
  (signature lambda ((procedure? for-each) obj) procedure?)
  (subsigs (for-each (lambda (element) undefined)) (return (lambda () *)))
  (desc . "A generator constructor that converts any collection obj to a generator that returns its elements using a for-each procedure appropriate for obj. This must be a procedure that when called as (for-each proc obj) calls proc on each element of obj. Examples of such procedures are for-each, string-for-each, and vector-for-each from R7RS. The value returned by for-each is ignored. The generator is finite if the collection is finite, which would typically be the case.
The collections need not be conventional ones (lists, strings, etc.) as long as for-each can invoke a procedure on everything that counts as a member."))
 ((name . "make-unfold-generator")
  (signature
   lambda
   ((procedure? stop?) (procedure? mapper) (procedure? successor) seed)
   procedure?)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) *))
   (successor (lambda (seed) *))
   (return (lambda () *)))
  (desc . "A generator constructor similar to SRFI 1's unfold.
The stop? predicate takes a seed value and determines whether to stop. The mapper procedure calculates a value to be returned by the generator from a seed value. The successor procedure calculates the next seed value from the current seed value.
For each call of the resulting generator, stop? is called with the current seed value. If it returns true, then the generator returns an end-of-file object. Otherwise, it applies mapper to the current seed value to get the value to return, and uses successor to update the seed value.
This generator is finite unless stop? never returns true."))
 ((name . "gcons*")
  (signature lambda (item ... (procedure? gen)) procedure?)
  (subsigs (gen (lambda () *)) (return (lambda () *)))
  (desc . "Returns a generator that adds items in front of gen. Once the items have been consumed, the generator is guaranteed to tail-call gen."))
 ((name . "gappend")
  (signature lambda ((procedure? gen) ...) procedure?)
  (subsigs 
    (gen (lambda () *))
    (return (lambda () *)))
  (desc . "Returns a generator that yields the items from the first given generator, and once it is exhausted, from the second generator, and so on."))
 ((name . "gflatten")
  (signature lambda ((procedure? gen) ...) procedure?)
  (subsigs 
    (gen (lambda () list?))
    (return (lambda () *)))
  (desc . "Returns a generator that yields the elements of the lists produced by the given generator."))
 ((name . "ggroup")
  (signature case-lambda
             (((procedure? gen) (integer? k)) procedure?)
             (((procedure? gen) (integer? k) padding) procedure?))
  (subsigs 
    (gen (lambda () *))
    (return (lambda () (or list? eof-object?))))
  (desc . "Returns a generator that yields lists of k items from the given generator. If fewer than k elements are available for the last list, and padding is absent, the short list is returned; otherwise, it is padded by padding to length k."))
 ((name . "gmerge")
  (signature lambda ((procedure? less-than) (procedure? gen1) ...) procedure?)
  (subsigs
    (less-than (lambda (a b) boolean?))
    (gen (lambda () *)))
  (desc . "Returns a generator that yields the items from the given generators in the order dictated by less-than. If the items are equal, the leftmost item is used first. When all of given generators are exhausted, the returned generator is exhausted also.
As a special case, if only one generator is given, it is returned."))
 ((name . "gmap")
  (signature lambda ((procedure? proc) (procedure? gen1) ...) procedure?)
  (subsigs
    (proc (lambda (el) *))
    (gen (lambda () *)))
  (desc . "When only one generator is given, returns a generator that yields the items from the given generator after invoking proc on them.
When more than one generator is given, each item of the resulting generator is a result of applying proc to the items from each generator. If any of input generator is exhausted, the resulting generator is also exhausted.
Note: This differs from generator-map->list, which consumes all values at once and returns the results as a list, while gmap returns a generator immediately without consuming input."))
 ((name . "gcombine")
  (signature
   lambda
   ((procedure? proc) seed (procedure? gen1) (procedure? gen2) ...)
   procedure?)
  (subsigs
   (proc (lambda (value1 value2 ... seed) (values * *)))
   (gen (lambda () *))
   (return (lambda () *)))
  (desc . "A generator for mapping with state. It yields a sequence of sub-folds over proc.
The proc argument is a procedure that takes as many arguments as the input generators plus one. It is called as (proc v1 v2 … seed), where v1, v2, … are the values yielded from the input generators, and seed is the current seed value. It must return two values, the yielding value and the next seed. The result generator is exhausted when any of the genn generators is exhausted, at which time all the others are in an undefined state."))
 ((name . "gfilter")
  (signature lambda ((procedure? pred) (procedure? gen)) procedure?)
  (subsigs
   (pred (lambda (element) boolean?))
   (gen (lambda () *))
   (return (lambda () *)))
  (desc . "Returns generators that yield the items from the source generator, except those on which pred answers false."))
 ((name . "gremove")
  (signature lambda ((procedure? pred) (procedure? gen)) procedure?)
  (subsigs
   (pred (lambda (element) boolean?))
   (gen (lambda () *))
   (return (lambda () *)))
  (desc . "Returns generators that yield the items from the source generator, except those on which pred answers true."))
 ((name . "gstate-filter")
  (signature lambda ((procedure? proc) seed (procedure? gen)) procedure?)
  (subsigs
    (proc (lambda (item state) (values boolean? *)))
    (gen (lambda () *))
    (return (lambda () *)))
  (desc . "Returns a generator that obtains items from the source generator and passes an item and a state (whose initial value is seed) as arguments to proc. Proc in turn returns two values, a boolean and a new value of the state. If the boolean is true, the item is returned; otherwise, this algorithm is repeated until gen is exhausted, at which point the returned generator is also exhausted. The final value of the state is discarded."))
 ((name . "gtake")
  (signature
   case-lambda
   (((procedure? gen) (integer? k)) procedure?)
   (((procedure? gen) (integer? k) padding) procedure?))
  (subsigs (gen (lambda () *)) (return (lambda () *)))
  (desc . "Gtake returns a generator that yields (at most) the first k items of the source generator.
The procedure won't complain if the source generator is exhausted before generating k items. By default, the generator returned by gtake terminates when the source generator does, but if you provide the padding argument, then the returned generator will yield exactly k items, using the padding value as needed to provide sufficient additional values."))
 ((name . "gdrop")
  (signature lambda ((procedure? gen) (integer? k)) procedure?)
  (subsigs (gen (lambda () *)) (return (lambda () *)))
  (desc . "Gdrop returns a generator that skips the first k items of the source generator. The procedure won't complain if the source generator is exhausted before generating k items."))
 ((name . "gtake-while")
  (signature lambda ((procedure? pred) (procedure? gen)) procedure?)
  (subsigs
   (pred (lambda (element) boolean?))
   (gen (lambda () *))
   (return (lambda () *)))
  (desc . "The generator returned from gtake-while yields items from the source generator as long as pred returns true for each."))
 ((name . "gdrop-while")
  (signature lambda ((procedure? pred) (procedure? gen)) procedure?)
  (subsigs
   (pred (lambda (element) boolean?))
   (gen (lambda () *))
   (return (lambda () *)))
  (desc . "The generator returned from gdrop-while first reads and discards values from the source generator while pred returns true for them, then starts yielding items returned by the source."))
 ((name . "gdelete")
  (signature
   case-lambda
   ((item (procedure? gen)) procedure?)
   ((item (procedure? gen) (procedure? =)) procedure?))
  (subsigs
   (gen (lambda () *))
   (= (lambda (a b) boolean?))
   (return (lambda () *)))
  (desc . "Creates a generator that returns whatever gen returns, except for any items that are the same as item in the sense of =, which defaults to equal?. The = predicate is passed exactly two arguments, of which the first is item and the second is an element generated by gen."))
 ((name . "gdelete-neighbor-dups")
  (signature
   case-lambda
   (((procedure? gen)) procedure?)
   (((procedure? gen) (procedure? =)) procedure?))
  (subsigs
   (gen (lambda () *))
   (= (lambda (a b) boolean?))
   (return (lambda () *)))
  (desc . "Creates a generator that returns whatever gen returns, except for any items that are equal to the preceding item in the sense of =, which defaults to equal?. The = predicate is passed exactly two arguments, of which the first was generated by gen before the second."))
 ((name . "gindex")
  (signature lambda ((procedure? value-gen) (procedure? index-gen)) procedure?)
  (subsigs
   (value-gen (lambda () *))
   (index-gen (lambda () (or integer? eof-object?)))
   (return (lambda () *)))
  (desc . "Creates a generator that returns elements of value-gen specified by the indices (non-negative exact integers) generated by index-gen. It is an error if the indices are not strictly increasing. The result generator is exhausted when either generator is exhausted, at which time the other is in an undefined state."))
 ((name . "gselect")
  (signature lambda ((procedure? value-gen) (procedure? truth-gen)) procedure?)
  (subsigs
   (value-gen (lambda () *))
   (truth-gen (lambda () (or boolean? eof-object?)))
   (return (lambda () *)))
  (desc . "Creates a generator that returns elements of value-gen that correspond to the values generated by truth-gen. If the current value of truth-gen is true, the current value of value-gen is generated, but otherwise not. The result generator is exhausted when either generator is exhausted, at which time the other is in an undefined state."))
 ((name . "generator->list")
  (signature
   case-lambda
   (((procedure? generator)) list?)
   (((procedure? generator) (integer? k)) list?))
  (subsigs (generator (lambda () *)))
  (desc . "Reads items from generator and returns a newly allocated list of them. By default, it reads until the generator is exhausted.
If an optional argument k is given, it must be a non-negative integer, and the list ends when either k items are consumed, or generator is exhausted; therefore generator can be infinite in this case."))
 ((name . "generator->reverse-list")
  (signature
   case-lambda
   (((procedure? generator)) list?)
   (((procedure? generator) (integer? k)) list?))
  (subsigs (generator (lambda () *)))
  (desc . "Reads items from generator and returns a newly allocated list of them in reverse order. By default, this reads until the generator is exhausted.
If an optional argument k is given, it must be a non-negative integer, and the list ends when either k items are read, or generator is exhausted; therefore generator can be infinite in this case. "))
 ((name . "generator->vector")
  (signature
   case-lambda
   (((procedure? generator)) vector?)
   (((procedure? generator) (integer? k)) vector?))
  (subsigs (generator (lambda () *)))
  (desc . "Reads items from generator and returns a newly allocated vector of them. By default, it reads until the generator is exhausted.
If an optional argument k is given, it must be a non-negative integer, and the list ends when either k items are consumed, or generator is exhausted; therefore generator can be infinite in this case."))
 ((name . "generator->vector!")
  (signature
   lambda
   ((vector? vector) (integer? at) (procedure? generator))
   integer?)
  (subsigs (generator (lambda () *)))
  (desc . "Reads items from generator and puts them into vector starting at index at, until vector is full or generator is exhausted. Generator can be infinite. The number of elements generated is returned."))
 ((name . "generator->string")
  (signature
   case-lambda
   (((procedure? generator)) string?)
   (((procedure? generator) (integer? k)) string?))
  (subsigs (generator (lambda () (or char? eof-object?))))
  (desc . "Reads items from generator and returns a newly allocated string of them. It is an error if the items are not characters. By default, it reads until the generator is exhausted.
If an optional argument k is given, it must be a non-negative integer, and the string ends when either k items are consumed, or generator is exhausted; therefore generator can be infinite in this case."))
 ((name . "generator-fold")
  (signature
   lambda
   ((procedure? proc) seed (procedure? gen1) (procedure? gen2) ...)
   procedure?)
  (subsigs (proc (lambda (val1 val2 ... state) *)) (gen (lambda () *)))
  (desc . "Works like SRFI 1 fold on the values generated by the generator arguments.
When one generator is given, for each value v generated by gen, proc is called as (proc v r), where r is the current accumulated result; the initial value of the accumulated result is seed, and the return value from proc becomes the next accumulated result. When gen is exhausted, the accumulated result at that time is returned from generator-fold.
When more than one generator is given, proc is invoked on the values returned by all the generator arguments followed by the current accumulated result. The procedure terminates when any of the genn generators is exhausted, at which time all the others are in an undefined state."))
 ((name . "generator-for-each")
  (signature
   lambda
   ((procedure? proc) (procedure? gen1) (procedure? gen2) ...)
   undefined)
  (subsigs (proc (lambda (val1 val2 ...) undefined)) (gen (lambda () *)))
  (desc . "A generator analogue of for-each that consumes generated values using side effects. Repeatedly applies proc on the values yielded by gen, gen2 … until any one of the generators is exhausted, at which time all the others are in an undefined state. The values returned from proc are discarded. Returns an unspecified value."))
 ((name . "generator-map->list")
  (signature lambda ((procedure? proc) (procedure? gen1) (procedure? gen2) ...) list?)
  (subsigs 
    (proc (lambda (element) *))
    (generator (lambda () *)))
  (desc . "A generator analogue of map that consumes generated values, processes them through a mapping function, and returns a list of the mapped values. Repeatedly applies proc on the values yielded by gen, gen2 … until any one of the generators is exhausted, at which time all the others are in an undefined state. The values returned from proc are accumulated into a list, which is returned."))
 ((name . "generator-find")
  (signature lambda ((procedure? pred) (procedure? generator)) *)
  (subsigs (pred (lambda (element) boolean?)) (generator (lambda () *)))
  (desc . "Applies pred to each item from gen. As soon as it yields a true value, the item is returned without consuming the rest of gen. If gen is exhausted, returns #f."))
 ((name . "generator-count")
  (signature lambda ((procedure? pred) (procedure? generator)) integer?)
  (subsigs (pred (lambda (element) boolean?)) (generator (lambda () *)))
  (desc . "Returns the number of items available from the generator gen that satisfy the predicate pred."))
 ((name . "generator-any")
  (signature lambda ((procedure? pred) (procedure? generator)) *)
  (subsigs (pred (lambda (element) boolean?)) (generator (lambda () *)))
  (desc . "Applies pred to each item from gen. As soon as it yields a true value, the value is returned without consuming the rest of gen. If gen is exhausted, returns #f."))
 ((name . "generator-every")
  (signature lambda ((procedure? pred) (procedure? generator)) *)
  (subsigs (pred (lambda (element) boolean?)) (generator (lambda () *)))
  (desc . "Applies pred to each item from gen. As soon as it yields a false value, the value is returned without consuming the rest of gen. If gen is exhausted, returns the last value returned by pred, or #t if pred was never called."))
 ((name . "generator-unfold")
  (signature lambda ((procedure? gen) (procedure? unfold) arg ...) *)
  (subsigs
   (gen (lambda () *))
   (unfold
    (lambda ((procedure? stop?)
             (procedure? mapper)
             (procedure? successor)
             seed
             args
             ...)
      *))
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) *))
   (successor (lambda (seed) *)))
  (desc . "Equivalent to (unfold eof-object? (lambda (x) x) (lambda (x) (gen)) (gen) arg ...). The values of gen are unfolded into the collection that unfold creates.
The signature of the unfold procedure is (unfold stop? mapper successor seed args ...). Note that the vector-unfold and vector-unfold-right of SRFI 43 and SRFI 133 do not have this signature and cannot be used with this procedure. To unfold into a vector, use SRFI 1's unfold and then apply list->vector to the result."))
 ((name . "make-accumulator")
  (signature lambda ((procedure? kons) knil (procedure? finalizer)) procedure?)
  (subsigs
    (kons (lambda (element state) *))
    (finalizer (lambda (state) *))
    (return (lambda (el) *)))
  (desc . "Returns an accumulator that, when invoked on an object other than an end-of-file object, invokes kons on its argument and the accumulator's current state, using the same order as a function passed to fold. It then sets the accumulator's state to the value returned by kons and returns an unspecified value. The initial state of the accumulator is set to knil. However, if an end-of-file object is passed to the accumulator, it returns the result of tail-calling the procedure finalizer on the state. Repeated calls with an end-of-file object will reinvoke finalizer. "))
 ((name . "count-accumulator")
  (signature lambda () procedure?)
  (subsigs
    (return (lambda (el) (or undefined integer?))))
  (desc . "Returns an accumulator that, when invoked on an object, adds 1 to a count inside the accumulator and returns an unspecified value. However, if an end-of-file object is passed, the accumulator returns the count. "))
 ((name . "list-accumulator")
  (signature lambda () procedure?)
  (subsigs
    (return (lambda (el) (or undefined list?))))
  (desc . "Returns an accumulator that, when invoked on an object, adds that object to a list inside the accumulator in reverse order of accumulation and returns an unspecified value. However, if an end-of-file object is passed, the accumulator returns the list."))
 ((name . "vector-accumulator")
  (signature lambda () procedure?)
  (subsigs
    (return (lambda (el) (or undefined vector?))))
  (desc . "Returns an accumulator that, when invoked on an object, adds that object to a vector inside the accumulator in order of accumulation and returns an unspecified value. However, if an end-of-file object is passed, the accumulator returns the vector."))
 ((name . "reverse-vector-accumulator")
  (signature lambda () procedure?)
  (subsigs
    (return (lambda (el) (or undefined vector?))))
  (desc . "Returns an accumulator that, when invoked on an object, adds that object to a vector inside the accumulator in reverse order of accumulation and returns an unspecified value. However, if an end-of-file object is passed, the accumulator returns the vector."))
 ((name . "vector-accumulator!")
  (signature lambda ((vector? vector) (integer? at)) procedure?)
  (subsigs
    (return (lambda (el) (or undefined vector?))))
  (desc . "Returns an accumulator that, when invoked on an object, adds that object to consecutive positions of vector starting at at in order of accumulation. It is an error to try to accumulate more objects than vector will hold. An unspecified value is returned. However, if an end-of-file object is passed, the accumulator returns vector."))
 ((name . "string-accumulator")
  (signature lambda () procedure?)
  (subsigs
    (return (lambda (((or char? eof-object?) char)) (or undefined string?))))
  (desc . "Returns an accumulator that, when invoked on a character, adds that character to a string inside the accumulator in order of accumulation and returns an unspecified value. However, if an end-of-file object is passed, the accumulator returns the string."))
 ((name . "bytevector-accumulator")
  (signature lambda () procedure?)
  (subsigs
    (return (lambda (((or integer? eof-object?) byte)) (or undefined bytevector?))))
  (desc . "Returns an accumulator that, when invoked on a byte, adds that integer to a bytevector inside the accumulator in order of accumulation and returns an unspecified value. However, if an end-of-file object is passed, the accumulator returns the bytevector."))
 ((name . "bytevector-accumulator!")
  (signature lambda ((bytevector? bytevector) (integer? at)) procedure?)
  (subsigs
    (return (lambda (((or integer? eof-object?) byte)) (or undefined bytevector?))))
  (desc . "Returns an accumulator that, when invoked on a byte, adds that byte to consecutive positions of bytevector starting at at in order of accumulation. It is an error to try to accumulate more bytes than vector will hold. An unspecified value is returned. However, if an end-of-file object is passed, the accumulator returns bytevector."))
 ((name . "sum-accumulator")
  (signature lambda () procedure?)
  (subsigs
    (return (lambda (((or number? eof-object?) char)) (or undefined number?))))
  (desc . "Returns an accumulator that, when invoked on a number, adds that number to a sum inside the accumulator in order of accumulation and returns an unspecified value. However, if an end-of-file object is passed, the accumulator returns the sum."))
 ((name . "product-accumulator")
  (signature lambda () procedure?)
  (subsigs
    (return (lambda (((or number? eof-object?) char)) (or undefined number?))))
  (desc . "Returns an accumulator that, when invoked on a number, multiplies that number to a product inside the accumulator in order of accumulation and returns an unspecified value. However, if an end-of-file object is passed, the accumulator returns the product.")))
