(((name . "stream-null")
  (signature value stream-null?)
  (desc . "Stream-null is a promise that, when forced, is a single object, distinguishable from all other objects, that represents the null stream. Stream-null is immutable and unique."))
 ((name . "stream-cons")
  (signature syntax-rules () ((_ obj stream) stream-pair?))
  (subsigs (stream (value stream?)))
  (tags pure)
  (desc . "Stream-cons is a macro that accepts an object and a stream and creates a newly-allocated stream containing a promise that, when forced, is a stream-pair with the object in its stream-car and the stream in its stream-cdr. Stream-cons must be syntactic, not procedural, because neither object nor stream is evaluated when stream-cons is called. Since stream is not evaluated, when the stream-pair is created, it is not an error to call stream-cons with a stream that is not of type stream; however, doing so will cause an error later when the stream-cdr of the stream-pair is accessed. Once created, a stream-pair is immutable; there is no stream-set-car! or stream-set-cdr! that modifies an existing stream-pair. There is no dotted-pair or improper stream as with lists."))
 ((name . "stream?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Stream? is a procedure that takes an object and returns #t if the object is a stream and #f otherwise. If object is a stream, stream? does not force its promise. If (stream? obj) is #t, then one of (stream-null? obj) and (stream-pair? obj) will be #t and the other will be #f; if (stream? obj) is #f, both (stream-null? obj) and (stream-pair? obj) will be #f."))
 ((name . "stream-null?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Stream-null? is a procedure that takes an object and returns #t if the object is the distinguished null stream and #f otherwise. If object is a stream, stream-null? must force its promise in order to distinguish stream-null from stream-pair."))
 ((name . "stream-pair?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Stream-pair? is a procedure that takes an object and returns #t if the object is a stream-pair constructed by stream-cons and #f otherwise. If object is a stream, stream-pair? must force its promise in order to distinguish stream-null from stream-pair."))
 ((name . "stream-car")
  (signature lambda ((stream-pair? stream)) *)
  (tags pure)
  (desc . "Stream-car is a procedure that takes a stream and returns the object stored in the stream-car of the stream. Stream-car signals an error if the object passed to it is not a stream-pair. Calling stream-car causes the object stored there to be evaluated if it has not yet been; the object’s value is cached in case it is needed again."))
 ((name . "stream-cdr")
  (signature lambda ((stream-pair? stream)) stream?)
  (tags pure)
  (desc . "Stream-car is a procedure that takes a stream and returns the object stored in the stream-car of the stream. Stream-car signals an error if the object passed to it is not a stream-pair. Calling stream-car causes the object stored there to be evaluated if it has not yet been; the object’s value is cached in case it is needed again."))
 ((name . "stream-lambda")
  (signature syntax-rules () ((_ formals body) procedure?))
  (subsigs
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))
  (desc . "Stream-lambda creates a procedure that returns a promise to evaluate the body of the procedure. The last body expression to be evaluated must yield a stream. As with normal lambda, args may be a single variable name, in which case all the formal arguments are collected into a single list, or a list of variable names, which may be null if there are no arguments, proper if there are an exact number of arguments, or dotted if a fixed number of arguments is to be followed by zero or more arguments collected into a list. Body must contain at least one expression, and may contain internal definitions preceding any expressions to be evaluated."))
 ((name . "define-stream")
  (signature
   syntax-rules
   ()
   ((_ (variable parameter1 ...) body))
   ((_ (variable parameter1 ... . parameter) body)))
  (desc . "Define-stream creates a procedure that returns a stream, and may appear anywhere a normal define may appear, including as an internal definition, and may have internal definitions of its own, including other define-streams. The defined procedure takes arguments in the same way as stream-lambda. Define-stream is syntactic sugar on stream-lambda; see also stream-let, which is also a sugaring of stream-lambda."))
 ((name . "list->stream")
  (signature lambda ((list? list-of-objects)) stream?)
  (tags pure)
  (desc . "[α] → {α}
List->stream takes a list of objects and returns a newly-allocated stream containing in its elements the objects in the list. Since the objects are given in a list, they are evaluated when list->stream is called, before the stream is created. If the list of objects is null, as in (list->stream '()), the null stream is returned. See also stream."))
 ((name . "port->stream")
  (signature case-lambda (() stream?) (((input-port? port)) stream?))
  (desc . "port → {char}
Port->stream takes a port and returns a newly-allocated stream containing in its elements the characters on the port. If port is not given it defaults to the current input port. The returned stream has finite length and is terminated by stream-null."))
 ((name . "stream")
  (signature syntax-rules () ((_ object ...) stream?))
  (tags pure)
  (desc . "Stream is syntax that takes zero or more objects and creates a newly-allocated stream containing in its elements the objects, in order. Since stream is syntactic, the objects are evaluated when they are accessed, not when the stream is created. If no objects are given, as in (stream), the null stream is returned. See also list->stream."))
 ((name . "stream->list")
  (signature
   case-lambda
   (((stream? stream)) list?)
   (((integer? n) (stream? stream)) list?))
  (tags pure)
  (desc . "nat × {α} → [α]
Stream->list takes a natural number n and a stream and returns a newly-allocated list containing in its elements the first n items in the stream. If the stream has less than n items all the items in the stream will be included in the returned list. If n is not given it defaults to infinity, which means that unless stream is finite stream->list will never return."))
 ((name . "stream-append")
  (signature lambda ((stream? stream) ...) stream?)
  (tags pure)
  (desc . "{α} ... → {α}
Stream-append returns a newly-allocated stream containing in its elements those elements contained in its input streams, in order of input. If any of the input streams is infinite, no elements of any of the succeeding input streams will appear in the output stream; thus, if x is infinite, (stream-append x y) ≡ x. See also stream-concat."))
 ((name . "stream-concat")
  (signature lambda ((stream? stream-of-streams)) stream?)
  (tags pure)
  (desc . "{{α}} ... → {α}
Stream-concat takes a stream consisting of one or more streams and returns a newly-allocated stream containing all the elements of the input streams. If any of the streams in the input stream is infinite, any remaining streams in the input stream will never appear in the output stream. See also stream-append."))
 ((name . "stream-constant")
  (signature lambda (object ...) stream?)
  (tags pure)
  (desc . "α ... → {α}
Stream-constant takes one or more objects and returns a newly-allocated stream containing in its elements the objects, repeating the objects in succession forever."))
 ((name . "stream-drop")
  (signature lambda ((integer? n) (stream? stream)) stream?)
  (tags pure)
  (desc . "nat × {α} → {α}
Stream-drop returns the suffix of the input stream that starts at the next element after the first n elements. The output stream shares structure with the input stream; thus, promises forced in one instance of the stream are also forced in the other instance of the stream. If the input stream has less than n elements, stream-drop returns the null stream. See also stream-take."))
 ((name . "stream-drop-while")
  (signature lambda ((procedure? pred?) (stream? stream)) stream?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure)
  (desc . "(α → boolean) × {α} → {α}
Stream-drop-while returns the suffix of the input stream that starts at the first element x for which (pred? x) is #f. The output stream shares structure with the input stream. See also stream-take-while."))
 ((name . "stream-filter")
  (signature lambda ((procedure? pred?) (stream? stream)) stream?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure)
  (desc . "(α → boolean) × {α} → {α}
Stream-filter returns a newly-allocated stream that contains only those elements x of the input stream for which (pred? x) is non-#f."))
 ((name . "stream-fold")
  (signature lambda ((procedure? proc) base (stream? stream)) *)
  (subsigs (proc (lambda (base element) *)))
  (tags pure)
  (desc . "(α × β → α) × α × {β} → α
Stream-fold applies a binary procedure to base and the first element of stream to compute a new base, then applies the procedure to the new base and the next element of stream to compute a succeeding base, and so on, accumulating a value that is finally returned as the value of stream-fold when the end of the stream is reached. Stream must be finite, or stream-fold will enter an infinite loop. See also stream-scan, which is similar to stream-fold, but useful for infinite streams. For readers familiar with other functional languages, this is a left-fold; there is no corresponding right-fold, since right-fold relies on finite streams that are fully-evaluated, at which time they may as well be converted to a list."))
 ((name . "stream-for-each")
  (signature lambda ((procedure? proc) (stream? stream1) ...) undefined)
  (subsigs (proc (lambda (element1 ...) undefined)))
  (desc . "(α × β × ...) × {α} × {β} ...
Stream-for-each applies a procedure element-wise to corresponding elements of the input streams for its side-effects; it returns nothing. Stream-for-each stops as soon as any of its input streams is exhausted."))
 ((name . "stream-from")
  (signature
   case-lambda
   (((number? from)) stream?)
   (((number? from) (number? step)) stream?))
  (tags pure)
  (desc . "number × number → {number}
Stream-from creates a newly-allocated stream that contains first as its first element and increments each succeeding element by step. If step is not given it defaults to 1. First and step may be of any numeric type. Stream-from is frequently useful as a generator in stream-of expressions. See also stream-range for a similar procedure that creates finite streams."))
 ((name . "stream-iterate")
  (signature lambda ((procedure? proc) base) stream?)
  (subsigs (proc (lambda (element) *)))
  (tags pure)
  (desc . "(α → α) × α → {α}
Stream-iterate creates a newly-allocated stream containing base in its first element and applies proc to each element in turn to determine the succeeding element. See also stream-unfold and stream-unfolds."))
 ((name . "stream-length")
  (signature lambda ((stream? stream)) integer?)
  (tags pure)
  (desc . "{α} → nat
Stream-length takes an input stream and returns the number of elements in the stream; it does not evaluate its elements. Stream-length may only be used on finite streams; it enters an infinite loop with infinite streams."))
 ((name . "stream-let")
  (signature syntax-rules () ((_ tag ((var expr) ...) body)))
  (desc . "Stream-let creates a local scope that binds each variable to the value of its corresponding expression. It additionally binds tag to a procedure which takes the bound variables as arguments and body as its defining expressions, binding the tag with stream-lambda. Tag is in scope within body, and may be called recursively. When the expanded expression defined by the stream-let is evaluated, stream-let evaluates the expressions in its body in an environment containing the newly-bound variables, returning the value of the last expression evaluated, which must yield a stream.
Stream-let provides syntactic sugar on stream-lambda, in the same manner as normal let provides syntactic sugar on normal lambda. However, unlike normal let, the tag is required, not optional, because unnamed stream-let is meaningless."))
 ((name . "stream-map")
  (signature lambda ((procedure? proc) (stream? stream1) ...) stream?)
  (subsigs (proc (lambda (element1 ...) *)))
  (desc . "(α × β ... → ω) × {α} × {β} ... → {ω}
Stream-map applies a procedure element-wise to corresponding elements of the input streams, returning a newly-allocated stream containing elements that are the results of those procedure applications. The output stream has as many elements as the minimum-length input stream, and may be infinite."))
 ((name . "stream-match")
  (signature syntax-rules (_) ((_ stream clause ...)))
  (subsigs
   (clause (pattern () (pat0 pat1 ...) (pat0 pat1 ... . pat_rest) pat))
   (pat (pattern identifier _))
   (stream (value stream?)))
  (desc . "Stream-match provides the syntax of pattern-matching for streams. The input stream is an expression that evaluates to a stream. Clauses are of the form (pattern [fender] expr), consisting of a pattern that matches a stream of a particular shape, an optional fender that must succeed if the pattern is to match, and an expression that is evaluated if the pattern matches. There are four types of patterns:
    * () — Matches the null stream.
    * (pat0 pat1 ...) — Matches a finite stream with length exactly equal to the number of pattern elements.
    * (pat0 pat1 ... . patrest) — Matches an infinite stream, or a finite stream with length at least as great as the number of pattern elements before the literal dot.
    * pat — Matches an entire stream. Should always appear last in the list of clauses; it’s not an error to appear elsewhere, but subsequent clauses could never match.

Each pattern element pati may be either:
    * An identifier — Matches any stream element. Additionally, the value of the stream element is bound to the variable named by the identifier, which is in scope in the fender and expression of the corresponding clause. Each identifier in a single pattern must be unique.
    * A literal underscore — Matches any stream element, but creates no bindings.

The patterns are tested in order, left-to-right, until a matching pattern is found; if fender is present, it must evaluate as non-#f for the match to be successful. Pattern variables are bound in the corresponding fender and expression. Once the matching pattern is found, the corresponding expression is evaluated and returned as the result of the match. An error is signaled if no pattern matches the input stream."))
 ((name . "stream-of")
  (signature syntax-rules (in is) ((_ expr clause ...) stream?))
  (subsigs (clause (pattern (var in stream-expr) (var is expr) (pred? expr))))
  (desc . "Stream-of provides the syntax of stream comprehensions, which generate streams by means of looping expressions. The result is a stream of objects of the type returned by expr. There are four types of clauses:
    * (var in stream-expr) — Loop over the elements of stream-expr, in order from the start of the stream, binding each element of the stream in turn to var. Stream-from and stream-range are frequently useful as generators for stream-expr.
    * (var is expr) — Bind var to the value obtained by evaluating expr.
    * (pred? expr) — Include in the output stream only those elements x for which (pred? x) is non-#f.

The scope of variables bound in the stream comprehension is the clauses to the right of the binding clause (but not the binding clause itself) plus the result expression.
When two or more generators are present, the loops are processed as if they are nested from left to right; that is, the rightmost generator varies fastest. A consequence of this is that only the first generator may be infinite and all subsequent generators must be finite. If no generators are present, the result of a stream comprehension is a stream containing the result expression; thus, (stream-of 1) produces a finite stream containing only the element 1."))
 ((name . "stream-range")
  (signature
   case-lambda
   (((real? first) (real? past)) stream?)
   (((real? first) (real? past) (real? step)) stream?))
  (tags pure)
  (desc . "number × number × number → {number}
Stream-range creates a newly-allocated stream that contains first as its first element and increments each succeeding element by step. The stream is finite and ends before past, which is not an element of the stream. If step is not given it defaults to 1 if first is less than past and -1 otherwise. First, past and step may be of any numeric type. Stream-range is frequently useful as a generator in stream-of expressions. See also stream-from for a similar procedure that creates infinite streams.
Successive elements of the stream are calculated by adding step to first, so if any of first, past or step are inexact, the length of the output stream may differ from (ceiling (- (/ (- past first) step) 1)."))
 ((name . "stream-ref")
  (signature lambda ((stream? stream) (integer? n)) *)
  (tags pure)
  (desc . "{α} × nat → α
Stream-ref returns the nth element of stream, counting from zero. An error is signaled if n is greater than or equal to the length of stream."))
 ((name . "stream-reverse")
  (signature lambda ((stream? stream)) stream?)
  (tags pure)
  (desc . "{α} → {α}
Stream-reverse returns a newly-allocated stream containing the elements of the input stream but in reverse order. Stream-reverse may only be used with finite streams; it enters an infinite loop with infinite streams. Stream-reverse does not force evaluation of the elements of the stream."))
 ((name . "stream-scan")
  (signature lambda ((procedure? proc) base (stream? stream)) stream?)
  (subsigs (proc (lambda (base element) *)))
  (tags pure)
  (desc . "(α × β → α) × α × {β} → {α}
Stream-scan accumulates the partial folds of an input stream into a newly-allocated output stream. The output stream is the base followed by (stream-fold proc base (stream-take i stream)) for each of the first i elements of stream."))
 ((name . "stream-take")
  (signature lambda ((integer? n) (stream? stream)) stream?)
  (tags pure)
  (desc . "nat × {α} → {α}
Stream-take takes a non-negative integer n and a stream and returns a newly-allocated stream containing the first n elements of the input stream. If the input stream has less than n elements, so does the output stream. See also stream-drop."))
 ((name . "stream-take-while")
  (signature lambda ((procedure? pred?) (stream? stream)) stream?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure)
  (desc . "(α → boolean) × {α} → {α}
Stream-take-while takes a predicate and a stream and returns a newly-allocated stream containing those elements x that form the maximal prefix of the input stream for which (pred? x) is non-#f. See also stream-drop-while."))
 ((name . "stream-unfold")
  (signature
   lambda
   ((procedure? map) (procedure? pred?) (procedure? gen) base)
   stream?)
  (subsigs
   (map (lambda (base) *))
   (pred? (lambda (base) boolean?))
   (gen (lambda (base) *)))
  (tags pure)
  (desc . "(α → β) × (α → boolean) × (α → α) × α → {β}
Stream-unfold is the fundamental recursive stream constructor. It constructs a stream by repeatedly applying gen to successive values of base, in the manner of stream-iterate, then applying map to each of the values so generated, appending each of the mapped values to the output stream as long as (pred? base) is non-#f. See also stream-iterate and stream-unfolds."))
 ((name . "stream-unfolds")
  (signature lambda ((procedure? proc) seed) (values stream? ...))
  (subsigs (proc (lambda (seed) (values (or list? #f) * ...))))
  (tags pure)
  (desc . "(α → (values α × β ...)) × α → (values {β} ...)
Stream-unfolds returns n newly-allocated streams containing those elements produced by successive calls to the generator proc, which takes the current seed as its argument and returns n+1 values
(proc seed → seed result0 ... resultn-1

where the returned seed is the input seed to the next call to the generator and resulti indicates how to produce the next element of the ith result stream:
    * (value) — value is the next car of the result stream
    * #f — no value produced by this iteration of the generator proc for the result stream
    * () — the end of the result stream

It may require multiple calls of proc to produce the next element of any particular result stream. See also stream-iterate and stream-unfold."))
 ((name . "stream-zip")
  (signature lambda ((stream? stream) ...) stream?)
  (tags pure)
  (desc . "{α} × {β} × ... → {[α β ...]}
Stream-zip takes one or more input streams and returns a newly-allocated stream in which each element is a list (not a stream) of the corresponding elements of the input streams. The output stream is as long as the shortest input stream, if any of the input streams is finite, or is infinite if all the input streams are infinite.
A common use of stream-zip is to add an index to a stream, as in (stream-finds eql? obj strm), which returns all the zero-based indices in strm at which obj appears; (stream-find eql? obj strm) returns the first such index, or #f if obj is not in strm.")))
