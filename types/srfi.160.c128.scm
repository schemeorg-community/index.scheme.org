(((name . "make-c128vector")
  (signature
   case-lambda
   (((integer? size)) c128vector?)
   (((integer? size) (c128? fill)) c128vector?))
  (tags pure))
 ((name . "c128vector")
  (signature lambda ((c128? value) ...) c128vector?)
  (tags pure))
 ((name . "c128?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes number?))
 ((name . "c128vector?")
  (signature lambda (obj) boolean?)
  (tags pure predicate))
 ((name . "c128vector-ref")
  (signature lambda ((c128vector? vec) (integer? i)) c128?)
  (tags pure))
 ((name . "c128vector-length")
  (signature lambda ((c128vector? vec)) integer?)
  (tags pure))
 ((name . "c128vector-set!")
  (signature lambda ((c128vector? vec) (integer? i) (c128? value)) undefined))
 ((name . "c128vector->list")
  (signature
   case-lambda
   (((c128vector? vec)) list?)
   (((c128vector? vec) (integer? start)) list?)
   (((c128vector? vec) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "list->c128vector")
  (signature lambda ((list? proper-list)) c128vector?)
  (tags pure))
 ((name . "c128vector-unfold")
  (signature
   case-lambda
   (((procedure? f) (integer? length) seed) c128vector?)
   (((procedure? f) (integer? length) seed) c128vector?))
  (subsigs (f (lambda ((integer? index) state) (values c128? *))))
  (tags pure))
 ((name . "c128vector-copy")
  (signature
   case-lambda
   (((c128vector? vec)) c128vector?)
   (((c128vector? vec) (integer? start)) c128vector?)
   (((c128vector? vec) (integer? start) (integer? end)) c128vector?))
  (tags pure))
 ((name . "c128vector-reverse-copy")
  (signature
   case-lambda
   (((c128vector? vec)) c128vector?)
   (((c128vector? vec) (integer? start)) c128vector?)
   (((c128vector? vec) (integer? start) (integer? end)) c128vector?))
  (tags pure))
 ((name . "c128vector-append")
  (signature lambda ((c128vector? vec) ...) c128vector?)
  (tags pure))
 ((name . "c128vector-concatenate")
  (signature lambda ((list? list-of-c128vectors)) c128vector?)
  (tags pure))
 ((name . "c128vector-append-subvectors")
  (signature
   lambda
   ((c128vector? vec1) (integer? start1) (integer? start2) ...)
   c128vector?)
  (tags pure))
 ((name . "c128vector-empty?")
  (signature lambda ((c128vector? vec)) boolean?)
  (tags pure))
 ((name . "c128vector=")
  (signature lambda ((c128vector? vec) ...) boolean?)
  (tags pure))
 ((name . "c128vector-take")
  (signature lambda ((c128vector? vec) (integer? n)) c128vector?)
  (tags pure))
 ((name . "c128vector-take-right")
  (signature lambda ((c128vector? vec) (integer? n)) c128vector?)
  (tags pure))
 ((name . "c128vector-drop")
  (signature lambda ((c128vector? vec) (integer? n)) c128vector?)
  (tags pure))
 ((name . "c128vector-drop-right")
  (signature lambda ((c128vector? vec) (integer? n)) c128vector?)
  (tags pure))
 ((name . "c128vector-segment")
  (signature lambda ((c128vector? vec) (integer? n)) list?)
  (tags pure))
 ((name . "c128vector-fold")
  (signature
   lambda
   ((procedure? kons) knil (c128vector? vec1) (c128vector? vec2) ...)
   *)
  (subsigs (kons (lambda (state obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c128vector-fold-right")
  (signature
   lambda
   ((procedure? kons) knil (c128vector? vec1) (c128vector? vec2) ...)
   *)
  (subsigs (kons (lambda (state obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c128vector-map")
  (signature
   lambda
   ((procedure? proc) (c128vector? vector1) (c128vector? vector2) ...)
   vector?)
  (subsigs (proc (lambda (obj ...) *)))
  (tags pure))
 ((name . "c128vector-map!")
  (signature
   lambda
   ((procedure? proc) (c128vector? vector1) (c128vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) *))))
 ((name . "c128vector-for-each")
  (signature
   lambda
   ((procedure? proc) (c128vector? vector1) (c128vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) undefined))))
 ((name . "c128vector-count")
  (signature
   lambda
   ((procedure? pred?) (c128vector? vec1) (c128vector? vec2) ...)
   integer?)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c128vector-cumulate")
  (signature lambda ((procedure? f) knil (c128vector? vec)) c128vector?)
  (subsigs (f (lambda (obj1 obj2) *)))
  (tags pure))
 ((name . "c128vector-take-while")
  (signature lambda ((procedure? pred?) (c128vector? vec)) c128vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c128vector-take-while-right")
  (signature lambda ((procedure? pred?) (c128vector? vec)) c128vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c128vector-drop-while")
  (signature lambda ((procedure? pred?) (c128vector? vec)) c128vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c128vector-drop-while-right")
  (signature lambda ((procedure? pred?) (c128vector? vec)) c128vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c128vector-index")
  (signature
   lambda
   ((procedure? pred?) (c128vector? vec1) (c128vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c128vector-index-right")
  (signature
   lambda
   ((procedure? pred?) (c128vector? vec1) (c128vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c128vector-skip")
  (signature
   lambda
   ((procedure? pred?) (c128vector? vec1) (c128vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c128vector-skip-right")
  (signature
   lambda
   ((procedure? pred?) (c128vector? vec1) (c128vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c128vector-any")
  (signature
   lambda
   ((procedure? pred?) (c128vector? vec1) (c128vector? vec2) ...)
   *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c128vector-every")
  (signature
   lambda
   ((procedure? pred?) (c128vector? vec1) (c128vector? vec2) ...)
   *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c128vector-partition")
  (signature
   lambda
   ((procedure? pred?) (c128vector? vec))
   (values c128vector? integer?))
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c128vector-filter")
  (signature lambda ((procedure? pred?) (c128vector? vec1)) c128vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c128vector-remove")
  (signature lambda ((procedure? pred?) (c128vector? vec1)) c128vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c128vector-swap!")
  (signature
   lambda
   ((c128vector? c128vector) (integer? i) (integer? j))
   undefined))
 ((name . "c128vector-fill!")
  (signature
   case-lambda
   (((c128vector? c128vector) (c128? fill)) undefined)
   (((c128vector? c128vector) (c128? fill) (integer? start)) undefined)
   (((c128vector? c128vector) (c128? fill) (integer? start) (integer? end))
    undefined)))
 ((name . "c128vector-reverse!")
  (signature
   case-lambda
   (((c128vector? c128vector)) undefined)
   (((c128vector? c128vector) (integer? start)) undefined)
   (((c128vector? c128vector) (integer? start) (integer? end)) undefined)))
 ((name . "c128vector-copy!")
  (signature
   case-lambda
   (((c128vector? to) (integer? at) (c128vector? from)) undefined)
   (((c128vector? to) (integer? at) (c128vector? from) (integer? start))
    undefined)
   (((c128vector? to)
     (integer? at)
     (c128vector? from)
     (integer? start)
     (integer? end))
    undefined)))
 ((name . "c128vector-reverse-copy!")
  (signature
   case-lambda
   (((c128vector? to) (integer? at) (c128vector? from)) undefined)
   (((c128vector? to) (integer? at) (c128vector? from) (integer? start))
    undefined)
   (((c128vector? to)
     (integer? at)
     (c128vector? from)
     (integer? start)
     (integer? end))
    undefined)))
 ((name . "c128vector-unfold!")
  (signature
   lambda
   ((procedure? f)
    (c128vector? vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...)))))
 ((name . "c128vector-unfold-right!")
  (signature
   lambda
   ((procedure? f)
    (c128vector? vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...)))))
 ((name . "reverse-c128vector->list")
  (signature
   case-lambda
   (((c128vector? vec)) list?)
   (((c128vector? vec) (integer? start)) list?)
   (((c128vector? vec) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "reverse-list->c128vector")
  (signature lambda ((list? proper-list)) c128vector?)
  (tags pure))
 ((name . "c128vector->vector")
  (signature
   case-lambda
   (((c128vector? vec)) vector?)
   (((c128vector? vec) (integer? start)) vector?)
   (((c128vector? vec) (integer? start) (integer? end)) vector?))
  (tags pure))
 ((name . "vector->c128vector")
  (signature
   case-lambda
   (((vector? vec)) c128vector?)
   (((vector? vec) (integer? start)) c128vector?)
   (((vector? vec) (integer? start) (integer? end)) c128vector?))
  (tags pure))
 ((name . "make-c128vector-generator")
  (signature lambda ((c128vector? vec)) procedure?)
  (subsigs (return (lambda () (or eof-object? c128?)))))
 ((name . "c128vector-comparator") (signature value comparator?))
 ((name . "write-c128vector")
  (signature
   case-lambda
   (((c128vector vec)) undefined)
   (((c128vector vec) (output-port? port)) undefined))))
