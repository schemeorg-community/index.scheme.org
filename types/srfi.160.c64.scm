(((name . "make-c64vector")
  (signature
   case-lambda
   (((integer? size)) c64vector?)
   (((integer? size) (c64? fill)) c64vector?))
  (tags pure))
 ((name . "c64vector")
  (signature lambda ((c64? value) ...) c64vector?)
  (tags pure))
 ((name . "c64?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes number?))
 ((name . "c64vector?")
  (signature lambda (obj) boolean?)
  (tags pure predicate))
 ((name . "c64vector-ref")
  (signature lambda ((c64vector? vec) (integer? i)) c64?)
  (tags pure))
 ((name . "c64vector-length")
  (signature lambda ((c64vector? vec)) integer?)
  (tags pure))
 ((name . "c64vector-set!")
  (signature lambda ((c64vector? vec) (integer? i) (c64? value)) undefined))
 ((name . "c64vector->list")
  (signature
   case-lambda
   (((c64vector? vec)) list?)
   (((c64vector? vec) (integer? start)) list?)
   (((c64vector? vec) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "list->c64vector")
  (signature lambda ((list? proper-list)) c64vector?)
  (tags pure))
 ((name . "c64vector-unfold")
  (signature
   case-lambda
   (((procedure? f) (integer? length) seed) c64vector?)
   (((procedure? f) (integer? length) seed) c64vector?))
  (subsigs (f (lambda ((integer? index) state) (values c64? *))))
  (tags pure))
 ((name . "c64vector-copy")
  (signature
   case-lambda
   (((c64vector? vec)) c64vector?)
   (((c64vector? vec) (integer? start)) c64vector?)
   (((c64vector? vec) (integer? start) (integer? end)) c64vector?))
  (tags pure))
 ((name . "c64vector-reverse-copy")
  (signature
   case-lambda
   (((c64vector? vec)) c64vector?)
   (((c64vector? vec) (integer? start)) c64vector?)
   (((c64vector? vec) (integer? start) (integer? end)) c64vector?))
  (tags pure))
 ((name . "c64vector-append")
  (signature lambda ((c64vector? vec) ...) c64vector?)
  (tags pure))
 ((name . "c64vector-concatenate")
  (signature lambda ((list? list-of-c64vectors)) c64vector?)
  (tags pure))
 ((name . "c64vector-append-subvectors")
  (signature
   lambda
   ((c64vector? vec1) (integer? start1) (integer? start2) ...)
   c64vector?)
  (tags pure))
 ((name . "c64vector-empty?")
  (signature lambda ((c64vector? vec)) boolean?)
  (tags pure))
 ((name . "c64vector=")
  (signature lambda ((c64vector? vec) ...) boolean?)
  (tags pure))
 ((name . "c64vector-take")
  (signature lambda ((c64vector? vec) (integer? n)) c64vector?)
  (tags pure))
 ((name . "c64vector-take-right")
  (signature lambda ((c64vector? vec) (integer? n)) c64vector?)
  (tags pure))
 ((name . "c64vector-drop")
  (signature lambda ((c64vector? vec) (integer? n)) c64vector?)
  (tags pure))
 ((name . "c64vector-drop-right")
  (signature lambda ((c64vector? vec) (integer? n)) c64vector?)
  (tags pure))
 ((name . "c64vector-segment")
  (signature lambda ((c64vector? vec) (integer? n)) list?)
  (tags pure))
 ((name . "c64vector-fold")
  (signature
   lambda
   ((procedure? kons) knil (c64vector? vec1) (c64vector? vec2) ...)
   *)
  (subsigs (kons (lambda (state obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c64vector-fold-right")
  (signature
   lambda
   ((procedure? kons) knil (c64vector? vec1) (c64vector? vec2) ...)
   *)
  (subsigs (kons (lambda (state obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c64vector-map")
  (signature
   lambda
   ((procedure? proc) (c64vector? vector1) (c64vector? vector2) ...)
   vector?)
  (subsigs (proc (lambda (obj ...) *)))
  (tags pure))
 ((name . "c64vector-map!")
  (signature
   lambda
   ((procedure? proc) (c64vector? vector1) (c64vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) *))))
 ((name . "c64vector-for-each")
  (signature
   lambda
   ((procedure? proc) (c64vector? vector1) (c64vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) undefined))))
 ((name . "c64vector-count")
  (signature
   lambda
   ((procedure? pred?) (c64vector? vec1) (c64vector? vec2) ...)
   integer?)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c64vector-cumulate")
  (signature lambda ((procedure? f) knil (c64vector? vec)) c64vector?)
  (subsigs (f (lambda (obj1 obj2) *)))
  (tags pure))
 ((name . "c64vector-take-while")
  (signature lambda ((procedure? pred?) (c64vector? vec)) c64vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c64vector-take-while-right")
  (signature lambda ((procedure? pred?) (c64vector? vec)) c64vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c64vector-drop-while")
  (signature lambda ((procedure? pred?) (c64vector? vec)) c64vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c64vector-drop-while-right")
  (signature lambda ((procedure? pred?) (c64vector? vec)) c64vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c64vector-index")
  (signature
   lambda
   ((procedure? pred?) (c64vector? vec1) (c64vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c64vector-index-right")
  (signature
   lambda
   ((procedure? pred?) (c64vector? vec1) (c64vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c64vector-skip")
  (signature
   lambda
   ((procedure? pred?) (c64vector? vec1) (c64vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c64vector-skip-right")
  (signature
   lambda
   ((procedure? pred?) (c64vector? vec1) (c64vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c64vector-any")
  (signature
   lambda
   ((procedure? pred?) (c64vector? vec1) (c64vector? vec2) ...)
   *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c64vector-every")
  (signature
   lambda
   ((procedure? pred?) (c64vector? vec1) (c64vector? vec2) ...)
   *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "c64vector-partition")
  (signature
   lambda
   ((procedure? pred?) (c64vector? vec))
   (values c64vector? integer?))
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c64vector-filter")
  (signature lambda ((procedure? pred?) (c64vector? vec1)) c64vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c64vector-remove")
  (signature lambda ((procedure? pred?) (c64vector? vec1)) c64vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "c64vector-swap!")
  (signature
   lambda
   ((c64vector? c64vector) (integer? i) (integer? j))
   undefined))
 ((name . "c64vector-fill!")
  (signature
   case-lambda
   (((c64vector? c64vector) (c64? fill)) undefined)
   (((c64vector? c64vector) (c64? fill) (integer? start)) undefined)
   (((c64vector? c64vector) (c64? fill) (integer? start) (integer? end))
    undefined)))
 ((name . "c64vector-reverse!")
  (signature
   case-lambda
   (((c64vector? c64vector)) undefined)
   (((c64vector? c64vector) (integer? start)) undefined)
   (((c64vector? c64vector) (integer? start) (integer? end)) undefined)))
 ((name . "c64vector-copy!")
  (signature
   case-lambda
   (((c64vector? to) (integer? at) (c64vector? from)) undefined)
   (((c64vector? to) (integer? at) (c64vector? from) (integer? start))
    undefined)
   (((c64vector? to)
     (integer? at)
     (c64vector? from)
     (integer? start)
     (integer? end))
    undefined)))
 ((name . "c64vector-reverse-copy!")
  (signature
   case-lambda
   (((c64vector? to) (integer? at) (c64vector? from)) undefined)
   (((c64vector? to) (integer? at) (c64vector? from) (integer? start))
    undefined)
   (((c64vector? to)
     (integer? at)
     (c64vector? from)
     (integer? start)
     (integer? end))
    undefined)))
 ((name . "c64vector-unfold!")
  (signature
   lambda
   ((procedure? f)
    (c64vector? vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...)))))
 ((name . "c64vector-unfold-right!")
  (signature
   lambda
   ((procedure? f)
    (c64vector? vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...)))))
 ((name . "reverse-c64vector->list")
  (signature
   case-lambda
   (((c64vector? vec)) list?)
   (((c64vector? vec) (integer? start)) list?)
   (((c64vector? vec) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "reverse-list->c64vector")
  (signature lambda ((list? proper-list)) c64vector?)
  (tags pure))
 ((name . "c64vector->vector")
  (signature
   case-lambda
   (((c64vector? vec)) vector?)
   (((c64vector? vec) (integer? start)) vector?)
   (((c64vector? vec) (integer? start) (integer? end)) vector?))
  (tags pure))
 ((name . "vector->c64vector")
  (signature
   case-lambda
   (((vector? vec)) c64vector?)
   (((vector? vec) (integer? start)) c64vector?)
   (((vector? vec) (integer? start) (integer? end)) c64vector?))
  (tags pure))
 ((name . "make-c64vector-generator")
  (signature lambda ((c64vector? vec)) procedure?)
  (subsigs (return (lambda () (or eof-object? c64?)))))
 ((name . "c64vector-comparator") (signature value comparator?))
 ((name . "write-c64vector")
  (signature
   case-lambda
   (((c64vector vec)) undefined)
   (((c64vector vec) (output-port? port)) undefined))))
