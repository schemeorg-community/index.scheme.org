(((name . "make-s8vector")
  (signature
   case-lambda
   (((integer? size)) s8vector?)
   (((integer? size) (s8? fill)) s8vector?))
  (tags pure))
 ((name . "s8vector")
  (signature lambda ((s8? value) ...) s8vector?)
  (tags pure))
 ((name . "s8?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes integer?))
 ((name . "s8vector?") (signature lambda (obj) boolean?) (tags pure predicate))
 ((name . "s8vector-ref")
  (signature lambda ((s8vector? vec) (integer? i)) s8?)
  (tags pure))
 ((name . "s8vector-length")
  (signature lambda ((s8vector? vec)) integer?)
  (tags pure))
 ((name . "s8vector-set!")
  (signature lambda ((s8vector? vec) (integer? i) (s8? value)) undefined))
 ((name . "s8vector->list")
  (signature
   case-lambda
   (((s8vector? vec)) list?)
   (((s8vector? vec) (integer? start)) list?)
   (((s8vector? vec) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "list->s8vector")
  (signature lambda ((list? proper-list)) s8vector?)
  (tags pure))
 ((name . "s8vector-unfold")
  (signature
   case-lambda
   (((procedure? f) (integer? length) seed) s8vector?)
   (((procedure? f) (integer? length) seed) s8vector?))
  (subsigs (f (lambda ((integer? index) state) (values s8? *))))
  (tags pure))
 ((name . "s8vector-copy")
  (signature
   case-lambda
   (((s8vector? vec)) s8vector?)
   (((s8vector? vec) (integer? start)) s8vector?)
   (((s8vector? vec) (integer? start) (integer? end)) s8vector?))
  (tags pure))
 ((name . "s8vector-reverse-copy")
  (signature
   case-lambda
   (((s8vector? vec)) s8vector?)
   (((s8vector? vec) (integer? start)) s8vector?)
   (((s8vector? vec) (integer? start) (integer? end)) s8vector?))
  (tags pure))
 ((name . "s8vector-append")
  (signature lambda ((s8vector? vec) ...) s8vector?)
  (tags pure))
 ((name . "s8vector-concatenate")
  (signature lambda ((list? list-of-s8vectors)) s8vector?)
  (tags pure))
 ((name . "s8vector-append-subvectors")
  (signature
   lambda
   ((s8vector? vec1) (integer? start1) (integer? start2) ...)
   s8vector?)
  (tags pure))
 ((name . "s8vector-empty?")
  (signature lambda ((s8vector? vec)) boolean?)
  (tags pure))
 ((name . "s8vector=")
  (signature lambda ((s8vector? vec) ...) boolean?)
  (tags pure))
 ((name . "s8vector-take")
  (signature lambda ((s8vector? vec) (integer? n)) s8vector?)
  (tags pure))
 ((name . "s8vector-take-right")
  (signature lambda ((s8vector? vec) (integer? n)) s8vector?)
  (tags pure))
 ((name . "s8vector-drop")
  (signature lambda ((s8vector? vec) (integer? n)) s8vector?)
  (tags pure))
 ((name . "s8vector-drop-right")
  (signature lambda ((s8vector? vec) (integer? n)) s8vector?)
  (tags pure))
 ((name . "s8vector-segment")
  (signature lambda ((s8vector? vec) (integer? n)) list?)
  (tags pure))
 ((name . "s8vector-fold")
  (signature
   lambda
   ((procedure? kons) knil (s8vector? vec1) (s8vector? vec2) ...)
   *)
  (subsigs (kons (lambda (state obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s8vector-fold-right")
  (signature
   lambda
   ((procedure? kons) knil (s8vector? vec1) (s8vector? vec2) ...)
   *)
  (subsigs (kons (lambda (state obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s8vector-map")
  (signature
   lambda
   ((procedure? proc) (s8vector? vector1) (s8vector? vector2) ...)
   vector?)
  (subsigs (proc (lambda (obj ...) *)))
  (tags pure))
 ((name . "s8vector-map!")
  (signature
   lambda
   ((procedure? proc) (s8vector? vector1) (s8vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) *))))
 ((name . "s8vector-for-each")
  (signature
   lambda
   ((procedure? proc) (s8vector? vector1) (s8vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) undefined))))
 ((name . "s8vector-count")
  (signature
   lambda
   ((procedure? pred?) (s8vector? vec1) (s8vector? vec2) ...)
   integer?)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s8vector-cumulate")
  (signature lambda ((procedure? f) knil (s8vector? vec)) s8vector?)
  (subsigs (f (lambda (obj1 obj2) *)))
  (tags pure))
 ((name . "s8vector-take-while")
  (signature lambda ((procedure? pred?) (s8vector? vec)) s8vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s8vector-take-while-right")
  (signature lambda ((procedure? pred?) (s8vector? vec)) s8vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s8vector-drop-while")
  (signature lambda ((procedure? pred?) (s8vector? vec)) s8vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s8vector-drop-while-right")
  (signature lambda ((procedure? pred?) (s8vector? vec)) s8vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s8vector-index")
  (signature
   lambda
   ((procedure? pred?) (s8vector? vec1) (s8vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s8vector-index-right")
  (signature
   lambda
   ((procedure? pred?) (s8vector? vec1) (s8vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s8vector-skip")
  (signature
   lambda
   ((procedure? pred?) (s8vector? vec1) (s8vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s8vector-skip-right")
  (signature
   lambda
   ((procedure? pred?) (s8vector? vec1) (s8vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s8vector-any")
  (signature
   lambda
   ((procedure? pred?) (s8vector? vec1) (s8vector? vec2) ...)
   *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s8vector-every")
  (signature
   lambda
   ((procedure? pred?) (s8vector? vec1) (s8vector? vec2) ...)
   *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s8vector-partition")
  (signature
   lambda
   ((procedure? pred?) (s8vector? vec))
   (values s8vector? integer?))
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s8vector-filter")
  (signature lambda ((procedure? pred?) (s8vector? vec1)) s8vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s8vector-remove")
  (signature lambda ((procedure? pred?) (s8vector? vec1)) s8vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s8vector-swap!")
  (signature
   lambda
   ((s8vector? s8vector) (integer? i) (integer? j))
   undefined))
 ((name . "s8vector-fill!")
  (signature
   case-lambda
   (((s8vector? s8vector) (s8? fill)) undefined)
   (((s8vector? s8vector) (s8? fill) (integer? start)) undefined)
   (((s8vector? s8vector) (s8? fill) (integer? start) (integer? end))
    undefined)))
 ((name . "s8vector-reverse!")
  (signature
   case-lambda
   (((s8vector? s8vector)) undefined)
   (((s8vector? s8vector) (integer? start)) undefined)
   (((s8vector? s8vector) (integer? start) (integer? end)) undefined)))
 ((name . "s8vector-copy!")
  (signature
   case-lambda
   (((s8vector? to) (integer? at) (s8vector? from)) undefined)
   (((s8vector? to) (integer? at) (s8vector? from) (integer? start)) undefined)
   (((s8vector? to)
     (integer? at)
     (s8vector? from)
     (integer? start)
     (integer? end))
    undefined)))
 ((name . "s8vector-reverse-copy!")
  (signature
   case-lambda
   (((s8vector? to) (integer? at) (s8vector? from)) undefined)
   (((s8vector? to) (integer? at) (s8vector? from) (integer? start)) undefined)
   (((s8vector? to)
     (integer? at)
     (s8vector? from)
     (integer? start)
     (integer? end))
    undefined)))
 ((name . "s8vector-unfold!")
  (signature
   lambda
   ((procedure? f)
    (s8vector? vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...)))))
 ((name . "s8vector-unfold-right!")
  (signature
   lambda
   ((procedure? f)
    (s8vector? vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...)))))
 ((name . "reverse-s8vector->list")
  (signature
   case-lambda
   (((s8vector? vec)) list?)
   (((s8vector? vec) (integer? start)) list?)
   (((s8vector? vec) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "reverse-list->s8vector")
  (signature lambda ((list? proper-list)) s8vector?)
  (tags pure))
 ((name . "s8vector->vector")
  (signature
   case-lambda
   (((s8vector? vec)) vector?)
   (((s8vector? vec) (integer? start)) vector?)
   (((s8vector? vec) (integer? start) (integer? end)) vector?))
  (tags pure))
 ((name . "vector->s8vector")
  (signature
   case-lambda
   (((vector? vec)) s8vector?)
   (((vector? vec) (integer? start)) s8vector?)
   (((vector? vec) (integer? start) (integer? end)) s8vector?))
  (tags pure))
 ((name . "make-s8vector-generator")
  (signature lambda ((s8vector? vec)) procedure?)
  (subsigs (return (lambda () (or eof-object? s8?)))))
 ((name . "s8vector-comparator") (signature value comparator?))
 ((name . "write-s8vector")
  (signature
   case-lambda
   (((s8vector vec)) undefined)
   (((s8vector vec) (output-port? port)) undefined))))
