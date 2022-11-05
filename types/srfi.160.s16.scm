(((name . "make-s16vector")
  (signature
   case-lambda
   (((integer? size)) s16vector?)
   (((integer? size) (s16? fill)) s16vector?))
  (tags pure))
 ((name . "s16vector")
  (signature lambda ((s16? value) ...) s16vector?)
  (tags pure))
 ((name . "s16?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes integer?))
 ((name . "s16vector?")
  (signature lambda (obj) boolean?)
  (tags pure predicate))
 ((name . "s16vector-ref")
  (signature lambda ((s16vector? vec) (integer? i)) s16?)
  (tags pure))
 ((name . "s16vector-length")
  (signature lambda ((s16vector? vec)) integer?)
  (tags pure))
 ((name . "s16vector-set!")
  (signature lambda ((s16vector? vec) (integer? i) (s16? value)) undefined))
 ((name . "s16vector->list")
  (signature
   case-lambda
   (((s16vector? vec)) list?)
   (((s16vector? vec) (integer? start)) list?)
   (((s16vector? vec) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "list->s16vector")
  (signature lambda ((list? proper-list)) s16vector?)
  (tags pure))
 ((name . "s16vector-unfold")
  (signature
   case-lambda
   (((procedure? f) (integer? length) seed) s16vector?)
   (((procedure? f) (integer? length) seed) s16vector?))
  (subsigs (f (lambda ((integer? index) state) (values s16? *))))
  (tags pure))
 ((name . "s16vector-copy")
  (signature
   case-lambda
   (((s16vector? vec)) s16vector?)
   (((s16vector? vec) (integer? start)) s16vector?)
   (((s16vector? vec) (integer? start) (integer? end)) s16vector?))
  (tags pure))
 ((name . "s16vector-reverse-copy")
  (signature
   case-lambda
   (((s16vector? vec)) s16vector?)
   (((s16vector? vec) (integer? start)) s16vector?)
   (((s16vector? vec) (integer? start) (integer? end)) s16vector?))
  (tags pure))
 ((name . "s16vector-append")
  (signature lambda ((s16vector? vec) ...) s16vector?)
  (tags pure))
 ((name . "s16vector-concatenate")
  (signature lambda ((list? list-of-s16vectors)) s16vector?)
  (tags pure))
 ((name . "s16vector-append-subvectors")
  (signature
   lambda
   ((s16vector? vec1) (integer? start1) (integer? start2) ...)
   s16vector?)
  (tags pure))
 ((name . "s16vector-empty?")
  (signature lambda ((s16vector? vec)) boolean?)
  (tags pure))
 ((name . "s16vector=")
  (signature lambda ((s16vector? vec) ...) boolean?)
  (tags pure))
 ((name . "s16vector-take")
  (signature lambda ((s16vector? vec) (integer? n)) s16vector?)
  (tags pure))
 ((name . "s16vector-take-right")
  (signature lambda ((s16vector? vec) (integer? n)) s16vector?)
  (tags pure))
 ((name . "s16vector-drop")
  (signature lambda ((s16vector? vec) (integer? n)) s16vector?)
  (tags pure))
 ((name . "s16vector-drop-right")
  (signature lambda ((s16vector? vec) (integer? n)) s16vector?)
  (tags pure))
 ((name . "s16vector-segment")
  (signature lambda ((s16vector? vec) (integer? n)) list?)
  (tags pure))
 ((name . "s16vector-fold")
  (signature
   lambda
   ((procedure? kons) knil (s16vector? vec1) (s16vector? vec2) ...)
   *)
  (subsigs (kons (lambda (state obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s16vector-fold-right")
  (signature
   lambda
   ((procedure? kons) knil (s16vector? vec1) (s16vector? vec2) ...)
   *)
  (subsigs (kons (lambda (state obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s16vector-map")
  (signature
   lambda
   ((procedure? proc) (s16vector? vector1) (s16vector? vector2) ...)
   vector?)
  (subsigs (proc (lambda (obj ...) *)))
  (tags pure))
 ((name . "s16vector-map!")
  (signature
   lambda
   ((procedure? proc) (s16vector? vector1) (s16vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) *))))
 ((name . "s16vector-for-each")
  (signature
   lambda
   ((procedure? proc) (s16vector? vector1) (s16vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) undefined))))
 ((name . "s16vector-count")
  (signature
   lambda
   ((procedure? pred?) (s16vector? vec1) (s16vector? vec2) ...)
   integer?)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s16vector-cumulate")
  (signature lambda ((procedure? f) knil (s16vector? vec)) s16vector?)
  (subsigs (f (lambda (obj1 obj2) *)))
  (tags pure))
 ((name . "s16vector-take-while")
  (signature lambda ((procedure? pred?) (s16vector? vec)) s16vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s16vector-take-while-right")
  (signature lambda ((procedure? pred?) (s16vector? vec)) s16vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s16vector-drop-while")
  (signature lambda ((procedure? pred?) (s16vector? vec)) s16vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s16vector-drop-while-right")
  (signature lambda ((procedure? pred?) (s16vector? vec)) s16vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s16vector-index")
  (signature
   lambda
   ((procedure? pred?) (s16vector? vec1) (s16vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s16vector-index-right")
  (signature
   lambda
   ((procedure? pred?) (s16vector? vec1) (s16vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s16vector-skip")
  (signature
   lambda
   ((procedure? pred?) (s16vector? vec1) (s16vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s16vector-skip-right")
  (signature
   lambda
   ((procedure? pred?) (s16vector? vec1) (s16vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s16vector-any")
  (signature
   lambda
   ((procedure? pred?) (s16vector? vec1) (s16vector? vec2) ...)
   *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s16vector-every")
  (signature
   lambda
   ((procedure? pred?) (s16vector? vec1) (s16vector? vec2) ...)
   *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "s16vector-partition")
  (signature
   lambda
   ((procedure? pred?) (s16vector? vec))
   (values s16vector? integer?))
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s16vector-filter")
  (signature lambda ((procedure? pred?) (s16vector? vec1)) s16vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s16vector-remove")
  (signature lambda ((procedure? pred?) (s16vector? vec1)) s16vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "s16vector-swap!")
  (signature
   lambda
   ((s16vector? s16vector) (integer? i) (integer? j))
   undefined))
 ((name . "s16vector-fill!")
  (signature
   case-lambda
   (((s16vector? s16vector) (s16? fill)) undefined)
   (((s16vector? s16vector) (s16? fill) (integer? start)) undefined)
   (((s16vector? s16vector) (s16? fill) (integer? start) (integer? end))
    undefined)))
 ((name . "s16vector-reverse!")
  (signature
   case-lambda
   (((s16vector? s16vector)) undefined)
   (((s16vector? s16vector) (integer? start)) undefined)
   (((s16vector? s16vector) (integer? start) (integer? end)) undefined)))
 ((name . "s16vector-copy!")
  (signature
   case-lambda
   (((s16vector? to) (integer? at) (s16vector? from)) undefined)
   (((s16vector? to) (integer? at) (s16vector? from) (integer? start))
    undefined)
   (((s16vector? to)
     (integer? at)
     (s16vector? from)
     (integer? start)
     (integer? end))
    undefined)))
 ((name . "s16vector-reverse-copy!")
  (signature
   case-lambda
   (((s16vector? to) (integer? at) (s16vector? from)) undefined)
   (((s16vector? to) (integer? at) (s16vector? from) (integer? start))
    undefined)
   (((s16vector? to)
     (integer? at)
     (s16vector? from)
     (integer? start)
     (integer? end))
    undefined)))
 ((name . "s16vector-unfold!")
  (signature
   lambda
   ((procedure? f)
    (s16vector? vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...)))))
 ((name . "s16vector-unfold-right!")
  (signature
   lambda
   ((procedure? f)
    (s16vector? vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...)))))
 ((name . "reverse-s16vector->list")
  (signature
   case-lambda
   (((s16vector? vec)) list?)
   (((s16vector? vec) (integer? start)) list?)
   (((s16vector? vec) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "reverse-list->s16vector")
  (signature lambda ((list? proper-list)) s16vector?)
  (tags pure))
 ((name . "s16vector->vector")
  (signature
   case-lambda
   (((s16vector? vec)) vector?)
   (((s16vector? vec) (integer? start)) vector?)
   (((s16vector? vec) (integer? start) (integer? end)) vector?))
  (tags pure))
 ((name . "vector->s16vector")
  (signature
   case-lambda
   (((vector? vec)) s16vector?)
   (((vector? vec) (integer? start)) s16vector?)
   (((vector? vec) (integer? start) (integer? end)) s16vector?))
  (tags pure))
 ((name . "make-s16vector-generator")
  (signature lambda ((s16vector? vec)) procedure?)
  (subsigs (return (lambda () (or eof-object? s16?)))))
 ((name . "s16vector-comparator") (signature value comparator?))
 ((name . "write-s16vector")
  (signature
   case-lambda
   (((s16vector vec)) undefined)
   (((s16vector vec) (output-port? port)) undefined))))
