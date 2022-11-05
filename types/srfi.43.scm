(((name . "make-vector")
  (signature
   case-lambda
   (((integer? k)) vector?)
   (((integer? k) fill) vector?))
  (tags pure))
 ((name . "vector") (signature lambda (obj ...) vector?) (tags pure))
 ((name . "vector-unfold")
  (signature
   lambda
   ((procedure? f) (integer? length) initial-seed ...)
   vector?)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
  (tags pure))
 ((name . "vector-unfold-right")
  (signature
   lambda
   ((procedure? f) (integer? length) initial-seed ...)
   vector?)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...))))
  (tags pure))
 ((name . "vector-copy")
  (signature
   case-lambda
   (((vector? vector)) vector?)
   (((vector? vector) (integer? start)) vector?)
   (((vector? vector) (integer? start) (integer? end)) vector?))
  (tags pure))
 ((name . "vector-reverse-copy")
  (signature
   case-lambda
   (((vector? vector)) vector?)
   (((vector? vector) (integer? start)) vector?)
   (((vector? vector) (integer? start) (integer? end)) vector?))
  (tags pure))
 ((name . "vector-append")
  (signature lambda ((vector? vector) ...) vector?)
  (tags pure))
 ((name . "vector-concatenate")
  (signature lambda ((list? list-of-vectors) ...) vector?)
  (tags pure))
 ((name . "vector?") (signature lambda (obj) boolean?) (tags pure predicate))
 ((name . "vector-empty?")
  (signature lambda ((vector? vec)) boolean?)
  (tags pure))
 ((name . "vector=")
  (signature lambda ((procedure? elt=?) (vector? vec) ...) boolean?)
  (subsigs (elt=? (lambda (a b) boolean?)))
  (tags pure))
 ((name . "vector-ref")
  (signature lambda ((vector? vector) (integer? k)) *)
  (tags pure))
 ((name . "vector-length")
  (signature lambda ((vector? vector)) integer?)
  (tags pure))
 ((name . "vector-fold")
  (signature
   lambda
   ((procedure? kons) knil (vector? vec1) (vector? vec2) ...)
   *)
  (subsigs (kons (lambda ((integer? index) state obj1 obj2 ...) *)))
  (tags pure))
 ((name . "vector-fold-right")
  (signature
   lambda
   ((procedure? kons) knil (vector? vec1) (vector? vec2) ...)
   *)
  (subsigs (kons (lambda ((integer? index) state obj1 obj2 ...) *)))
  (tags pure))
 ((name . "vector-map")
  (signature
   lambda
   ((procedure? proc) (vector? vector1) (vector? vector2) ...)
   vector?)
  (subsigs (proc (lambda ((integer? index) obj ...) *)))
  (tags pure))
 ((name . "vector-map!")
  (signature
   lambda
   ((procedure? proc) (vector? vector1) (vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda ((integer? index) obj ...) *))))
 ((name . "vector-for-each")
  (signature
   lambda
   ((procedure? proc) (vector? vector1) (vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda ((integer? index) obj ...) undefined))))
 ((name . "vector-count")
  (signature
   lambda
   ((procedure? pred?) (vector? vec1) (vector? vec2) ...)
   integer?)
  (subsigs (pred? (lambda ((integer? index) obj1 obj2 ...) *)))
  (tags pure))
 ((name . "vector-index")
  (signature
   lambda
   ((procedure? pred?) (vector? vec1) (vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "vector-index-right")
  (signature
   lambda
   ((procedure? pred?) (vector? vec1) (vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "vector-skip")
  (signature
   lambda
   ((procedure? pred?) (vector? vec1) (vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "vector-skip-right")
  (signature
   lambda
   ((procedure? pred?) (vector? vec1) (vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "vector-binary-search")
  (signature lambda ((vector? vec) value (procedure? cmp)) (or integer? #f))
  (subsigs (cmp (lambda (a b) integer?)))
  (tags pure))
 ((name . "vector-any")
  (signature lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "vector-every")
  (signature lambda ((procedure? pred?) (vector? vec1) (vector? vec2) ...) *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "vector-set!")
  (signature lambda ((vector? vector) (integer? k) obj) undefined))
 ((name . "vector-swap!")
  (signature lambda ((vector? vector) (integer? i) (integer? j)) undefined))
 ((name . "vector-fill!")
  (signature
   case-lambda
   (((vector? vector) fill) undefined)
   (((vector? vector) fill (integer? start)) undefined)
   (((vector? vector) fill (integer? start) (integer? end)) undefined)))
 ((name . "vector-reverse!")
  (signature
   case-lambda
   (((vector? vector)) undefined)
   (((vector? vector) (integer? start)) undefined)
   (((vector? vector) (integer? start) (integer? end)) undefined)))
 ((name . "vector-copy!")
  (signature
   case-lambda
   (((vector? to) (integer? at) (vector? from)) undefined)
   (((vector? to) (integer? at) (vector? from) (integer? start)) undefined)
   (((vector? to) (integer? at) (vector? from) (integer? start) (integer? end))
    undefined)))
 ((name . "vector-reverse-copy!")
  (signature
   case-lambda
   (((vector? to) (integer? at) (vector? from)) undefined)
   (((vector? to) (integer? at) (vector? from) (integer? start)) undefined)
   (((vector? to) (integer? at) (vector? from) (integer? start) (integer? end))
    undefined)))
 ((name . "vector->list")
  (signature
   case-lambda
   (((vector? vector)) list?)
   (((vector? vector) (integer? start)) list?)
   (((vector? vector) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "reverse-vector->list")
  (signature
   case-lambda
   (((vector? vector)) list?)
   (((vector? vector) (integer? start)) list?)
   (((vector? vector) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "list->vector")
  (signature lambda ((list? list)) vector?)
  (tags pure))
 ((name . "reverse-list->vector")
  (signature lambda ((list? list)) vector?)
  (tags pure)))
