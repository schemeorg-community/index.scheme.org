(((name . "make-u16vector")
  (signature
   case-lambda
   (((integer? size)) u16vector?)
   (((integer? size) (u16? fill)) u16vector?))
  (tags pure))
 ((name . "u16vector")
  (signature lambda ((u16? value) ...) u16vector?)
  (tags pure))
 ((name . "u16?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes integer?))
 ((name . "u16vector?")
  (signature lambda (obj) boolean?)
  (tags pure predicate))
 ((name . "u16vector-ref")
  (signature lambda ((u16vector? vec) (integer? i)) u16?)
  (tags pure))
 ((name . "u16vector-length")
  (signature lambda ((u16vector? vec)) integer?)
  (tags pure))
 ((name . "u16vector-set!")
  (signature lambda ((u16vector? vec) (integer? i) (u16? value)) undefined))
 ((name . "u16vector->list")
  (signature
   case-lambda
   (((u16vector? vec)) list?)
   (((u16vector? vec) (integer? start)) list?)
   (((u16vector? vec) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "list->u16vector")
  (signature lambda ((list? proper-list)) u16vector?)
  (tags pure))
 ((name . "u16vector-unfold")
  (signature
   case-lambda
   (((procedure? f) (integer? length) seed) u16vector?)
   (((procedure? f) (integer? length) seed) u16vector?))
  (subsigs (f (lambda ((integer? index) state) (values u16? *))))
  (tags pure))
 ((name . "u16vector-copy")
  (signature
   case-lambda
   (((u16vector? vec)) u16vector?)
   (((u16vector? vec) (integer? start)) u16vector?)
   (((u16vector? vec) (integer? start) (integer? end)) u16vector?))
  (tags pure))
 ((name . "u16vector-reverse-copy")
  (signature
   case-lambda
   (((u16vector? vec)) u16vector?)
   (((u16vector? vec) (integer? start)) u16vector?)
   (((u16vector? vec) (integer? start) (integer? end)) u16vector?))
  (tags pure))
 ((name . "u16vector-append")
  (signature lambda ((u16vector? vec) ...) u16vector?)
  (tags pure))
 ((name . "u16vector-concatenate")
  (signature lambda ((list? list-of-u16vectors)) u16vector?)
  (tags pure))
 ((name . "u16vector-append-subvectors")
  (signature
   lambda
   ((u16vector? vec1) (integer? start1) (integer? start2) ...)
   u16vector?)
  (tags pure))
 ((name . "u16vector-empty?")
  (signature lambda ((u16vector? vec)) boolean?)
  (tags pure))
 ((name . "u16vector=")
  (signature lambda ((u16vector? vec) ...) boolean?)
  (tags pure))
 ((name . "u16vector-take")
  (signature lambda ((u16vector? vec) (integer? n)) u16vector?)
  (tags pure))
 ((name . "u16vector-take-right")
  (signature lambda ((u16vector? vec) (integer? n)) u16vector?)
  (tags pure))
 ((name . "u16vector-drop")
  (signature lambda ((u16vector? vec) (integer? n)) u16vector?)
  (tags pure))
 ((name . "u16vector-drop-right")
  (signature lambda ((u16vector? vec) (integer? n)) u16vector?)
  (tags pure))
 ((name . "u16vector-segment")
  (signature lambda ((u16vector? vec) (integer? n)) list?)
  (tags pure))
 ((name . "u16vector-fold")
  (signature
   lambda
   ((procedure? kons) knil (u16vector? vec1) (u16vector? vec2) ...)
   *)
  (subsigs (kons (lambda (state obj1 obj2 ...) *)))
  (tags pure))
 ((name . "u16vector-fold-right")
  (signature
   lambda
   ((procedure? kons) knil (u16vector? vec1) (u16vector? vec2) ...)
   *)
  (subsigs (kons (lambda (state obj1 obj2 ...) *)))
  (tags pure))
 ((name . "u16vector-map")
  (signature
   lambda
   ((procedure? proc) (u16vector? vector1) (u16vector? vector2) ...)
   vector?)
  (subsigs (proc (lambda (obj ...) *)))
  (tags pure))
 ((name . "u16vector-map!")
  (signature
   lambda
   ((procedure? proc) (u16vector? vector1) (u16vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) *))))
 ((name . "u16vector-for-each")
  (signature
   lambda
   ((procedure? proc) (u16vector? vector1) (u16vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) undefined))))
 ((name . "u16vector-count")
  (signature
   lambda
   ((procedure? pred?) (u16vector? vec1) (u16vector? vec2) ...)
   integer?)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "u16vector-cumulate")
  (signature lambda ((procedure? f) knil (u16vector? vec)) u16vector?)
  (subsigs (f (lambda (obj1 obj2) *)))
  (tags pure))
 ((name . "u16vector-take-while")
  (signature lambda ((procedure? pred?) (u16vector? vec)) u16vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "u16vector-take-while-right")
  (signature lambda ((procedure? pred?) (u16vector? vec)) u16vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "u16vector-drop-while")
  (signature lambda ((procedure? pred?) (u16vector? vec)) u16vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "u16vector-drop-while-right")
  (signature lambda ((procedure? pred?) (u16vector? vec)) u16vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "u16vector-index")
  (signature
   lambda
   ((procedure? pred?) (u16vector? vec1) (u16vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "u16vector-index-right")
  (signature
   lambda
   ((procedure? pred?) (u16vector? vec1) (u16vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "u16vector-skip")
  (signature
   lambda
   ((procedure? pred?) (u16vector? vec1) (u16vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "u16vector-skip-right")
  (signature
   lambda
   ((procedure? pred?) (u16vector? vec1) (u16vector? vec2) ...)
   (or integer? #f))
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "u16vector-any")
  (signature
   lambda
   ((procedure? pred?) (u16vector? vec1) (u16vector? vec2) ...)
   *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "u16vector-every")
  (signature
   lambda
   ((procedure? pred?) (u16vector? vec1) (u16vector? vec2) ...)
   *)
  (subsigs (pred? (lambda (obj1 obj2 ...) *)))
  (tags pure))
 ((name . "u16vector-partition")
  (signature
   lambda
   ((procedure? pred?) (u16vector? vec))
   (values u16vector? integer?))
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "u16vector-filter")
  (signature lambda ((procedure? pred?) (u16vector? vec1)) u16vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "u16vector-remove")
  (signature lambda ((procedure? pred?) (u16vector? vec1)) u16vector?)
  (subsigs (pred? (lambda (obj) boolean?)))
  (tags pure))
 ((name . "u16vector-swap!")
  (signature
   lambda
   ((u16vector? u16vector) (integer? i) (integer? j))
   undefined))
 ((name . "u16vector-fill!")
  (signature
   case-lambda
   (((u16vector? u16vector) (u16? fill)) undefined)
   (((u16vector? u16vector) (u16? fill) (integer? start)) undefined)
   (((u16vector? u16vector) (u16? fill) (integer? start) (integer? end))
    undefined)))
 ((name . "u16vector-reverse!")
  (signature
   case-lambda
   (((u16vector? u16vector)) undefined)
   (((u16vector? u16vector) (integer? start)) undefined)
   (((u16vector? u16vector) (integer? start) (integer? end)) undefined)))
 ((name . "u16vector-copy!")
  (signature
   case-lambda
   (((u16vector? to) (integer? at) (u16vector? from)) undefined)
   (((u16vector? to) (integer? at) (u16vector? from) (integer? start))
    undefined)
   (((u16vector? to)
     (integer? at)
     (u16vector? from)
     (integer? start)
     (integer? end))
    undefined)))
 ((name . "u16vector-reverse-copy!")
  (signature
   case-lambda
   (((u16vector? to) (integer? at) (u16vector? from)) undefined)
   (((u16vector? to) (integer? at) (u16vector? from) (integer? start))
    undefined)
   (((u16vector? to)
     (integer? at)
     (u16vector? from)
     (integer? start)
     (integer? end))
    undefined)))
 ((name . "u16vector-unfold!")
  (signature
   lambda
   ((procedure? f)
    (u16vector? vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...)))))
 ((name . "u16vector-unfold-right!")
  (signature
   lambda
   ((procedure? f)
    (u16vector? vec)
    (integer? start)
    (integer? end)
    initial-seed
    ...)
   undefined)
  (subsigs (f (lambda ((integer? index) seed ...) (values * * ...)))))
 ((name . "reverse-u16vector->list")
  (signature
   case-lambda
   (((u16vector? vec)) list?)
   (((u16vector? vec) (integer? start)) list?)
   (((u16vector? vec) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "reverse-list->u16vector")
  (signature lambda ((list? proper-list)) u16vector?)
  (tags pure))
 ((name . "u16vector->vector")
  (signature
   case-lambda
   (((u16vector? vec)) vector?)
   (((u16vector? vec) (integer? start)) vector?)
   (((u16vector? vec) (integer? start) (integer? end)) vector?))
  (tags pure))
 ((name . "vector->u16vector")
  (signature
   case-lambda
   (((vector? vec)) u16vector?)
   (((vector? vec) (integer? start)) u16vector?)
   (((vector? vec) (integer? start) (integer? end)) u16vector?))
  (tags pure))
 ((name . "make-u16vector-generator")
  (signature lambda ((u16vector? vec)) procedure?)
  (subsigs (return (lambda () (or eof-object? u16?)))))
 ((name . "u16vector-comparator") (signature value comparator?))
 ((name . "write-u16vector")
  (signature
   case-lambda
   (((u16vector vec)) undefined)
   (((u16vector vec) (output-port? port)) undefined))))
