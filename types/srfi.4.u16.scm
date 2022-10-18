(

 ((name . u16vector?)
  (signature lambda (obj) boolean?)
  (tags pure predicate))
 ((name . make-u16vector)
  (signature lambda ((integer? size)) u16vector?))
 ((name . make-u16vector)
  (signature lambda ((integer? size) (integer? fill)) u16vector?)
  (tags pure))
 ((name . u16vector-length)
  (signature lambda ((u16vector? vec)) integer?)
  (tags pure))
 ((name . u16vector-ref)
  (signature lambda ((u16vector? vec) (integer? i)) integer?)
  (tags pure))
 ((name . u16vector-set!)
  (signature lambda ((u16vector? vec) (integer? i) (integer? value)) undefined))
 ((name . u16vector->list)
  (signature lambda ((u16vector? vec)) list?)
  (tags pure))
 ((name . list->u16vector)
  (signature lambda ((list? proper-list)) u16vector?)
  (tags pure))

)