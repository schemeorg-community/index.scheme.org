(

 ((name . u64vector?)
  (signature lambda (obj) boolean?)
  (tags pure predicate))
 ((name . make-u64vector)
  (signature lambda ((integer? size)) u64vector?))
 ((name . make-u64vector)
  (signature lambda ((integer? size) (integer? fill)) u64vector?)
  (tags pure))
 ((name . u64vector-length)
  (signature lambda ((u64vector? vec)) integer?)
  (tags pure))
 ((name . u64vector-ref)
  (signature lambda ((u64vector? vec) (integer? i)) integer?)
  (tags pure))
 ((name . u64vector-set!)
  (signature lambda ((u64vector? vec) (integer? i) (integer? value)) undefined))
 ((name . u64vector->list)
  (signature lambda ((u64vector? vec)) list?)
  (tags pure))
 ((name . list->u64vector)
  (signature lambda ((list? proper-list)) u64vector?)
  (tags pure))

)