(((name . "s64vector?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if the if an object is homogenous s64vector, #f otherwise."))
 ((name . "make-s64vector")
  (signature
   case-lambda
   (((integer? size)) s64vector?)
   (((integer? size) (integer? fill)) s64vector?))
  (tags pure)
  (desc . "Create homogenous s64 vector. If fill is given, it must be signed exact integer in the range -(2^63) to (2^63)-1. If fill isn't given, vector is filled with unspecified values which are guaranteed to fit into the range of the s64vector."))
 ((name . "s64vector")
  (signature lambda ((integer? value) ...) s64vector?)
  (tags pure)
  (desc . "Create homogenous s64 vector. Each value must be signed exact integer in the range -(2^63) to (2^63)-1."))
 ((name . "s64vector-length")
  (signature lambda ((s64vector? vec)) integer?)
  (tags pure)
  (desc . "Returns the number of elements in s64vector as an exact integer."))
 ((name . "s64vector-ref")
  (signature lambda ((s64vector? vec) (integer? i)) integer?)
  (tags pure)
  (desc . "Returns i-th element in the vector. i must be in range [0, (s64vector-length vec))"))
 ((name . "s64vector-set!")
  (signature lambda ((s64vector? vec) (integer? i) (integer? value)) undefined)
  (desc . "s64vector-set! stores value in element k of vector. Value must be signed exact integer in the range -(2^63) to (2^63)-1. i must be in range [0, (s64vector-length vec))"))
 ((name . "s64vector->list")
  (signature lambda ((s64vector? vec)) list?)
  (tags pure)
  (desc . "Converts a homogenous vector to a list."))
 ((name . "list->s64vector")
  (signature lambda ((list? proper-list)) s64vector?)
  (subsigs
    (proper-list (list integer?)))
  (tags pure)
  (desc . "Converts a list to a homogenous s64vector. Each element in input list must be signed exact integer in the range -(2^63) to (2^63)-1.")))
