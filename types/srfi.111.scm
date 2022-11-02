(((name . "box") (signature lambda (value) box?) (tags pure))
 ((name . "box?") (signature lambda (object) boolean?) (tags pure predicate))
 ((name . "unbox") (signature lambda ((box? box)) *) (tags pure))
 ((name . "set-box!") (signature lambda ((box? box) value) undefined)))
