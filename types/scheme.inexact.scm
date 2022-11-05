(((name . "acos") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "asin") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "atan")
  (signature
   case-lambda
   (((number? z)) number?)
   (((real? y) (real? x)) number?))
  (tags pure))
 ((name . "cos") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "exp") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "finite?") (signature lambda ((number? z)) boolean?) (tags pure))
 ((name . "infinite?") (signature lambda ((number? z)) boolean?) (tags pure))
 ((name . "log")
  (signature
   case-lambda
   (((number? z)) number?)
   (((number? z1) (number? z2)) number?))
  (tags pure))
 ((name . "nan?") (signature lambda ((number? z)) boolean?) (tags pure))
 ((name . "sin") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "sqrt") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "tan") (signature lambda ((number? z)) number?) (tags pure)))
