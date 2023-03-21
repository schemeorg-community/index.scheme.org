(
 ((group
    ((name . "acos") (signature lambda ((number? z)) number?) (tags pure))
    ((name . "asin") (signature lambda ((number? z)) number?) (tags pure))
    ((name . "atan")
     (signature
       case-lambda
       (((number? z)) number?)
       (((real? y) (real? x)) number?))
     (tags pure))
    ((name . "cos") (signature lambda ((number? z)) number?) (tags pure))
    ((name . "exp") (signature lambda ((number? z)) number?) (tags pure))
    ((name . "log")
     (signature
       case-lambda
       (((number? z)) number?)
       (((number? z1) (number? z2)) number?))
     (tags pure))
    ((name . "sin") (signature lambda ((number? z)) number?) (tags pure))
    ((name . "tan") (signature lambda ((number? z)) number?) (tags pure)))
  (desc . "These procedures compute the usual transcendental functions. The log procedure computes the natural logarithm of z (not the base ten logarithm) if a single argument is given, or the base-z2 logarithm of z1 if two arguments are given. The asin, acos, and atan procedures compute arcsine (sin−1), arc-cosine (cos−1), and arctangent (tan−1), respectively. The two-argument variant of atan computes (angle (make-rectangular x y)) (see below), even in implementations that don’t support complex numbers.
In general, the mathematical functions log, arcsine, arccosine, and arctangent are multiply defined. The value of log z is defined to be the one whose imaginary part lies in the range from −π (inclusive if -0.0 is distinguished, exclusive otherwise) to π (inclusive). The value of log 0 is mathematically undefined. With log defined this way, the values of sin−1 z, cos−1 z, and tan−1 z are according to the following formulæ:

sin^-1 z = -i log(iz + (1 - z^2)^0.5)
cos^-1 z = pi/2 - sin^-1 z
tan^-1 z = (log(1 + iz) - log(1 - iz)) / (2i)"))
 ((name . "sqrt")
  (signature lambda ((number? z)) number?)
  (tags pure)
  (desc . "Returns the principal square root of z. The result will have either a positive real part, or a zero real part and a non-negative imaginary part."))
 ((name . "finite?")
  (signature lambda ((number? z)) boolean?)
  (tags pure)
  (desc . "The finite? procedure returns #t on all real numbers except +inf.0, -inf.0, and +nan.0, and on complex numbers if their real and imaginary parts are both finite. Otherwise it returns #f."))
 ((name . "infinite?") 
  (signature lambda ((number? z)) boolean?)
  (tags pure)
  (desc . "The infinite? procedure returns #t on the real numbers +inf.0 and -inf.0, and on complex numbers if their real or imaginary parts or both are infinite. Otherwise it returns #f."))
 ((name . "nan?")
  (signature lambda ((number? z)) boolean?)
  (tags pure)
  (desc . "The nan? procedure returns #t on +nan.0, and on complex numbers if their real or imaginary parts or both are +nan.0. Otherwise it returns #f.")))
