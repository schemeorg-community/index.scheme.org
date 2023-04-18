(((group
    ((name . "angle") (signature lambda ((complex? z)) real?) (tags pure))
    ((name . "imag-part") (signature lambda ((complex? z)) real?) (tags pure))
    ((name . "magnitude") (signature lambda ((complex? z)) real?) (tags pure))
    ((name . "make-polar")
     (signature lambda ((real? x3) (real? x4)) complex?)
     (tags pure))
    ((name . "make-rectangular")
     (signature lambda ((real? x1) (real? x2)) complex?)
     (tags pure))
    ((name . "real-part") (signature lambda ((complex? z)) real?) (tags pure)))
  (desc . "Let x1, x2, x3, and x4 be real numbers and z be a complex number such that
z = x1 + x2i = x3 * e^(i*x4)
Then all of
(make-rectangular x1 x2) => z
(make-polar x3 x4) => z
(real-part z) => x1
(imag-part z) => x2
(magnitude z) => |x3|
(angle z) =⇒ x_angle
are true, where -pi <= x_angle <= pi with x_angle = x4 + 2*pi*n for some integer n.
The make-polar procedure may return an inexact complex number even if its arguments are exact. The real-part and imag-part procedures may return exact real numbers when applied to an inexact complex number if the corresponding argument passed to make-rectangular was exact.")))
