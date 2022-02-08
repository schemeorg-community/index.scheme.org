(
 (angle
   (lambda ((complex? z)) real?)
   (pure))
 
 (imag-part
   (lambda ((complex? z)) real?)
   (pure))
 
 (magnitude
   (lambda ((complex? z)) real?)
   (pure))

 (make-polar
   (lambda ((real? x3) (real? x4)) complex?)
   (pure))
 
 (make-rectangular
   (lambda ((real? x1) (real? x2)) complex?)
   (pure))
 
 (real-part
   (lambda ((complex? z)) real?)
   (pure))
 
 )
