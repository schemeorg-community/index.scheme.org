(
 
 (box
   (lambda (value) box?)
   (pure))
 
 (box?
   (lambda (object) boolean?)
   (pure predicate))
 
 (unbox
   (lambda ((box? box)) *)
   (pure))
 
 (set-box!
   (lambda ((box? box) value) undefined)
   ())
 
 )
