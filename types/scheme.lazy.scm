(
 (force
   (lambda ((promise? promise)) *))
 
 (make-promise
   (lambda (obj) promise?)
   (pure))
 
 (promise?
   (lambda (obj) boolean?)
   (pure predicate))
 )
