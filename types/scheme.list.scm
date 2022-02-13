(
 (cons
   (lambda (a d) pair?)
   (pure))
 
 (list
   (lambda (object ...) list?)
   (pure))
 
 (xcons
   (lambda (d a) pair?)
   (pure))
 
 (cons*
   (lambda (elt1 elt2 ...) *))
 
 (make-list
   (lambda ((integer? n)) list?))
 
 (make-list
   (lambda ((integer? n) fill) list?)
   (pure))
 
 (list-tabulate
   (lambda ((integer? n) (procedure? init-proc)) list?)
   (pure)
   ((init-proc (lambda ((integer? i)) *))))
 
 )
