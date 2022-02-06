((eq? 
   (lambda (a b) boolean?)
   (pure))
 
 (map
   (lambda ((procedure? mapper) (list? lst)) list?)
   (pure)
   ((mapper
      (lambda (el) *))))
 
 (integer?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (exact?))
 
 (exact?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (number?))
 
 (apply
   (lambda ((procedure? proc) arg ... (list? args)) *)
   (pure)))
