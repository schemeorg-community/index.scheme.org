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
   (pure))
 
 (if
   (syntax-rules ()
     ((_ test consequent))
     ((_ test consequent alternate))))
 
 (->char-set
   (lambda ((char? x)) string?))
 
 (|char-set:lower-case|
   (value char-set?)))
