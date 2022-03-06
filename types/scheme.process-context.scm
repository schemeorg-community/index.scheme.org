(
 (command-line
   (lambda () list?)
   (pure))
 
 (emergency-exit
   (lambda () undefined))
 
 (emergency-exit
   (lambda (obj) undefined))
 
 (exit
   (lambda () undefined))

 (exit
   (lambda (obj) undefined))
 
 (get-environment-variable
   (lambda ((string? name)) (or string? #f))
   (pure))
 
 (get-environment-variables
   (lambda () list?)
   (pure))
 )
