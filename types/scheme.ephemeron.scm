(
 
 (ephemeron?
   (lambda (object) boolean?)
   (pure predicate))
 
 (make-ephemeron
   (lambda (key datum) ephemeron?))
 
 (ephemeron-broken?
   (lambda ((ephemeron? ephemeron)) boolean?))
 
 (ephemeron-key
   (lambda ((ephemeron? ephemeron)) *))
 
 (ephemeron-datum
   (lambda ((ephemeron? ephemeron)) *))
 
 (reference-barrier
   (lambda (key) *))
 
 )
