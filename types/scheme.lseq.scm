(
 
 (generator->lseq
   (lambda ((procedure? generator)) lseq?)
   ()
   ((generator (lambda () *))))
 
 (lseq?
   (lambda (x) boolean?)
   (pure predicate)
   ()
   (list? dotted-list?))
 
 (lseq=?
   (lambda ((procedure? elt=?) (lseq? lseq1) (lseq? lseq2)) boolean?)
   (pure)
   ((elt=? (lambda (a b) boolean?))))
 
 (lseq-car
   (lambda ((lseq? lseq)) *)
   (pure))
 
 (lseq-first
   (lambda ((lseq? lseq)) *)
   (pure))
 
 (lseq-cdr
   (lambda ((lseq? lseq)) lseq?)
   (pure))
 
 (lseq-rest
   (lambda ((lseq? lseq)) lseq?)
   (pure))
 
 (lseq-ref
   (lambda ((lseq? lseq) (integer? i)) *)
   (pure))
 
 (lseq-take
   (lambda ((lseq? lseq) (integer? i)) lseq)
   (pure))
 
 (lseq-drop
   (lambda ((lseq? lseq) (integer? i)) lseq)
   (pure))
 
 (lseq-realize
   (lambda ((lseq? lseq)) list?)
   (pure))
 
 (lseq->generator
   (lambda ((lseq? lseq)) procedure?)
   ()
   ((return (lambda () *))))
 
 (lseq-length
   (lambda ((lseq? lseq)) integer?)
   (pure))
 
 (lseq-append
   (lambda ((lseq? lseq) ...) lseq?)
   (pure))

 (lseq-zip
   (lambda ((lseq? lseq1) (lseq? lseq2) ...) lseq?)
   (pure))
 
 (lseq-map
   (lambda ((procedure? proc) (lseq? lseq1) (lseq? lseq2) ...) lseq?)
   (pure)
   ((proc (lambda (value1 value2 ...) *))))
 
 (lseq-for-each
   (lambda ((procedure? proc) (lseq? lseq1) (lseq? lseq2) ...) *)
   ()
   ((proc (lambda (value1 value2 ...) *))))
 
 (lseq-filter
   (lambda ((procedure? pred) (lseq? lseq)) lseq?)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (lseq-remove
   (lambda ((procedure? pred) (lseq? lseq)) lseq?)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (lseq-find
   (lambda ((procedure? pred) (lseq? lseq)) *)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (lseq-find-tail
   (lambda ((procedure? pred) (lseq? lseq)) (or #f lseq?))
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (lseq-take-while
   (lambda ((procedure? pred) (lseq? lseq)) lseq?)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (lseq-drop-while
   (lambda ((procedure? pred) (lseq? lseq)) lseq?)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (lseq-any
   (lambda ((procedure? pred) (lseq? lseq1) (lseq? lseq2) ...) *)
   (pure)
   ((pred (lambda (element) *))))
 
 (lseq-every
   (lambda ((procedure? pred) (lseq? lseq1) (lseq? lseq2) ...) *)
   (pure)
   ((pred (lambda (element) *))))

 (lseq-index
   (lambda ((procedure? pred) (lseq? lseq1) (lseq? lseq2) ...) (or integer? #f))
   (pure)
   ((pred (lambda (element) *))))
 
 (lseq-member
   (lambda (x (lseq? lseq)) (or #f lseq?))
   (pure))
 
 (lseq-member
   (lambda (x (lseq? lseq) (procedure? =)) (or #f lseq?))
   (pure)
   ((= (lambda (a b) boolean?))))
 
 (lseq-memq
   (lambda (x (lseq? lseq)) (or #f lseq?))
   (pure))
 
 (lseq-memv
   (lambda (x (lseq? lseq)) (or #f lseq?))
   (pure))
 
 )
