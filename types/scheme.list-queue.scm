(
 
 (make-list-queue
   (lambda ((list? list)) list-queue?)
   (pure))
 
 (make-list-queue
   (lambda ((list? list) (pair? last)) list-queue?)
   (pure))
 
 (list-queue
   (lambda (element ...) list-queue?)
   (pure))
 
 (list-queue
   (lambda ((list-queue? list-queue)) list-queue?)
   (pure))
 
 (list-queue-unfold
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed) list-queue?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) *))
    (successor (lambda (seed) *))))
 
 (list-queue-unfold
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed (list-queue? queue)) list-queue?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) *))
    (successor (lambda (seed) *))))
 
 (list-queue-unfold-right
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed) list-queue?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) *))
    (successor (lambda (seed) *))))
 
 (list-queue-unfold-right
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed (list-queue? queue)) list-queue?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) *))
    (successor (lambda (seed) *))))
 
 (list-queue?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (list-queue-empty?
   (lambda ((list-queue? list-queue)) boolean?)
   (pure))
 
 (list-queue-front
   (lambda ((list-queue? list-queue)) *)
   (pure))
 
 (list-queue-back
   (lambda ((list-queue? list-queue)) *)
   (pure))
 
 (list-queue-list
   (lambda ((list-queue? list-queue)) list?)
   (pure))
 
 (list-queue-first-last
   (lambda ((list-queue? list-queue)) (values list? list?))
   (pure))
 
 (list-queue-add-front!
   (lambda ((list-queue? list-queue) element) undefined)
   ())
 
 (list-queue-add-back!
   (lambda ((list-queue? list-queue) element) undefined)
   ())
 
 (list-queue-remove-front!
   (lambda ((list-queue? list-queue)) *)
   ())
 
 (list-queue-remove-back!
   (lambda ((list-queue? list-queue)) *)
   ())
 
 (list-queue-remove-all!
   (lambda ((list-queue? list-queue)) list?)
   ())
 
 (list-queue-set-list!
   (lambda ((list-queue? list-queue) (list? list)) undefined)
   ())
 
 (list-queue-set-list!
   (lambda ((list-queue? list-queue) (list? list) (pair? last)) undefined)
   ())
 
 (list-queue-append
   (lambda ((list-queue? list-queue) ...) list-queue?)
   (pure))
 
 (list-queue-append!
   (lambda ((list-queue? list-queue) ...) list-queue?)
   ())
 
 (list-queue-concatenate
   (lambda ((list? list-of-list-queues)) list-queue?)
   (pure))
 
 (list-queue-map
   (lambda ((procedure? proc) (list-queue? list-queue)) list-queue?)
   (pure)
   ((proc (lambda (element) *))))
 
 (list-queue-map!
   (lambda ((procedure? proc) (list-queue? list-queue)) undefined)
   ()
   ((proc (lambda (element) *))))
 
 (list-queue-for-each
   (lambda ((procedure? proc) (list-queue? list-queue)) undefined)
   ()
   ((proc (lambda (element) undefined))))
 
 )
