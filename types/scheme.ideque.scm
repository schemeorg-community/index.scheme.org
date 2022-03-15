(
 
 (ideque
   (lambda (element ...) ideque?)
   (pure))
 
 (ideque-tabulate
   (lambda ((integer? n) (procedure? proc)) ideque?)
   (pure)
   ((proc (lambda ((integer? k)) *))))
 
 (ideque-unfold
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed) ideque?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) *))
    (successor (lambda (seed) *))))
 
 (ideque-unfold-right
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed) ideque?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) *))
    (successor (lambda (seed) *))))
 
 (ideque?
   (lambda (x) boolean?)
   (pure predicate))
 
 (ideque=
   (lambda ((procedure? elt=) (ideque? ideque) ...) boolean?)
   (pure)
   ((elt= (lambda (a b) boolean?))))
 
 (ideque-any
   (lambda ((procedure? pred) (ideque? ideque)) *)
   (pure)
   ((pred (lambda (element) *))))
 
 (ideque-every
   (lambda ((procedure? pred) (ideque? ideque)) *)
   (pure)
   ((pred (lambda (element) *))))
 
 (ideque-front
   (lambda ((ideque? ideque)) *)
   (pure))
 
 (ideque-back
   (lambda ((ideque? ideque)) *)
   (pure))
 
 (ideque-remove-front
   (lambda ((ideque? ideque)) ideque?)
   (pure))
 
 (ideque-remove-back
   (lambda ((ideque? ideque)) ideque?)
   (pure))
 
 (ideque-add-front
   (lambda ((ideque? ideque) obj) ideque?)
   (pure))
 
 (ideque-add-back
   (lambda ((ideque? ideque) obj) ideque?)
   (pure))
 
 (ideque-ref
   (lambda ((ideque? ideque) (integer? n)) *)
   (pure))
 
 (ideque-take
   (lambda ((ideque? ideque) (integer? n)) ideque?)
   (pure))
 
 (ideque-take-right
   (lambda ((ideque? ideque) (integer? n)) ideque?)
   (pure))
 
 (ideque-drop
   (lambda ((ideque? ideque) (integer? n)) ideque?)
   (pure))
 
 (ideque-drop-right
   (lambda ((ideque? ideque) (integer? n)) ideque?)
   (pure))
 
 (ideque-split-at
   (lambda ((ideque? ideque) (integer? n)) (values ideque? ideque?))
   (pure))
 
 (ideque-length
   (lambda ((ideque? ideque)) integer?)
   (pure))
 
 (ideque-append
   (lambda ((ideque? ideque) ...) ideque?)
   (pure))
 
 (ideque-reverse
   (lambda ((ideque? ideque)) ideque?)
   (pure))
 
 (ideque-count
   (lambda ((procedure? pred) (ideque? ideque)) integer?)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (ideque-zip
   (lambda ((ideque? ideque1) (ideque? ideque2) ...) ideque?)
   (pure))
 
 (ideque-map
   (lambda ((procedure? proc) (ideque? ideque)) ideque?)
   (pure)
   ((proc (lambda (element) *))))
 
 (ideque-filter-map
   (lambda ((procedure? proc) (ideque? ideque)) ideque?)
   (pure)
   ((proc (lambda (element) *))))
 
 (ideque-for-each
   (lambda ((procedure? proc) (ideque? ideque)) undefined)
   ()
   ((proc (lambda (element) undefined))))
 
 (ideque-for-each-right
   (lambda ((procedure? proc) (ideque? ideque)) undefined)
   ()
   ((proc (lambda (element) undefined))))
 
 (ideque-fold
   (lambda ((procedure? proc) nil (ideque? ideque)) *)
   (pure)
   ((proc (lambda (element state) *))))
 
 (ideque-fold-right
   (lambda ((procedure? proc) nil (ideque? ideque)) *)
   (pure)
   ((proc (lambda (element state) *))))
 
 (ideque-append-map
   (lambda ((procedure? proc) (ideque? ideque)) ideque?)
   (pure)
   ((proc (lambda (element) list?))))
 
 (ideque-filter
   (lambda ((procedure? pred) (ideque? ideque)) ideque?)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (ideque-remove
   (lambda ((procedure? pred) (ideque? ideque)) ideque?)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (ideque-partition
   (lambda ((procedure? pred) (ideque? ideque)) (values ideque? ideque?))
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (ideque-find
   (lambda ((procedure? pred) (ideque? ideque)) *)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (ideque-find
   (lambda ((procedure? pred) (ideque? ideque) (procedure? failure)) *)
   (pure)
   ((pred (lambda (element) boolean?))
    (failure (lambda () *))))
 
 (ideque-find-right
   (lambda ((procedure? pred) (ideque? ideque)) *)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (ideque-find-right
   (lambda ((procedure? pred) (ideque? ideque) (procedure? failure)) *)
   (pure)
   ((pred (lambda (element) boolean?))
    (failure (lambda () *))))
 
 (ideque-take-while
   (lambda ((procedure? pred) (ideque? ideque)) ideque?)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (ideque-take-while-right
   (lambda ((procedure? pred) (ideque? ideque)) ideque?)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (ideque-drop-while
   (lambda ((procedure? pred) (ideque? ideque)) ideque?)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (ideque-drop-while-right
   (lambda ((procedure? pred) (ideque? ideque)) ideque?)
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (ideque-span
   (lambda ((procedure? pred) (ideque? ideque)) (values ideque? ideque?))
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (ideque-break
   (lambda ((procedure? pred) (ideque? ideque)) (values ideque? ideque?))
   (pure)
   ((pred (lambda (element) boolean?))))
 
 (list->ideque
   (lambda ((list? list)) ideque?)
   (pure))
 
 (ideque->list
   (lambda ((ideque? ideque)) list?)
   (pure))
 
 (generator->ideque
   (lambda ((procedure? generator)) ideque?)
   ()
   ((generator (lambda () *))))
 
 (ideque->generator
   (lambda ((ideque? ideque)) procedure?)
   ()
   ((return (lambda () *))))
 
 )
