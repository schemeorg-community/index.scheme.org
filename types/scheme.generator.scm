(
 (generator
   (lambda (arg ...) procedure?)
   ()
   ((return (lambda () *))))
 
 (make-iota-generator
   (lambda ((integer? count)) procedure?)
   ()
   ((return (lambda () (or integer? eof-object?)))))
 
 (make-iota-generator
   (lambda ((integer? count) (real? start)) procedure?)
   ()
   ((return (lambda () (or real? eof-object?)))))
 
 (make-iota-generator
   (lambda ((integer? count) (real? start) (real? step)) procedure?)
   ()
   ((return (lambda () (or real? eof-object?)))))
 
 (make-range-generator
   (lambda ((real? start)) procedure?)
   ()
   ((return (lambda () real?))))
 
 (make-range-generator
   (lambda ((real? start) (real? end)) procedure?)
   ()
   ((return (lambda () (or real? eof-object?)))))
 
 (make-range-generator
   (lambda ((real? start) (real? end) (real? step)) procedure?)
   ()
   ((return (lambda () (or real? eof-object?)))))
 
 (make-coroutine-generator
   (lambda ((procedure? proc)) procedure?)
   ()
   ((proc (lambda ((procedure? yield)) undefined))
    (yield (lambda (value) undefined))
    (return (lambda () *))))
 
 (list->generator
   (lambda ((list? lis)) procedure?)
   ()
   ((return (lambda () *))))
 
 (vector->generator
   (lambda ((vector? vec)) procedure?)
   ()
   ((return (lambda () *))))
 
 (vector->generator
   (lambda ((vector? vec) (integer? start)) procedure?)
   ()
   ((return (lambda () *))))
 
 (vector->generator
   (lambda ((vector? vec) (integer? start) (integer? end)) procedure?)
   ()
   ((return (lambda () *))))
 
 (reverse-vector->generator
   (lambda ((vector? vec)) procedure?)
   ()
   ((return (lambda () *))))
 
 (reverse-vector->generator
   (lambda ((vector? vec) (integer? start)) procedure?)
   ()
   ((return (lambda () *))))
 
 (reverse-vector->generator
   (lambda ((vector? vec) (integer? start) (integer? end)) procedure?)
   ()
   ((return (lambda () *))))
 
 (string->generator
   (lambda ((string? str)) procedure?)
   ()
   ((return (lambda () (or char? eof-object?)))))
 
 (string->generator
   (lambda ((string? str) (integer? start)) procedure?)
   ()
   ((return (lambda () (or char? eof-object?)))))
 
 (string->generator
   (lambda ((string? str) (integer? start) (integer? end)) procedure?)
   ()
   ((return (lambda () (or char? eof-object?)))))
 
 (bytevector->generator
   (lambda ((bytevector? bytevector)) procedure?)
   ()
   ((return (lambda () (or integer? eof-object?)))))
 
 (bytevector->generator
   (lambda ((bytevector? bytevector) (integer? start)) procedure?)
   ()
   ((return (lambda () (or integer? eof-object?)))))
 
 (bytevector->generator
   (lambda ((bytevector? bytevector) (integer? start) (integer? end)) procedure?)
   ()
   ((return (lambda () (or integer? eof-object?)))))
 
 (make-for-each-generator
   (lambda ((procedure? for-each) obj) procedure?)
   ()
   ((for-each (lambda (element) undefined))
    (return (lambda () *))))
 
 (make-unfold-generator
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed) procedure?)
   ()
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) *))
    (successor (lambda (seed) *))
    (return (lambda () *))))
 
 (gcons*
   (lambda (item ... (procedure? gen)) procedure?)
   ()
   ((gen (lambda () *))
    (return (lambda () *))))
 
 (gappend
   (lambda ((procedure? gen) ...) procedure?)
   ()
   ((gen (lambda () *))
    (return (lambda () *))))
 
 (gcombine
   (lambda ((procedure? proc) seed (procedure? gen1) (procedure? gen2) ...) procedure?)
   ()
   ((proc (lambda (value1 value2 ... seed) (values * *)))
    (gen (lambda () *))
    (return (lambda () *))))
 
 (gfilter
   (lambda ((procedure? pred) (procedure? gen)) procedure?)
   ()
   ((pred (lambda (element) boolean?))
    (gen (lambda () *))
    (return (lambda () *))))
 
 (gremove
   (lambda ((procedure? pred) (procedure? gen)) procedure?)
   ()
   ((pred (lambda (element) boolean?))
    (gen (lambda () *))
    (return (lambda () *))))
 
 (gtake
   (lambda ((procedure? gen) (integer? k)) procedure?)
   ()
   ((gen (lambda () *))
    (return (lambda () *))))
 
 (gtake
   (lambda ((procedure? gen) (integer? k) padding) procedure?)
   ()
   ((gen (lambda () *))
    (return (lambda () *))))
 
 (gdrop
   (lambda ((procedure? gen) (integer? k)) procedure?)
   ()
   ((gen (lambda () *))
    (return (lambda () *))))
 
 (gtake-while
   (lambda ((procedure? pred) (procedure? gen)) procedure?)
   ()
   ((pred (lambda (element) boolean?))
    (gen (lambda () *))
    (return (lambda () *))))
 
 (gdrop-while
   (lambda ((procedure? pred) (procedure? gen)) procedure?)
   ()
   ((pred (lambda (element) boolean?))
    (gen (lambda () *))
    (return (lambda () *))))
 
 (gdelete
   (lambda (item (procedure? gen)) procedure?)
   ()
   ((gen (lambda () *))
    (return (lambda () *))))
 
 (gdelete
   (lambda (item (procedure? gen) (procedure? =)) procedure?)
   ()
   ((gen (lambda () *))
    (= (lambda (a b) boolean?))
    (return (lambda () *))))
 
 (gdelete-neighbor-dups
   (lambda ((procedure? gen)) procedure?)
   ()
   ((gen (lambda () *))
    (return (lambda () *))))
 
 (gdelete-neighbor-dups
   (lambda ((procedure? gen) (procedure? =)) procedure?)
   ()
   ((gen (lambda () *))
    (= (lambda (a b) boolean?))
    (return (lambda () *))))
 
 (gindex
   (lambda ((procedure? value-gen) (procedure? index-gen)) procedure?)
   ()
   ((value-gen (lambda () *))
    (index-gen (lambda () (or integer? eof-object?)))
    (return (lambda () *))))
 
 (gselect
   (lambda ((procedure? value-gen) (procedure? truth-gen)) procedure?)
   ()
   ((value-gen (lambda () *))
    (truth-gen (lambda () (or boolean? eof-object?)))
    (return (lambda () *))))
 
 (generator->list
   (lambda ((procedure? generator)) list?)
   ()
   ((generator (lambda () *))))
 
 (generator->list
   (lambda ((procedure? generator) (integer? k)) list?)
   ()
   ((generator (lambda () *))))
 
 (generator->reverse-list
   (lambda ((procedure? generator)) list?)
   ()
   ((generator (lambda () *))))
 
 (generator->reverse-list
   (lambda ((procedure? generator) (integer? k)) list?)
   ()
   ((generator (lambda () *))))
 
 (generator->vector
   (lambda ((procedure? generator)) vector?)
   ()
   ((generator (lambda () *))))
 
 (generator->vector
   (lambda ((procedure? generator) (integer? k)) vector?)
   ()
   ((generator (lambda () *))))
 
 (generator->vector!
   (lambda ((vector? vector) (integer? at) (procedure? generator)) integer?)
   ()
   ((generator (lambda () *))))
 
 (generator->string
   (lambda ((procedure? generator)) string?)
   ()
   ((generator (lambda () (or char? eof-object?)))))
 
 (generator->string
   (lambda ((procedure? generator) (integer? k)) string?)
   ()
   ((generator (lambda () (or char? eof-object?)))))
 
 (generator-fold
   (lambda ((procedure? proc) seed (procedure? gen1) (procedure? gen2) ...) procedure?)
   ()
   ((proc (lambda (val1 val2 ... state) *))
    (gen (lambda () *))))
 
 (generator-for-each
   (lambda ((procedure? proc) (procedure? gen1) (procedure? gen2) ...) undefined)
   ()
   ((proc (lambda (val1 val2 ...) undefined))
    (gen (lambda () *))))
 
 (generator-find
   (lambda ((procedure? pred) (procedure? generator)) *)
   ()
   ((pred (lambda (element) boolean?))
    (generator (lambda () *))))
 
 (generator-count
   (lambda ((procedure? pred) (procedure? generator)) integer?)
   ()
   ((pred (lambda (element) boolean?))
    (generator (lambda () *))))
 
 (generator-any
   (lambda ((procedure? pred) (procedure? generator)) *)
   ()
   ((pred (lambda (element) boolean?))
    (generator (lambda () *))))
 
 (generator-every
   (lambda ((procedure? pred) (procedure? generator)) *)
   ()
   ((pred (lambda (element) boolean?))
    (generator (lambda () *))))
 
 (generator-unfold
   (lambda ((procedure? gen) (procedure? unfold) arg ...) *)
   ()
   ((gen (lambda () *))
    (unfold (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed args ...) *))
    (stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) *))
    (successor (lambda (seed) *))))
 
 )
