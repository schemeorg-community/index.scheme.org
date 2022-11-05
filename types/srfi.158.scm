(((name . "generator")
  (signature lambda (arg ...) procedure?)
  (subsigs (return (lambda () *))))
 ((name . "make-iota-generator")
  (signature
   case-lambda
   (((integer? count)) procedure?)
   (((integer? count) (real? start)) procedure?)
   (((integer? count) (real? start) (real? step)) procedure?))
  (subsigs (return (lambda () (or real? eof-object?)))))
 ((name . "make-range-generator")
  (signature
   case-lambda
   (((real? start)) procedure?)
   (((real? start) (real? end)) procedure?)
   (((real? start) (real? end) (real? step)) procedure?))
  (subsigs (return (lambda () (or real? eof-object?)))))
 ((name . "make-coroutine-generator")
  (signature lambda ((procedure? proc)) procedure?)
  (subsigs
   (proc (lambda ((procedure? yield)) undefined))
   (yield (lambda (value) undefined))
   (return (lambda () *))))
 ((name . "list->generator")
  (signature lambda ((list? lis)) procedure?)
  (subsigs (return (lambda () *))))
 ((name . "vector->generator")
  (signature
   case-lambda
   (((vector? vec)) procedure?)
   (((vector? vec) (integer? start)) procedure?)
   (((vector? vec) (integer? start) (integer? end)) procedure?))
  (subsigs (return (lambda () *))))
 ((name . "reverse-vector->generator")
  (signature
   case-lambda
   (((vector? vec)) procedure?)
   (((vector? vec) (integer? start)) procedure?)
   (((vector? vec) (integer? start) (integer? end)) procedure?))
  (subsigs (return (lambda () *))))
 ((name . "string->generator")
  (signature
   case-lambda
   (((string? str)) procedure?)
   (((string? str) (integer? start)) procedure?)
   (((string? str) (integer? start) (integer? end)) procedure?))
  (subsigs (return (lambda () (or char? eof-object?)))))
 ((name . "bytevector->generator")
  (signature
   case-lambda
   (((bytevector? bytevector)) procedure?)
   (((bytevector? bytevector) (integer? start)) procedure?)
   (((bytevector? bytevector) (integer? start) (integer? end)) procedure?))
  (subsigs (return (lambda () (or integer? eof-object?)))))
 ((name . "make-for-each-generator")
  (signature lambda ((procedure? for-each) obj) procedure?)
  (subsigs (for-each (lambda (element) undefined)) (return (lambda () *))))
 ((name . "make-unfold-generator")
  (signature
   lambda
   ((procedure? stop?) (procedure? mapper) (procedure? successor) seed)
   procedure?)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) *))
   (successor (lambda (seed) *))
   (return (lambda () *))))
 ((name . "gcons*")
  (signature lambda (item ... (procedure? gen)) procedure?)
  (subsigs (gen (lambda () *)) (return (lambda () *))))
 ((name . "gappend")
  (signature lambda ((procedure? gen) ...) procedure?)
  (subsigs (gen (lambda () *)) (return (lambda () *))))
 ((name . "gcombine")
  (signature
   lambda
   ((procedure? proc) seed (procedure? gen1) (procedure? gen2) ...)
   procedure?)
  (subsigs
   (proc (lambda (value1 value2 ... seed) (values * *)))
   (gen (lambda () *))
   (return (lambda () *))))
 ((name . "gfilter")
  (signature lambda ((procedure? pred) (procedure? gen)) procedure?)
  (subsigs
   (pred (lambda (element) boolean?))
   (gen (lambda () *))
   (return (lambda () *))))
 ((name . "gremove")
  (signature lambda ((procedure? pred) (procedure? gen)) procedure?)
  (subsigs
   (pred (lambda (element) boolean?))
   (gen (lambda () *))
   (return (lambda () *))))
 ((name . "gtake")
  (signature
   case-lambda
   (((procedure? gen) (integer? k)) procedure?)
   (((procedure? gen) (integer? k) padding) procedure?))
  (subsigs (gen (lambda () *)) (return (lambda () *))))
 ((name . "gdrop")
  (signature lambda ((procedure? gen) (integer? k)) procedure?)
  (subsigs (gen (lambda () *)) (return (lambda () *))))
 ((name . "gtake-while")
  (signature lambda ((procedure? pred) (procedure? gen)) procedure?)
  (subsigs
   (pred (lambda (element) boolean?))
   (gen (lambda () *))
   (return (lambda () *))))
 ((name . "gdrop-while")
  (signature lambda ((procedure? pred) (procedure? gen)) procedure?)
  (subsigs
   (pred (lambda (element) boolean?))
   (gen (lambda () *))
   (return (lambda () *))))
 ((name . "gdelete")
  (signature
   case-lambda
   ((item (procedure? gen)) procedure?)
   ((item (procedure? gen) (procedure? =)) procedure?))
  (subsigs
   (gen (lambda () *))
   (= (lambda (a b) boolean?))
   (return (lambda () *))))
 ((name . "gdelete-neighbor-dups")
  (signature
   case-lambda
   (((procedure? gen)) procedure?)
   (((procedure? gen) (procedure? =)) procedure?))
  (subsigs
   (gen (lambda () *))
   (= (lambda (a b) boolean?))
   (return (lambda () *))))
 ((name . "gindex")
  (signature lambda ((procedure? value-gen) (procedure? index-gen)) procedure?)
  (subsigs
   (value-gen (lambda () *))
   (index-gen (lambda () (or integer? eof-object?)))
   (return (lambda () *))))
 ((name . "gselect")
  (signature lambda ((procedure? value-gen) (procedure? truth-gen)) procedure?)
  (subsigs
   (value-gen (lambda () *))
   (truth-gen (lambda () (or boolean? eof-object?)))
   (return (lambda () *))))
 ((name . "generator->list")
  (signature
   case-lambda
   (((procedure? generator)) list?)
   (((procedure? generator) (integer? k)) list?))
  (subsigs (generator (lambda () *))))
 ((name . "generator->reverse-list")
  (signature
   case-lambda
   (((procedure? generator)) list?)
   (((procedure? generator) (integer? k)) list?))
  (subsigs (generator (lambda () *))))
 ((name . "generator->vector")
  (signature
   case-lambda
   (((procedure? generator)) vector?)
   (((procedure? generator) (integer? k)) vector?))
  (subsigs (generator (lambda () *))))
 ((name . "generator->vector!")
  (signature
   lambda
   ((vector? vector) (integer? at) (procedure? generator))
   integer?)
  (subsigs (generator (lambda () *))))
 ((name . "generator->string")
  (signature
   case-lambda
   (((procedure? generator)) string?)
   (((procedure? generator) (integer? k)) string?))
  (subsigs (generator (lambda () (or char? eof-object?)))))
 ((name . "generator-fold")
  (signature
   lambda
   ((procedure? proc) seed (procedure? gen1) (procedure? gen2) ...)
   procedure?)
  (subsigs (proc (lambda (val1 val2 ... state) *)) (gen (lambda () *))))
 ((name . "generator-for-each")
  (signature
   lambda
   ((procedure? proc) (procedure? gen1) (procedure? gen2) ...)
   undefined)
  (subsigs (proc (lambda (val1 val2 ...) undefined)) (gen (lambda () *))))
 ((name . "generator-find")
  (signature lambda ((procedure? pred) (procedure? generator)) *)
  (subsigs (pred (lambda (element) boolean?)) (generator (lambda () *))))
 ((name . "generator-count")
  (signature lambda ((procedure? pred) (procedure? generator)) integer?)
  (subsigs (pred (lambda (element) boolean?)) (generator (lambda () *))))
 ((name . "generator-any")
  (signature lambda ((procedure? pred) (procedure? generator)) *)
  (subsigs (pred (lambda (element) boolean?)) (generator (lambda () *))))
 ((name . "generator-every")
  (signature lambda ((procedure? pred) (procedure? generator)) *)
  (subsigs (pred (lambda (element) boolean?)) (generator (lambda () *))))
 ((name . "generator-unfold")
  (signature lambda ((procedure? gen) (procedure? unfold) arg ...) *)
  (subsigs
   (gen (lambda () *))
   (unfold
    (lambda ((procedure? stop?)
             (procedure? mapper)
             (procedure? successor)
             seed
             args
             ...)
      *))
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) *))
   (successor (lambda (seed) *)))))
