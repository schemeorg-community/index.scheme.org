(
 
 (rquote
   (syntax-rules ()
     ((_ datum))))
 
 (requal?
   (lambda (obj1 obj2) boolean?)
   (pure))
 
 (rpair?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (rcons
   (lambda (obj1 obj2) rpair?)
   (pure))
 
 (rcar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcdr
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcaar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcadr
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcdar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcddr
   (lambda ((rpair? pair)) *)
   (pure))

 (rcaaar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcaadr
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcadar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcaddr
   (lambda ((rpair? pair)) *)
   (pure))

 (rcdaar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcdadr
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcddar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcdddr
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcaaaar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcaaadr
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcaadar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcaaddr
   (lambda ((rpair? pair)) *)
   (pure))

 (rcadaar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcadadr
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcaddar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcadddr
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcdaaar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcdaadr
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcdadar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcdaddr
   (lambda ((rpair? pair)) *)
   (pure))

 (rcddaar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcddadr
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcdddar
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rcddddr
   (lambda ((rpair? pair)) *)
   (pure))
 
 (rnull?
   (lambda (obj) boolean?)
   (pure))
 
 (rlist?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (rpair?))
 
 (rlist
   (lambda (obj ...) rlist?)
   (pure))
 
 (make-rlist
   (lambda ((integer? k)) rlist?))
 
 (make-rlist
   (lambda ((integer? k) obj) rlist?)
   (pure))
 
 (rlength
   (lambda ((rlist? list)) integer?)
   (pure))
 
 (rlength<=?
   (lambda (obj (integer? k)) boolean?)
   (pure))
 
 (rappend
   (lambda ((rlist? list) ...) rlist?)
   (pure))
 
 (rappend
   (lambda ((rlist? list) ... obj) *)
   (pure))
 
 (rreverse
   (lambda ((rlist? list)) rlist?)
   (pure))
 
 (rlist-tail
   (lambda ((rlist? list) (integer? k)) rlist?)
   (pure))
 
 (rlist-tail
   (lambda (obj (integer? k)) *)
   (pure))

 (rlist-ref
   (lambda ((rpair? pair) (integer? k)) *)
   (pure))

 (rlist-set
   (lambda ((rpair? pair) (integer? k) obj) rpair?)
   (pure))

 (rlist-ref/update
   (lambda ((rpair? pair) (integer? k) (procedure? proc)) (values * rpair?))
   (pure)
   ((proc (lambda (obj) *))))
 
 (rmap
   (lambda ((procedure? proc) (rlist? list1) (rlist? list2) ...) rlist?)
   (pure)
   ((proc (lambda (obj1 obj2 ...) *))))
 
 (rfor-each
   (lambda ((procedure? proc) (rlist? list1) (rlist? list2) ...) undefined)
   (pure)
   ((proc (lambda (obj1 obj2 ...) undefined))))
 
 (rlist->list
   (lambda ((rlist? rlist)) list?)
   (pure))
 
 (list->rlist
   (lambda ((list? list)) rlist?)
   (pure))
 
 )
