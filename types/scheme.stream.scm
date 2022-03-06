(
 
 (stream-null
   (value stream-null?)
   ())
 
 (stream-cons
   (lambda (obj (stream? stream)) stream-pair?)
   (pure syntax))
 
 (stream?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (stream-null?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (stream?))
 
 (stream-pair?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (stream?))
 
 (stream-car
   (lambda ((stream-pair? stream)) *)
   (pure))
 
 (stream-cdr
   (lambda ((stream-pair? stream)) stream?)
   (pure))

 (stream-lambda
   (syntax-rules ()
     ((_ formals body)))
   ()
   ((formals (variable1 ...)
             variable
             (variable1 ... variable_n . variable_n+1))))
 
 (define-stream
   (syntax-rules ()
     ((_ (variable parameter1 ...) body))
     ((_ (variable parameter1 ... . parameter) body)))
   ())
 
 (list->stream
   (lambda ((list? list-of-objects)) stream?)
   (pure))
 
 (port->stream
   (lambda () stream?)
   (parameterized))
 
 (port->stream
   (lambda ((input-port? port)) stream?))
 
 (stream
   (lambda (object ...) stream?)
   (pure syntax))
 
 (stream->list
   (lambda ((stream? stream)) list?)
   (pure))
 
 (stream->list
   (lambda ((integer? n) (stream? stream)) list?)
   (pure))
 
 (stream-append
   (lambda ((stream? stream) ...) stream?)
   (pure))
 
 (stream-concat
   (lambda ((stream? stream-of-streams)) stream?)
   (pure))
 
 (stream-constant
   (lambda (object ...) stream?)
   (pure))
 
 (stream-drop
   (lambda ((integer? n) (stream? stream)) stream?)
   (pure))
 
 (stream-drop-while
   (lambda ((procedure? pred?) (stream? stream)) stream?)
   (pure)
   ((pred? (lambda (obj) boolean?))))
 
 (stream-filter
   (lambda ((procedure? pred?) (stream? stream)) stream?)
   (pure)
   ((pred? (lambda (obj) boolean?))))
 
 (stream-fold
   (lambda ((procedure? proc) base (stream? stream)) *)
   (pure)
   ((proc (lambda (base element) *))))
 
 (stream-for-each
   (lambda ((procedure? proc) (stream? stream1) ...) undefined)
   ()
   ((proc (lambda (element1 ...) undefined))))
 
 (stream-from
   (lambda ((number? from)) stream?)
   (pure))
 
 (stream-from
   (lambda ((number? from) (number? step)) stream?)
   (pure))
 
 (stream-iterate
   (lambda ((procedure? proc) base) stream?)
   (pure)
   ((proc (lambda (element) *))))
 
 (stream-length
   (lambda ((stream? stream)) integer?)
   (pure))
 
 (stream-let
   (syntax-rules ()
     ((_ tag ((var expr) ...) body))))
 
 (stream-map
   (lambda ((procedure? proc) (stream? stream1) ...) stream?)
   ()
   ((proc (lambda (element1 ...) *))))
 
 (stream-match
   (syntax-rules (_)
     ((_ stream clause ...)))
   ()
   ((clause ()
            (pat0 pat1 ...)
            (pat0 pat1 ... . pat_rest)
            pat)
    (pat identifier
         _)))
 
 (stream-of
   (syntax-rules (in is)
     ((_ expr clause ...)))
   ()
   ((clause (var in stream-expr)
            (var is expr)
            (pred? expr)
            )))
 
 (stream-range
   (lambda ((real? first) (real? past)) stream?)
   (pure))
 
 (stream-range
   (lambda ((real? first) (real? past) (real? step)) stream?)
   (pure))
 
 (stream-ref
   (lambda ((stream? stream) (integer? n)) *)
   (pure))
 
 (stream-reverse
   (lambda ((stream? stream)) stream?)
   (pure))
 
 (stream-scan
   (lambda ((procedure? proc) base (stream? stream)) stream?)
   (pure)
   ((proc (lambda (base element) *))))
 
 (stream-take
   (lambda ((integer? n) (stream? stream)) stream?)
   (pure))
 
 (stream-take-while
   (lambda ((procedure? pred?) (stream? stream)) stream?)
   (pure)
   ((pred? (lambda (obj) boolean?))))
 
 (stream-unfold
   (lambda ((procedure? map) (procedure? pred?) (procedure? gen) base) stream?)
   (pure)
   ((map (lambda (base) *))
    (pred? (lambda (base) boolean?))
    (gen (lambda (base) *))))
 
 (stream-unfolds
   (lambda ((procedure? proc) seed) (values stream? ...))
   (pure)
   ((proc (lambda (seed) (values (or list? #f) * ...)))))
 
 (stream-zip
   (lambda ((stream? stream) ...) stream?)
   (pure))
 
 )
