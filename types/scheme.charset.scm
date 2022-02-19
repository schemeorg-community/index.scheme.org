(
 
 (char-set?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (char-set=
   (lambda ((char-set? cs1) ...) boolean?)
   (pure))
 
 (char-set<=
   (lambda ((char-set? cs1) ...) boolean?)
   (pure))
 
 (char-set-hash
   (lambda ((char-set? cs)) integer?)
   (pure))
 
 (char-set-hash
   (lambda ((char-set? cs) (integer? bound)) integer?)
   (pure))
 
 (char-set-cursor
   (lambda ((char-set? cset)) *))
 
 (char-set-ref
   (lambda ((char-set? cset) cursor) char?))
 
 (char-set-cursor-next
   (lambda ((char-set? cset) cursor) *))
 
 (end-of-char-set?
   (lambda (cursor) boolean?))
 
 (char-set-fold
   (lambda ((procedure? kons) knil (char-set? cs)) *)
   (pure)
   ((kons (lambda ((char? c) state) *))))
 
 (char-set-unfold
   (lambda ((procedure? f) (procedure? p) (procedure? g) seed) char-set?)
   (pure)
   ((f (lambda (seed) char?))
    (p (lambda (seed) boolean?))
    (g (lambda (seed) *))))
 
 (char-set-unfold
   (lambda ((procedure? f) (procedure? p) (procedure? g) seed (char-set? base-cs)) char-set?)
   (pure)
   ((f (lambda (seed) char?))
    (p (lambda (seed) boolean?))
    (g (lambda (seed) *))))
 
 (char-set-unfold!
   (lambda ((procedure? f) (procedure? p) (procedure? g) seed (char-set? base-cs)) char-set?)
   (pure)
   ((f (lambda (seed) char?))
    (p (lambda (seed) boolean?))
    (g (lambda (seed) *))))
 
 (char-set-for-each
   (lambda ((procedure? proc) (char-set? cs)) undefined)
   ()
   ((proc (lambda ((char? c)) undefined))))
 
 (char-set-map
   (lambda ((procedure? proc) (char-set? cs)) char-set?)
   (pure)
   ((proc (lambda ((char? c)) char?))))
 
 (char-set-copy
   (lambda ((char-set? cs)) char-set?)
   (pure))
 
 (char-set
   (lambda ((char? char1) ...) char-set?)
   (pure))
 
 (list->char-set
   (lambda ((list? char-list)) char-set?)
   (pure))
 
 (list->char-set
   (lambda ((list? char-list) (char-set? base-cs)) char-set?)
   (pure))
 
 (list->char-set!
   (lambda ((list? char-list) (char-set? base-cs)) char-set?)
   ())
 
 (string->char-set
   (lambda ((string? s)) char-set?)
   (pure))
 
 (string->char-set
   (lambda ((string? s) (char-set? base-cs)) char-set?)
   (pure))
 
 (string->char-set!
   (lambda ((string? s) (char-set? base-cs)) char-set?)
   ())
 
 (char-set-filter
   (lambda ((procedure? pred) (char-set? cs)) char-set?)
   (pure)
   ((pred (lambda ((char? c)) boolean?))))
 
 (char-set-filter
   (lambda ((procedure? pred) (char-set? cs) (char-set? base-cs)) char-set?)
   (pure)
   ((pred (lambda ((char? c)) boolean?))))
 
 (char-set-filter!
   (lambda ((procedure? pred) (char-set? cs) (char-set? base-cs)) char-set?)
   ()
   ((pred (lambda ((char? c)) boolean?))))
 
 (ucs-range->char-set
   (lambda ((integer? lower) (integer? upper)) char-set?)
   (pure))
 
 (ucs-range->char-set
   (lambda ((integer? lower) (integer? upper) (boolean? error?)) char-set?)
   (pure))
 
 (ucs-range->char-set
   (lambda ((integer? lower) (integer? upper) (boolean? error?) (char-set? base-cs)) char-set?)
   (pure))
 
 (ucs-range->char-set!
   (lambda ((integer? lower) (integer? upper) (boolean? error?) (char-set? base-cs)) char-set?)
   ())
 
 (|->char-set|
   (lambda ((string? x)) char-set?)
   (pure))
 
 (|->char-set|
   (lambda ((char? x)) char-set?)
   (pure))
 
 (|->char-set|
   (lambda ((char-set? x)) char-set?)
   (pure))

 (char-set-size
   (lambda ((char-set? cs)) integer?)
   (pure))
 
 (char-set-count
   (lambda ((procedure? pred) (char-set? cs)) integer?)
   (pure)
   ((pred (lambda ((char? c)) boolean?))))
 
 (char-set->list
   (lambda ((char-set? cs)) list?))
 
 (char-set->string
   (lambda ((char-set? cs)) string?))
 
 (char-set-contains?
   (lambda ((char-set? cs) (char? char)) boolean?))
 
 (char-set-every
   (lambda ((procedure? pred) (char-set? cs)) boolean?)
   (pure)
   ((pred (lambda ((char? c)) boolean?))))
 
 (char-set-any
   (lambda ((procedure? pred) (char-set? cs)) boolean?)
   (pure)
   ((pred (lambda ((char? c)) boolean?))))
 
 (char-set-adjoin
   (lambda ((char-set? cs) (char? char1) ...) char-set?)
   (pure))
 
 (char-set-delete
   (lambda ((char-set? cs) (char? char1) ...) char-set?)
   (pure))
 
 (char-set-adjoin!
   (lambda ((char-set? cs) (char? char1) ...) char-set?)
   ())
 
 (char-set-delete!
   (lambda ((char-set? cs) (char? char1) ...) char-set?)
   ())
 
 (char-set-complement
   (lambda ((char-set? cs)) char-set?)
   (pure))
 
 (char-set-union
   (lambda ((char-set? cs1) ...) char-set?)
   (pure))
 
 (char-set-intersection
   (lambda ((char-set? cs1) ...) char-set?)
   (pure))
 
 (char-set-difference
   (lambda ((char-set? cs1) (char-set? cs2) ...) char-set?)
   (pure))
 
 (char-set-xor
   (lambda ((char-set? cs1) ...) char-set?)
   (pure))
 
 (char-set-diff+intersection
   (lambda ((char-set? cs1) (char-set? cs2) ...) (values char-set? char-set?))
   (pure))
 
 (char-set-complement!
   (lambda ((char-set? cs)) char-set?)
   ())
 
 (char-set-union
   (lambda ((char-set? cs1) (char-set? cs2) ...) char-set?)
   ())
 
 (char-set-intersection!
   (lambda ((char-set? cs1) (char-set? cs2) ...) char-set?)
   ())
 
 (char-set-difference!
   (lambda ((char-set? cs1) (char-set? cs2) ...) char-set?)
   ())
 
 (char-set-xor!
   (lambda ((char-set? cs1) (char-set? cs2) ...) char-set?)
   ())
 
 (char-set-diff+intersection!
   (lambda ((char-set? cs1) (char-set? cs2) (char-set? cs3) ...) (values char-set? char-set?))
   ())

 (|char-set:lower-case|
  (value char-set?))
 
 (|char-set:upper-case|
  (value char-set?))
 
 (|char-set:title-case|
  (value char-set?))

 (|char-set:letter|
  (value char-set?))
 
 (|char-set:digit|
  (value char-set?))
 
 (|char-set:letter+digit|
  (value char-set?))

 (|char-set:graphic|
  (value char-set?))
 
 (|char-set:printing|
  (value char-set?))
 
 (|char-set:whitespace|
  (value char-set?))
 
 (|char-set:iso-control|
  (value char-set?))
 
 (|char-set:punctuation|
  (value char-set?))
 
 (|char-set:symbol|
  (value char-set?))
 
 (|char-set:hex-digit|
  (value char-set?))
 
 (|char-set:blank|
  (value char-set?))
 
 (|char-set:ascii|
  (value char-set?))
 
 (|char-set:empty|
  (value char-set?))
 
 (|char-set:full|
  (value char-set?))
 
 )
