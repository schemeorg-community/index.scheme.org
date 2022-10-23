(((name . dynamic-extent?)
  (signature lambda (obj) boolean?)
  (tags pure predicate))
 ((name . current-dynamic-extent)
  (signature lambda () dynamic-extent?))
 ((name . with-dynamic-extent)
  (signature lambda ((dynamic-extent? dynamic-extent) (procedure? thunk)) *)
  (tags pure)
  (subsigs
    (thunk (lambda () *))))
 ((name . dynamic-lambda)
  (signature syntax-rules () ((_ formals body) procedure?))
  (subsigs
    (formals
      (variable1 ...)
      variable
      (variable1 ... variable_n . variable_n+1)))))
