(((name . "dynamic-extent?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "The dynamic-extent? procedure returns #t if its argument is a dynamic extent, and #f otherwise. Note that dynamic extents are not necessarily disjoint from other Scheme types such as procedures."))
 ((name . "current-dynamic-extent") 
  (signature lambda () dynamic-extent?)
  (desc . "The current-dynamic-extent procedure returns the current dynamic extent by capturing the dynamic extent of the call to current-dynamic-extent, which can be reinstated by the procedure with-dynamic-extent."))
 ((name . "with-dynamic-extent")
  (signature lambda ((dynamic-extent? dynamic-extent) (procedure? thunk)) *)
  (subsigs (thunk (lambda () *)))
  (tags pure)
  (desc . "The with-dynamic-extent procedure calls the thunk and returns the values yielded by thunk. The call to thunk happens in the dynamic extent captured by the dynamic-extent."))
 ((name . "dynamic-lambda")
  (signature syntax-rules () ((_ formals body) procedure?))
  (subsigs
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))
  (desc . "A dynamic-lambda expression is equivalent to lambda expression except that not only the environment but also the dynamic extent in effect when the dynamic-lambda expression was evaluated is remembered. In other words, the expression (dynamic-lambda <formals> <body>) closes also over the dynamic extent.")))
