(((name . let*-values)
  (signature syntax-rules () ((_ mv-binding-spec body)))
  (subsigs
    (mv-binding-spec ((formals1 init1) ...))
    (formals
      (variable1 ...)
      variable
      (variable1 ... variable_n . variable_n+1))))
 ((name . let-values)
  (signature syntax-rules () ((_ mv-binding-spec body)))
  (subsigs
    (mv-binding-spec ((formals1 init1) ...))
    (formals
      (variable1 ...)
      variable
      (variable1 ... variable_n . variable_n+1)))))
