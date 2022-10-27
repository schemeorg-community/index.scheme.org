(((name . "let*-values")
  (signature syntax-rules () ((_ mv-binding-spec body)))
  (subsigs
   (mv-binding-spec (pattern ((formals1 init1) ...)))
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1)))))
 ((name . "let-values")
  (signature syntax-rules () ((_ mv-binding-spec body)))
  (subsigs
   (mv-binding-spec (pattern ((formals1 init1) ...)))
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))))
