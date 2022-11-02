(((name . "case-lambda")
  (signature syntax-rules () ((_ clause ...) procedure?))
  (subsigs
   (clause (pattern (formals body)))
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))))
