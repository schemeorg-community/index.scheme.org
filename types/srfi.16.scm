(((name . case-lambda)
  (signature syntax-rules () ((_ clause ...) procedure?))
  (subsigs
   (clause (formals body))
   (formals
    (variable1 ...)
    variable
    (variable1 ... variable_n . variable_n+1)))))
