(((name . case-lambda)
  (signature syntax-rules () ((_ clause ...)))
  (subsigs
   (clause (formals body))
   (formals
    (variable1 ...)
    variable
    (variable1 ... variable_n . variable_n+1)))))
