(((name . "case-lambda")
  (signature syntax-rules () ((_ clause ...) procedure?))
  (subsigs
   (clause (pattern (formals body)))
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))
  (desc . "A CASE-LAMBDA expression evaluates to a procedure that accepts a variable number of arguments and is lexically scoped in the same manner as procedures resulting from LAMBDA expressions. When the procedure is called with some arguments V1 .. Vk, then the first <clause> for which the arguments agree with <formals> is selected, where agreement is specified as for the <formals> of a LAMBDA expression. The variables of <formals> are bound to fresh locations, the values V1 .. Vk are stored in those locations, the <body> is evaluated in the extended environment, and the results of <body> are returned as the results of the procedure call. It is an error for the arguments not to agree with the <formals> of any <clause>.")))
