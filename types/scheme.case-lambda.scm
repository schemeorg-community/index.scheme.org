(
 (case-lambda
   (syntax-rules ()
     ((_ clause ...)))
   ()
   ((clause (formals body))
    (formals (variable1 ...)
             variable
             (variable1 ... variable_n . variable_n+1))))
 )
