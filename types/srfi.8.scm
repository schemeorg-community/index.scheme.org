(((name . "receive")
  (signature syntax-rules () ((_ formals expression body)))
  (subsigs
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))))
