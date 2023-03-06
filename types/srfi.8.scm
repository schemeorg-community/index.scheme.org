(((name . "receive")
  (signature syntax-rules () ((_ formals expression body)))
  (subsigs
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))
  (desc . "<Formals>, <expression>, and <body> are as described in R5RS. Specifically, <formals> can have any of three forms:
* (<variable1> ... <variablen>): The environment in which the receive-expression is evaluated is extended by binding <variable1>, ..., <variablen> to fresh locations. The <expression> is evaluated, and its values are stored into those locations. (It is an error if <expression> does not have exactly n values.)
* <variable>: The environment in which the receive-expression is evaluated is extended by binding <variable> to a fresh location. The <expression> is evaluated, its values are converted into a newly allocated list, and the list is stored in the location bound to <variable>.
* (<variable1> ... <variablen> . <variablen + 1>): The environment in which the receive-expression is evaluated is extended by binding <variable1>, ..., <variablen + 1> to fresh locations. The <expression> is evaluated. Its first n values are stored into the locations bound to <variable1> ... <variablen>. Any remaining values are converted into a newly allocated list, which is stored into the location bound to <variablen + 1>. (It is an error if <expression> does not have at least n values.)")))
