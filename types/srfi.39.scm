(((name . "make-parameter")
  (signature
   case-lambda
   ((obj) procedure?)
   ((obj (procedure? converter)) procedure?))
  (subsigs (converter (lambda (obj) *)))
  (tags pure)
  (desc . "Returns a newly allocated parameter object, which is a procedure that accepts zero arguments and returns the value associated with the parameter object. Initially, this value is the value of (converter init), or of init if the conversion procedure converter is not specified. The associated value can be temporarily changed using parameterize, which is described below. The effect of passing arguments to a parameter object is implementation-dependent."))
 ((name . "parameterize")
  (signature syntax-rules () ((_ ((param1 value1) ...) body)))
  (desc . "A parameterize expression is used to change the values returned by specified parameter objects during the evaluation of the body. The <param> and <value> expressions are evaluated in an unspecified order. The <body> is evaluated in a dynamic environment in which calls to the parameters return the results of passing the corresponding values to the conversion procedure specified when the parameters were created. Then the previous values of the parameters are restored without passing them to the conversion procedure. The results of the last expression in the <body> are returned as the results of the entire parameterize expression. Note: If the conversion procedure is not idempotent, the results of (parameterize ((x (x))) ...), which appears to bind the parameter x to its current value, might not be what the user expects. If an implementation supports multiple threads of execution, then parameterize must not change the associated values of any parameters in any thread other than the current thread and threads created inside <body>. Parameter objects can be used to specify configurable settings for a computation without the need to pass the value to every procedure in the call chain explicitly.")))
