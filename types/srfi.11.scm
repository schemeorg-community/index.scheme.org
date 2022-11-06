(((name . "let*-values")
  (signature syntax-rules () ((_ mv-binding-spec body)))
  (subsigs
   (mv-binding-spec (pattern ((formals1 init1) ...)))
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))
  (desc . "The let*-values construct is similar to let-values, but the <init>s are evaluated and bindings created sequentially from left to right, with the region of the bindings of each <formals> including the <init>s to its right as well as <body>. Thus the second <init> is evaluated in an environment in which the first set of bindings is visible and initialized, and so on."))
 ((name . "let-values")
  (signature syntax-rules () ((_ mv-binding-spec body)))
  (subsigs
   (mv-binding-spec (pattern ((formals1 init1) ...)))
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))
  (desc . "The <init>s are evaluated in the current environment (in some unspecified order) as if by invoking call-with-values, and the variables occurring in the <formals> are bound to fresh locations holding the values returned by the <init>s, where the <formals> are matched to the return values in the same way that the <formals> in a lambda expression are matched to the arguments in a procedure call. Then, the <body> is evaluated in the extended environment, and the values of the last expression of <body> are returned. Each binding of a <variable> has <body> as its region. It is an error if the <formals> do not match the number of values returned by the corresponding <init>.")))
