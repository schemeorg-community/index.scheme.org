(((name . "cut")
  (signature
   syntax-rules
   (<> <...>)
   ((_ slot-or-expr slot-or-expr ...) procedure?)
   ((_ slot-or-expr slot-or-expr ... <...>) procedure?))
  (subsigs (slot-or-expr (pattern <> expression)))
  (desc . "The macro cut transforms a <cut-expression> into a <lambda expression> with as many formal variables as there are slots in the list <slot-or-expr>*. The body of the resulting <lambda expression> calls the first <slot-or-expr> with arguments from <slot-or-expr>* in the order they appear. In case there is a rest-slot symbol, the resulting procedure is also of variable arity, and the body calls the first <slot-or-expr> with all arguments provided to the actual call of the specialized procedure."))
 ((name . "cute")
  (signature
   syntax-rules
   (<> <...>)
   ((_ slot-or-expr slot-or-expr ...) procedure?)
   ((_ slot-or-expr slot-or-expr ... <...>) procedure?))
  (subsigs (slot-or-expr (pattern <> expression)))
  (desc . "The macro cute is similar to the macro cut, except that it first binds new variables to the result of evaluating the non-slot expressions (in an unspecific order) and then substituting the variables for the non-slot expressions. In effect, cut evaluates non-slot expressions at the time the resulting procedure is called, whereas cute evaluates the non-slot expressions at the time the procedure is constructed.")))
