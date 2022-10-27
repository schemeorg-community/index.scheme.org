(((name . "when")
  (signature syntax-rules () ((_ test expression1 expression2 ...))))
 ((name . "unless")
  (signature syntax-rules () ((_ test expression1 expression2 ...))))
 ((name . "do")
  (signature
   syntax-rules
   ()
   ((_ (variable-decl1 ...) (test expression ...) command ...)))
  (subsigs (variable-decl (pattern (variable init step) (variable init)))))
 ((name . "case-lambda")
  (signature syntax-rules () ((_ clause ...) procedure?))
  (subsigs
   (clause (pattern (formals body)))
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))))
