(((name . cond)
  (signature syntax-rules (else =>) ((_ clause1 clause2 ...)))
  (subsigs
   (clause
    (test expression1 ...)
    (test => receiver)
    (test guard => receiver)
    (else expression1 expression2 ...)))
  (syntax-param-signatures
    (guard procedure?)
    (receiver procedure?))))
