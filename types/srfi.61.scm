(((name . "cond")
  (signature syntax-rules (else =>) ((_ clause1 clause2 ...)))
  (subsigs
   (clause
    (pattern
     (test expression1 ...)
     (test => receiver)
     (test guard => receiver)
     (else expression1 expression2 ...)))
   (guard (value procedure?))
   (receiver (value procedure?)))))
