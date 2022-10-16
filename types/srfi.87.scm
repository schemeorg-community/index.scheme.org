(((name . case)
  (signature syntax-rules (=> else) ((_ key clause1 clause2 ...)))
  (subsigs
   (clause
    ((datum1 ...) expression1 expression2 ...)
    ((datum1 ...) => expression)
    (else expression1 expression2 ...)
    (else => expression)))))
