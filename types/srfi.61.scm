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
   (receiver (value procedure?)))
  (desc . "The <cond clause> production in the formal syntax of Scheme as written by R5RS in section 7.1.3 is extended with a new option:
      <cond clause> --->
          ...
        | (<generator> <guard> => <receiver>)
where <generator>, <guard>, & <receiver> are all <expression>s.
Clauses of this form have the following semantics: <generator> is evaluated. It may return arbitrarily many values. <Guard> is applied to an argument list containing the values in order that <generator> returned. If <guard> returns a true value for that argument list, <receiver> is applied with an equivalent argument list. If <guard> returns a false value, however, the clause is abandoned and the next one is tried.")))
