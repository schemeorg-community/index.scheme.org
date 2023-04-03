(((name . "independently")
  (signature syntax-rules ()
             ((_ expression ...)))
  (desc . "Evaluates the ⟨expressions⟩s in an unspecified order and discards their return values. The result of the independently expression is unspecified.
Note: Although the order of evaluation is otherwise unspecified, the effect of any concurrent evaluation of the ⟨expressions⟩ is constrained to be consistent with some sequential order of evaluation. The order of evaluation may be chosen differently for each evaluation of the independently form.")))
