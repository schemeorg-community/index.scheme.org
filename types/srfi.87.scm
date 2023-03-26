(((name . "case")
  (signature syntax-rules (=> else) ((_ key clause1 clause2 ...)))
  (subsigs
   (clause
    (pattern
     ((datum1 ...) expression1 expression2 ...)
     ((datum1 ...) => expression)
     (else expression1 expression2 ...)
     (else => expression))))
  (desc . "A `case' expression is evaluated as follows. <Key> is evaluated and its result is compared against each <datum>. If the result of evaluating <key> is equivalent (in the sense of `eqv?'; see section see section 6.1 Equivalence predicates) to a <datum>, then the expressions in the corresponding <clause> are evaluated from left to right and the result(s) of the last expression in the <clause> is(are) returned as the result(s) of the `case' expression. If the result of evaluating <key> is different from every <datum>, then if there is an else clause its expressions are evaluated and the result(s) of the last is(are) the result(s) of the `case' expression; otherwise the result of the `case' expression is unspecified. If the selected <clause> uses the => alternate form, then the <expression> is evaluated. Its value must be a procedure that accepts one argument; this procedure is then called on the value of <Key> and the value(s) returned by this procedure is(are) returned by the `case' expression.")))
