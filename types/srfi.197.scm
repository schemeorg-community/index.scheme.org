(((name . "chain")
  (signature syntax-rules () 
             ((_ initial-value step ...))
             ((_ initial-value placeholder step ...))
             ((_ initial-value placeholder ellipsis step ...)))
  (subsigs
    (step (pattern (datum ...))))
  (desc . "Syntax: <initial-value> is an expression.
<placeholder> and <ellipsis> are literal symbols; these are the placeholder symbol and ellipsis symbol. If <placeholder> or <ellipsis> are not present, they default to _ and ..., respectively.
The syntax of <step> is (<datum> ...), where each <datum> is either the placeholder symbol, the ellipsis symbol, or an expression. A <step> must contain at least one <datum>. The ellipsis symbol is only allowed at the end of a <step>, and it must immediately follow a placeholder symbol.
Semantics: chain evaluates each <step> in order from left to right, passing the result of each step to the next.
Each <step> is evaluated as an application, and the return value(s) of that application are passed to the next step as its pipeline values. <initial-value> is the pipeline value of the first step. The return value(s) of chain are the return value(s) of the last step.
The placeholder symbols in each <step> are replaced with that step's pipeline values, in the order they appear. It is an error if the number of placeholders for a step does not equal the number of pipeline values for that step, unless the step contains no placeholders, in which case it will ignore its pipeline values."))
 ((name . "chain-and")
  (signature syntax-rules () 
             ((_ initial-value step ...))
             ((_ initial-value placeholder step ...)))
  (subsigs
    (step (pattern (datum ...))))
  (desc . "Syntax: <initial-value> is an expression. <placeholder> is a literal symbol; this is the placeholder symbol. If <placeholder> is not present, the placeholder symbol is _. The syntax of <step> is (<datum> ... [<_> <datum> ...]), where <_> is the placeholder symbol.
Semantics: A variant of chain that short-circuits and returns #f if any step returns #f. chain-and is to chain as SRFI 2 and-let* is to let*.
Each <step> is evaluated as an application. If the step evaluates to #f, the remaining steps are not evaluated, and chain-and returns #f. Otherwise, the return value of the step is passed to the next step as its pipeline value. <initial-value> is the pipeline value of the first step. If no step evaluates to #f, the return value of chain-and is the return value of the last step.
The <_> placeholder in each <step> is replaced with that step's pipeline value. If a <step> does not contain <_>, it will ignore its pipeline value, but chain-and will still check whether that pipeline value is #f.
Because chain-and checks the return value of each step, it does not support steps with multiple return values. It is an error if a step returns more than one value."))
 ((name . "chain-when")
  (signature syntax-rules () 
             ((_ initial-value (guard step) ...))
             ((_ initial-value placeholder (guard step) ...))
             ((_ initial-value (step) ...))
             ((_ initial-value placeholder (step) ...)))
  (subsigs
    (step (pattern (datum ...))))
  (desc . "Syntax: <initial-value> and <guard> are expressions. <placeholder> is a literal symbol; this is the placeholder symbol. If <placeholder> is not present, the placeholder symbol is _. The syntax of <step> is (<datum> ... [<_> <datum> ...]), where <_> is the placeholder symbol.
Semantics: A variant of chain in which each step has a guard expression and will be skipped if the guard expression evaluates to #f.
Each <step> is evaluated as an application. The return value of the step is passed to the next step as its pipeline value. <initial-value> is the pipeline value of the first step.
The <_> placeholder in each <step> is replaced with that step's pipeline value. If a <step> does not contain <_>, it will ignore its pipeline value
If a step's <guard> is present and evaluates to #f, that step will be skipped, and its pipeline value will be reused as the pipeline value of the next step. The return value of chain-when is the return value of the last non-skipped step, or <initial-value> if all steps are skipped.
Because chain-when may skip steps, it does not support steps with multiple return values. It is an error if a step returns more than one value."))
 ((name . "chain-lambda")
  (signature syntax-rules () 
             ((_ initial-value step ...) procedure?)
             ((_ initial-value placeholder step ...) procedure?)
             ((_ initial-value placeholder ellipsis step ...) procedure?))
  (subsigs
    (step (pattern (datum ...))))
  (desc . "Syntax: <placeholder> and <ellipsis> are literal symbols; these are the placeholder symbol and ellipsis symbol. If <placeholder> or <ellipsis> are not present, they default to _ and ..., respectively.
The syntax of <step> is (<datum> ...), where each <datum> is either the placeholder symbol, the ellipsis symbol, or an expression. A <step> must contain at least one <datum>. The ellipsis symbol is only allowed at the end of a <step>, and it must immediately follow a placeholder symbol.
Semantics: Creates a procedure from a sequence of chain steps. When called, a chain-lambda procedure evaluates each <step> in order from left to right, passing the result of each step to the next.
Each <step> is evaluated as an application, and the return value(s) of that application are passed to the next step as its pipeline values. The procedure's arguments are the pipeline values of the first step. The return value(s) of the procedure are the return value(s) of the last step.
The placeholder symbols in each <step> are replaced with that step's pipeline values, in the order they appear. It is an error if the number of placeholders for a step does not equal the number of pipeline values for that step, unless the step contains no placeholders, in which case it will ignore its pipeline values.
If a <step> ends with a placeholder symbol followed by an ellipsis symbol, that placeholder sequence is replaced with all remaining pipeline values that do not have a matching placeholder.
The number of placeholders in the first <step> determines the arity of the procedure. If the first step ends with an ellipsis symbol, the procedure is variadic."))
 ((name . "nest")
  (signature syntax-rules () 
             ((_ step ... initial-value))
             ((_ placeholder step ... initial-value)))
  (subsigs
    (step (pattern (datum ...))))
  (desc . "Syntax: <placeholder> is a literal symbol; this is the placeholder symbol. If <placeholder> is not present, the placeholder symbol is _. The syntax of <step> is (<datum> ... <_> <datum> ...), where <_> is the placeholder symbol. <initial-value> is expression.
Semantics: nest is similar to chain, but sequences its steps in the opposite order. Unlike chain, nest literally nests expressions; as a result, it does not provide the same strict evaluation order guarantees as chain.
A nest expression is evaluated by lexically replacing the <_> in the last <step> with <initial-value>, then replacing the <_> in the next-to-last <step> with that replacement, and so on until the <_> in the first <step> has been replaced. It is an error if the resulting final replacement is not an expression, which is then evaluated and its values are returned.
Because it produces an actual nested form, nest can build expressions that chain cannot. For example, nest can build a quoted data structure:
nest can also safely include special forms like if, let, lambda, or parameterize in a pipeline."))
 ((name . "nest-reverse")
  (signature syntax-rules () 
             ((_ initial-value step ...))
             ((_ initial-value placeholder step ...)))
  (subsigs
    (step (pattern (datum ...))))
  (desc . "Syntax: <initial-value> is an expression. <placeholder> is a literal symbol; this is the placeholder symbol. If <placeholder> is not present, the placeholder symbol is _. The syntax of <step> is (<datum> ... <_> <datum> ...), where <_> is the placeholder symbol.
Semantics: nest-reverse is variant of nest that nests in reverse order, which is the same order as chain.
A nest-reverse expression is evaluated by lexically replacing the <_> in the first <step> with <initial-value>, then replacing the <_> in the second <step> with that replacement, and so on until the <_> in the last <step> has been replaced. It is an error if the resulting final replacement is not an expression, which is then evaluated and its values are returned.")))
