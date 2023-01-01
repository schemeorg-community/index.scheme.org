(((name . "when")
  (signature syntax-rules () ((_ test expression1 expression2 ...)))
  (desc . "A when expression is evaluated by evaluating the <test> expression. If <test> evaluates to a true value, the remaining <expression>s are evaluated in order, and the results of the last <expression> are returned as the results of the entire when expression. Otherwise, the when expression returns unspecified values.
The final <expression> is in tail context if the when form is itself in tail context."))
 ((name . "unless")
  (signature syntax-rules () ((_ test expression1 expression2 ...)))
  (desc . "An unless expression is evaluated by evaluating the <test> expression. If <test> evaluates to #f, the remaining <expression>s are evaluated in order, and the results of the last <expression> are returned as the results of the entire unless expression. Otherwise, the unless expression returns unspecified values.
The final <expression> is in tail context if the unless form is itself in tail context."))
 ((name . "do")
  (signature
   syntax-rules
   ()
   ((_ (variable-decl1 ...) (test expression ...) command ...)))
  (subsigs (variable-decl (pattern (variable init step) (variable init))))
  (desc . "The do expression is an iteration construct. It specifies a set of variables to be bound, how they are to be initialized at the start, and how they are to be updated on each iteration.
A do expression is evaluated as follows: The <init> expressions are evaluated (in some unspecified order), the <variable>s are bound to fresh locations, the results of the <init> expressions are stored in the bindings of the <variable>s, and then the iteration phase begins.
Each iteration begins by evaluating <test>; if the result is #f, then the <command>s are evaluated in order for effect, the <step> expressions are evaluated in some unspecified order, the <variable>s are bound to fresh locations holding the results, and the next iteration begins.
If <test> evaluates to a true value, the <expression>s are evaluated from left to right and the values of the last <expression> are returned. If no <expression>s are present, then the do expression returns unspecified values.
The regionof the binding of a <variable> consists of the entire do expression except for the <init>s.
A <step> may be omitted, in which case the effect is the same as if (<variable> <init> <variable>) had been written instead of (<variable> <init>).
If a do expression appears in a tail context, the <expression>s are a <tail sequence> in the sense of report section on “Tail calls and tail contexts”, i.e., the last <expression> is also in a tail context."))
 ((name . "case-lambda")
  (signature syntax-rules () ((_ clause ...) procedure?))
  (subsigs
   (clause (pattern (formals body)))
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))
  (desc . "A case-lambda expression evaluates to a procedure. This procedure, when applied, tries to match its arguments to the <case-lambda clause>s in order. The arguments match a clause if one of the following conditions is fulfilled:
* <Formals> has the form (<variable> ...) and the number of arguments is the same as the number of formal parameters in <formals>.
* <Formals> has the form (<variable1> ...<variablen> . <variablen+1)> and the number of arguments is at least n.
* <Formals> has the form <variable>.

For the first clause matched by the arguments, the variables of the <formals> are bound to fresh locations containing the argument values in the same arrangement as with lambda. The last expression of a <body> in a case-lambda expression is in tail context. If the arguments match none of the clauses, an exception with condition type &assertion is raised.")))
