(((name . "raise")
  (signature lambda (obj) undefined)
  (desc . "Raises an exception by invoking the current exception handler on obj. The handler is called with the same dynamic environment as that of the call to raise, except that the current exception handler is the one that was in place when the handler being called was installed. If the handler returns, a secondary exception is raised in the same dynamic environment as the handler. The relationship between obj and the object raised by the secondary exception is unspecified."))
 ((name . "guard")
  (signature
   syntax-rules
   (=> else)
   ((_ (variable cond-clause1 cond-clause2 ...) body)))
  (subsigs
   (cond-clause
    (pattern
     (test expression1 ...)
     (test => expression)
     (else expression1 expression2 ...))))
  (desc . "The <body> is evaluated with an exception handler that binds the raised object (see raise in section 6.11) to <variable> and, within the scope of that binding, evaluates the clauses as if they were the clauses of a cond expression. That implicit cond expression is evaluated with the continuation and dynamic environment of the guard expression. If every <cond clause>’s <test> evaluates to #f and there is no else clause, then raise-continuable is invoked on the raised object within the dynamic environment of the original call to raise or raise-continuable, except that the current exception handler is that of the guard expression. See section 6.11 for a more complete discussion of exceptions."))
 ((name . "with-exception-handler")
  (signature lambda ((procedure? handler) (procedure? thunk)) *)
  (subsigs (handler (lambda (obj) *)) (thunk (lambda () *)))
  (desc . "It is an error if handler does not accept one argument. It is also an error if thunk does not accept zero arguments. The with-exception-handler procedure returns the results of invoking thunk. Handler is installed as the current exception handler in the dynamic environment used for the invocation of thunk.")))
