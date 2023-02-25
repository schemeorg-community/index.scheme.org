(((name . "with-exception-handler")
  (signature lambda ((procedure? handler) (procedure? thunk)) *)
  (subsigs (handler (lambda (obj) *)) (thunk (lambda () *)))
  (desc . " Handler must be a procedure and should accept one argument. Thunk must be a procedure that accepts zero arguments. The with-exception-handler procedure returns the results of invoking thunk. Handler is installed as the current exception handler for the dynamic extent (as determined by dynamic-wind) of the invocation of thunk.
Implementation responsibilities: The implementation must check the restrictions on handler to the extent performed by applying it as described when it is called as a result of a call to raise or raise-continuable. An implementation may check whether handler is an appropriate argument before applying it."))
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
  (desc . "Evaluating a guard form evaluates <body> with an exception handler that binds the raised object to <variable> and within the scope of that binding evaluates the clauses as if they were the clauses of a cond expression. That implicit cond expression is evaluated with the continuation and dynamic environment of the guard expression. If every <cond clause>'s <test> evaluates to #f and there is no else clause, then raise is re-invoked on the raised object within the dynamic environment of the original call to raise except that the current exception handler is that of the guard expression. The final expression in a <cond> clause is in a tail context if the guard expression itself is. "))
 ((name . "raise") 
  (signature lambda (obj) undefined)
  (desc . "Raises a non-continuable exception by invoking the current exception handler on obj. The handler is called with a continuation whose dynamic environment is that of the call to raise, except that the current exception handler is the one that was in place when the handler being called was installed. When the handler returns, a non-continuable exception with condition type &non-continuable is raised in the same dynamic environment as the handler."))
 ((name . "raise-continuable") 
  (signature lambda (obj) undefined)
  (desc . "Raises a continuable exception by invoking the current exception handler on obj. The handler is called with a continuation that is equivalent to the continuation of the call to raise-continuable, with these two exceptions: (1) the current exception handler is the one that was in place when the handler being called was installed, and (2) if the handler being called returns, then it will again become the current exception handler. If the handler returns, the values it returns become the values returned by the call to raise-continuable.")))
