(((name . with-exception-handler)
  (signature lambda ((procedure? handler) (procedure? thunk)) *)
  (subsigs (handler (lambda (obj) *)) (thunk (lambda () *))))
 ((name . guard)
  (signature
    syntax-rules
    (=> else)
    ((_ (variable cond-clause1 cond-clause2 ...) body)))
  (subsigs
    (cond-clause
      (test expression1 ...)
      (test => expression)
      (else expression1 expression2 ...))))
 ((name . raise) (signature lambda (obj) undefined))
 ((name . raise-continuable) (signature lambda (obj) undefined)))
