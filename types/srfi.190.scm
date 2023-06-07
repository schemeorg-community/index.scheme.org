(((name . "coroutine-generator")
  (signature syntax-rules ()
             ((_ body) procedure?))
  (desc . "Creates a generator from a coroutine. When evaluated, immediately returns a generator g. When g is called, the definitions and expressions in <body> are evaluated until the yielding procedure of the coroutine generator is called. Calling the yielding procedure of the coroutine generator causes the evaluation of <body> to be suspended, and g returns the value passed to yield.
Whether this generator is finite or infinite depends on the behavior of <body>. If the last expression in body> returns, it is the end of the sequence — g returns an end-of-file object from then on."))
 ((name . "yield")
  (signature lambda () procedure?)
  (subsigs
    (return (lambda (value) undefined)))
  (desc . "Evaluates to the yielding procedure in the (expansion of the) <body> of a coroutine generator. It is an error to evaluate yield outside the body of a coroutine generator."))
 ((name . "define-coroutine-generator")
  (signature syntax-rules ()
             ((_ name body))
             ((_ (name . formals) body)))
  (desc . "Expands into
  (define <name> (coroutine-generator <body>))
and
  (define (<name> . <formals>) (coroutine-generator <body>))
respectively.")))
