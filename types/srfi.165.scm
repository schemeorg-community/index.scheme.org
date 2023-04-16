(((name . "make-computation-environment-variable")
  (signature lambda (((or string? symbol?) name) default (boolean? immutable?)) computation-env-variable)
  (tags pure)
  (desc . "Returns a Scheme object that can be used as an environment variable, whose default value is default and which is immutable if immutable? is not #f. The symbol or string name is solely for debugging purposes. The type of the returned object is unspecified. Each invocation returns an environment variable different to any previously returned environment variable."))
 ((name . "make-computation-environment")
  (signature lambda () computation-env)
  (tags pure)
  (desc . "Returns a new environment, in which environment variables can be bound to values. The type of the returned object is unspecified."))
 ((name . "computation-environment-ref")
  (signature lambda ((computation-env env) (computation-env-variable var)) *)
  (tags pure)
  (desc . "If the variable var is bound to a value in the environment env, returns that value, and the environment variable's default value otherwise."))
 ((name . "computation-environment-update")
  (signature lambda ((computation-env env) (computation-env-variable var1) val1 ...) computation-env)
  (tags pure)
  (desc . "The arguments arg... alternate between environment variables var and values val. Returns a new environment that extends the environment env by binding each environment variable var to the respective value val."))
 ((name . "computation-environment-update!")
  (signature lambda ((computation-env env) (computation-env-variable var) val) undefined)
  (desc . "Updates the environment env in place by binding the environment variable var to val, and returns an unspecified value."))
 ((name . "computation-environment-copy")
  (signature lambda ((computation-env env)) computation-env)
  (tags pure)
  (desc . "Returns a fresh copy of the environment env."))
 ((name . "make-computation")
  (signature lambda ((procedure? proc)) computation)
  (subsigs
    (proc (lambda ((procedure? compute)) *))
    (compute (lambda ((computation computation)) *)))
  (desc . "Takes a procedure proc, which takes one argument, and returns a computation. The Scheme type of a computation is disjoint from any type, as if created by define-record-type, except possibly procedures.
When the computation is later executed on an environment, the procedure proc is called with an argument compute, which is a procedure taking one argument. Whenever compute is invoked on another computation, the other computation is executed on the environment and its results are returned. The results yielded by the execution are the results of the invocation of proc."))
 ((name . "computation-run")
  (signature lambda ((computation computation)) *)
  (tags pure)
  (desc . "Executes the computation computation and returns the results it yields."))
 ((name . "computation-ask")
  (signature lambda () computation)
  (tags pure)
  (desc . "Returns a computation that, when executed on an environment, yields that environment."))
 ((name . "computation-local")
  (signature lambda ((procedure? updater) (computation computation)) computation)
  (subsigs
    (updater (lambda ((computation-env env)) computation-env)))
  (tags pure)
  (desc . "Returns a computation that, when executed on an environment env, invokes the procedure updater on env, executes the computation computation on the result of the invocation of updater, which must be an environment, and yields its results."))
 ((name . "computation-pure")
  (signature lambda (obj1 ...) computation)
  (tags pure)
  (desc . "Returns a computation that, when executed, yields the values obj1..."))
 ((name . "computation-each")
  (signature lambda ((computation computation1) ...) computation)
  (tags pure)
  (desc . "Returns a computation that, when executed, sequentially executes the computations computation1, …, computationn and yields the results yielded by the last computation."))
 ((name . "computation-each-in-list")
  (signature lambda ((list? list)) computation)
  (subsigs
    (list (list computation)))
  (tags pure)
  (desc . "Equivalent to (computation-each computation1 ... computationn) if list is a list whose elements are computation1, ..., computationn."))
 ((name . "computation-bind")
  (signature lambda ((computation computation) (procedure? proc1) ...) computation)
  (subsigs
    (proc (lambda (obj) computation)))
  (tags pure)
  (desc . "(computation-bind computation) is equivalent to computation. (computation-bind computation proc1 proc2 ...) is equivalent to (computation-bind (computation-bind computation proc1) proc2 ...).
The invocation of (computation-bind computation proc) returns a computation that, when executed, executes the computation computation, on which results the procedure proc is then invoked, resulting in a computation that is then executed and whose results are yielded."))
 ((name . "computation-sequence")
  (signature lambda ((list? list)) computation)
  (subsigs
    (list (list computation)))
  (tags pure)
  (desc . "When invoked on a list list of computations, returns a computation that, when executed, executes the computations in sequence and yields a list whose elements are the results yielded by the computations."))
 ((name . "computation-forked")
  (signature lambda ((computation computation1) ...) computation)
  (tags pure)
  (desc . "Returns a computation that, when executed on an environment, executes each of the computations computation1, ... on fresh copies of the environment, and finally executes computation_n on the original environment and yields its results."))
 ((name . "computation-bind/forked")
  (signature lambda ((computation computation) (procedure? proc1) ...) computation)
  (subsigs
    (proc (lambda (obj) computation)))
  (tags pure)
  (desc . "As (computation-bind computation proc1 ...), but executes computation on a fresh copy of the environment."))
 ((name . "computation-fn")
  (signature syntax-rules ()
             ((_ ((variable1 init1) ...) body) computation))
  (desc . "Evaluates the expressions init1... to environment variables var1... in an unspecified order and returns a computation that, when executed, lexically binds the variables variable1... to the values to which the environment variables var1, ... are bound, and evaluates the body in the resulting lexical environment. The value of the last expression in body has to be a computation, which is then executed and its results yielded.
A clause of the form (variable variable) (i.e. the expression init is the variable reference variable) can be abbreviated by variable.
An unbound environment variable behaves as if it were bound to its default value."))
 ((name . "computation-with")
  (signature syntax-rules ()
             ((_ ((variable1 init1) ...) expr1 ... expr_n) computation))
  (desc . "Evaluates the expressions expr1...expr_n to computations computation1, ..., computation_n, the expressions variable1... to environment variables var1... and the expressions init1... to values val1... in an unspecified order, and returns a computation that, when executed on an environment, extends that environment non-destructively by binding var1,... to val1..., sequentially executes the computations computation1, ... computation_n on that extended environment, and then yields the results of the last computation."))
 ((name . "computation-with!")
  (signature syntax-rules ()
             ((_ (variable1 init1) ...) computation))
  (desc . "Evaluates the expressions variable1... to mutable environment variables var1... and the expressions init1... to values val1... in an unspecified order, and returns a computation that, when executed on an environment, modifies this environment in place by binding var1,... to val1... and which yields an unspecified value."))
 ((name . "default-computation")
  (signature value computation-env-variable)
  (desc . "This SRFI exports the identifier default-computation, which is bound to a location holding a mutable environment variable (as if created by (make-computation-environment-variable)) in the sense of this SRFI. In each fresh computation environment, default-computation is initially unbound. Whenever a computation computation is to be executed on an environment and is neither a computation nor a procedure, the value to which default-computation is bound in the environment has to be a procedure, which is then invoked on computation to return a computation, which is then executed on the environment."))
 ((name . "define-computation-type")
  (signature syntax-rules ()
             ((_ make-environment run clause ...)))
  (subsigs
    (clause (pattern (variable default)
                     (variable default "immutable")
                     variable)))
  (desc . "This syntax may appear wherever other definitions may appear. Each clause is of the form (variable default), (variable default \"immutable\"), or variable. The latter form is equivalent to (variable #f). make-environment, run, and each variable are identifiers.
An instance of define-computation-type is equivalent to the following definitions:
* make-environment is bound to a procedure that takes no arguments. Invoking the procedure is equivalent to invoking make-computation-environment except that the environment variables defined below can be used with the resulting environment.
* run is bound to a procedure that takes one argument. Invoking the procedure is equivalent to invoking computation-run except for that the initial environment is created by invoking the procedure bound to make-environment.
* Each variable is bound to an environment variable, which can only be used with an environment created by invoking the procedure bound to make-environment. Its default value is the result of evaluating default. The environment variable is immutable if the (variable default \"immutable\") form is used.")))