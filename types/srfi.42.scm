;;TODO document if / not / and / or forms without repetition
(((name . "do-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... command)))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro)))
  (desc . "Evaluates the <command> exactly once for each binding in the sequence defined by the qualifiers. If there are no qualifiers <command> is evaluated exactly once. The expression is evaluated for its side-effects only. The result of the comprehension is unspecified."))
 ((name . "list-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) list?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro)))
  (desc . "The list of values obtained by evaluating <expression> once for each binding in the sequence defined by the qualifiers. If there are no qualifiers the result is the list with the value of <expression>."))
 ((name . "append-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) list?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value list?)))
  (desc . "The list obtained by appending all values of <expression>, which must all be lists. Think of it as (apply append (list-ec <qualifier>* <expression>))."))
 ((name . "string-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) string?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value char?)))
  (desc . "The string of all values of <expression>. Think of it as (list->string (list-ec <qualifier>* <expression>))."))
 ((name . "string-append-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) string?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value string?)))
  (desc . "The string obtained by appending all values of <expression>, which must all be strings. Think of it as (apply string-append (list-ec <qualifier>* <expression>))."))
 ((name . "vector-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) vector?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro)))
  (desc . "The vector of all values of <expression>. Think of it as (list->vector (list-ec <qualifier>* <expression>))."))
 ((name . "vector-of-length-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ k qualifier ... expression) vector?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (k (value integer?)))
  (desc . "The vector of all values of <expression>, of which there must be exactly <k>. This comprehension behaves like vector-ec but can be implemented more efficiently."))
 ((name . "sum-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) number?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value number?)))
  (desc . "The sum of all values of <expression>. Think of it as (apply + (list-ec <qualifier>* <expression>))."))
 ((name . "product-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) number?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value number?)))
  (desc . "The product of all values of <expression>. Think of it as (apply * (list-ec <qualifier>* <expression>))."))
 ((name . "min-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) number?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value number?)))
  (desc . "The minimum of all values of <expression>."))
 ((name . "max-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) number?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value number?)))
  (desc . "The maximum of all values of <expression>."))
 ((name . "any?-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... test) boolean?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro)))
  (desc . "Tests whether any value of <test> in the sequence of bindings specified by the qualifiers is non-#f. If this is the case, #t is returned, otherwise #f. If there are no bindings in the sequence specified by the qualifiers at all then the result is #f. The enumeration of values stops after the first non-#f encountered."))
 ((name . "every?-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... test) boolean?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro)))
  (desc . "Tests whether all values of <test> are non-#f. If this is the case, #t is returned, otherwise #f. If the sequence is empty the result is #t. Enumeration stops after the first #f."))
 ((name . "first-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ default qualifier ... expression)))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro)))
  (desc . "The first value of <expression> in the sequence of bindings specified by the qualifiers. Before enumeration, the result is initialized with the value of <default>; so this will be the result if the sequence is empty. Enumeration is terminated in first-ec when the first value has been computed."))
 ((name . "last-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ default qualifier ... expression)))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro)))
  (desc . "The last value of <expression> in the sequence of bindings specified by the qualifiers. Before enumeration, the result is initialized with the value of <default>; so this will be the result if the sequence is empty."))
 ((name . "fold-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ x0 qualifier ... expression f2)))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (f2 (value procedure?)))
  (desc . "Reduces the sequence x[0], x[1], ..., x[n-1] of values obtained by evaluating <expression> once for each binding as specified by <qualifier>*. The arguments <x0>, <f2>, and <f1>, all syntactically equivalent to <expression>, specify the reduction process.
The reduction process for fold-ec is defined as follows. A reduction variable x is initialized to the value of <x0>, and for each k in {0, ..., n-1} the command (set! x (<f2> x[k] x)) is evaluated. Finally, x is returned as the value of the comprehension.
As the order of the arguments suggests, <x0> is evaluated outside the scope of the qualifiers, whereas the reduction expressions involving <f1> and <f2> are inside the scope of the qualifiers (so they may depend on any variable introduced by the qualifiers). Note that <f2> is evaluated repeatedly, with any side-effect or overhead this might have."))
 ((name . "fold3-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ x0 qualifier ... expression f1 f2)))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (f1 (value procedure?))
   (f2 (value procedure?)))
  (desc . "Reduces the sequence x[0], x[1], ..., x[n-1] of values obtained by evaluating <expression> once for each binding as specified by <qualifier>*. The arguments <x0>, <f2>, and <f1>, all syntactically equivalent to <expression>, specify the reduction process.
The reduction process for fold3-ec is defined as follows.  If and only if n = 0, i.e. the sequence is empty, then <x0> is evaluated and returned as the value of the comprehension. Otherwise, a reduction variable x is initialized to the value of (<f1> x[0]), and for each k in {1, ..., n-1} the command (set! x (<f2> x[k] x)) is evaluated. Finally, x is returned as the value of the comprehension.
As the order of the arguments suggests, <x0> is evaluated outside the scope of the qualifiers, whereas the reduction expressions involving <f1> and <f2> are inside the scope of the qualifiers (so they may depend on any variable introduced by the qualifiers). Note that <f2> is evaluated repeatedly, with any side-effect or overhead this might have."))
 ((name . ":")
  (signature
   syntax-rules
   (index)
   ((_ var arg1 arg2 ...) generator-macro)
   ((_ var1 (index var2) arg1 arg2 ...) generator-macro))
  (desc . "First the expressions <arg1> <arg>* are evaluated into a[1] a[2] ... a[n] and then a global dispatch procedure is used to dispatch on the number and types of the arguments and run the resulting generator.
Initially (after loading the SRFI), the following cases are recognized:
:list if for all i in {1..n}: (list? a[i]).
:string if for all i in {1..n}: (string? a[i]).
:vector if for all i in {1..n}: (vector? a[i]).
:range if n in {1..3} and for all i in {1..n}: (integer? a[i]) and (exact? a[i]).
:real-range if n in {1..3} and for all i in {1..n}: (real? a[i]).
:char-range if  n = 2 and for all i in {1, 2}: (char? a[i]).
:port if  n in {1,2} and (input-port? a[1]) and (procedure? a[2])."))
 ((name . ":-dispatch-ref")
  (signature lambda () procedure?)
  (desc . "The current dispatcher used by `:`."))
 ((name . ":-dispatch-set!") 
  (signature lambda ((procedure? d)) undefined)
  (desc . "Install dispatched to be used with `:`."))
 ((name . "make-initial-:-dispatch") 
  (signature lambda () procedure?)
  (desc . "Get a copy of initial dispatcher for `:`."))
 ((name . ":list")
  (signature
   syntax-rules
   (index)
   ((_ var arg1 arg2 ...) generator-macro)
   ((_ var1 (index var2) arg1 arg2 ...) generator-macro))
  (subsigs (arg1 (value list?)) (arg2 (value list?)))
  (desc . "Run through one or more lists. First all expressions in <arg1> <arg>* are evaluated and then all elements of the resulting values are enumerated from left to right."))
 ((name . ":string")
  (signature
   syntax-rules
   (index)
   ((_ var arg1 arg2 ...) generator-macro)
   ((_ var1 (index var2) arg1 arg2 ...) generator-macro))
  (subsigs (arg1 (value string?)) (arg2 (value string?)))
  (desc . "Run through one or more strings. First all expressions in <arg1> <arg>* are evaluated and then all elements of the resulting values are enumerated from left to right."))
 ((name . ":vector")
  (signature
   syntax-rules
   (index)
   ((_ var arg1 arg2 ...) generator-macro)
   ((_ var1 (index var2) arg1 arg2 ...) generator-macro))
  (subsigs (arg1 (value vector?)) (arg2 (value vector?)))
  (desc . "Run through one or more vectors. First all expressions in <arg1> <arg>* are evaluated and then all elements of the resulting values are enumerated from left to right."))
 ((name . ":integers")
  (signature
   syntax-rules
   (index)
   ((_ var) generator-macro)
   ((_ var1 (index var2)) generator-macro))
  (desc . "Runs through the sequence 0, 1, 2, ... of non-negative integers. This is most useful in combination with :parallel, :while, and :until or with a non-local exit in the body of the comprehension."))
 ((name . ":range")
  (signature
   syntax-rules
   (index)
   ((_ var stop) generator-macro)
   ((_ var1 (index var2) stop) generator-macro)
   ((_ var start stop) generator-macro)
   ((_ var1 (index var2) start stop) generator-macro)
   ((_ var start stop step) generator-macro)
   ((_ var1 (index var2) start stop step) generator-macro))
  (subsigs
   (start (value integer?))
   (stop (value integer?))
   (step (value integer?)))
  (desc . "Runs through a range of exact rational numbers.
The form (:range <vars> <stop>) evaluates the expression <stop>, which must result in an exact integer n, and runs through the finite sequence 0, 1, 2, ..., n-1. If n is zero or negative the sequence is empty.
The form (:range <vars> <start> <stop>) evaluates the expressions <start> and <stop>, which must result in exact integers a and b, and runs through the finite sequence a, a+1, a+2, ..., b-1. If b is less or equal a then the sequence is empty.
The form (:range <vars> <start> <stop> <step>) first evaluates the expressions <start>, <stop>, and <step>, which must result in exact integers a, b, and s such that s is unequal to zero. Then the sequence a, a + s, a + 2 s, ..., a + (n-1) s is enumerated where n = ceil((b-a)/s). In other words, the sequence starts at a, increments by s, and stops when the next value would reach or cross b. If n is zero or negative the sequence is empty."))
 ((name . ":real-range")
  (signature
   syntax-rules
   (index)
   ((_ var stop) generator-macro)
   ((_ var1 (index var2) stop) generator-macro)
   ((_ var start stop) generator-macro)
   ((_ var1 (index var2) start stop) generator-macro)
   ((_ var start stop step) generator-macro)
   ((_ var1 (index var2) start stop step) generator-macro))
  (subsigs (start (value real?)) (stop (value real?)) (step (value real?)))
  (desc . "Runs through a range of real numbers using an explicit index variable. This form of range enumeration avoids accumulation of rounding errors and is the one to use if any of the numbers defining the range is inexact, not an integer, or a bignum of large magnitude. 
Providing default value 0 for <start> and 1 for <step>, the generator first evaluates <start>, <stop>, and <step>, which must result in reals a, b, and s such that n = (b-a)/s is also representable as a real. Then the sequence 0, 1, 2, ... is enumerated while the current value i is less than n, and the variable in <vars> is bound to the value a + i s. If any of the values a, b, or s is non-exact then all values in the sequence are non-exact."))
 ((name . ":char-range")
  (signature
   syntax-rules
   (index)
   ((_ var min max) generator-macro)
   ((_ var1 (index var2) min max) generator-macro))
  (subsigs (min (value char?)) (max (value char?)))
  (desc . "Runs through a range of characters. First <min> and <max> are evaluated, which must result in two characters a and b. Then the sequence of characters a, a+1, a+2, ..., b is enumerated in the order defined by char<=? in the sense of [R5RS, 6.3.4.]. If b is smaller than a then the sequence is empty. (Note that b is included in the sequence.)"))
 ((name . ":port")
  (signature
   syntax-rules
   (index)
   ((_ var port) generator-macro)
   ((_ var1 (index var2) port) generator-macro)
   ((_ var port read-proc) generator-macro)
   ((_ var1 (index var2) port read-proc) generator-macro))
  (subsigs (port (value input-port?)) (read-proc (value procedure?)))
  (desc . "Reads from the port until the eof-object is read. Providing the default read for <read-proc>, the generator first evaluates <port> and <read-proc>, which must result in an input port p and a procedure r. Then the variable is run through the sequence obtained by (r p) while the result does not satisfy eof-object?."))
 ((name . ":dispatched")
  (signature
   syntax-rules
   (index)
   ((_ var dispatch arg1 arg2 ...) generator-macro)
   ((_ var1 (index var2) dispatch arg1 arg2 ...) generator-macro))
  (subsigs (dispatch (value procedure?)))
  (desc . "Runs the variables through a sequence defined by <dispatch> and <arg1> <arg>*. The purpose of :dispatched is implementing dispatched generators, in particular the predefined dispatching generator :.
The working of :dispatched is as follows. First <dispatch> and <arg1> <arg>* are evaluated, resulting in a procedure d (the 'dispatcher') and the values a[1] a[2] ... a[n]. Then (d (list a[1] a[2] ... a[n] )) is evaluated, resulting in a value g. If g is not a procedure then the dispatcher did not recognize the argument list and an error is raised. Otherwise the 'generator procedure' g is used to run <vars> through a sequence of values. The sequence defined by g is obtained by repeated evaluation of (g empty) until the result is empty. In other words, g indicates the end of the sequence by returning its only argument, for which the caller has provided an object distinct from anything g can produce."))
 ((name . ":generator-proc")
  (signature syntax-rules () ((_ generator-macro) procedure?))
  (desc . "Constructs a generator procedure from a typed generator. Let (g var arg1 arg ...) be an instance of the <generator> syntax, for example an application-specific typed generator, with a single variable var and no index variable. Then 
(:generator-proc (g arg1 arg ...)) => g
where the generator procedure g runs through the list (list-ec (g var arg1 arg ...) var)."))
 ((name . "dispatch-union")
  (signature lambda ((procedure? d1) (procedure? d2)) procedure?)
  (desc . "The new dispatcher d recognizes the union of the cases recognized by the dispatchers d1 and d2. The new dispatcher always tries both component dispatchers and raises an error in case of conflict. The identification returned by (d) is the concatenation of the component identifications (d1) and (d2), enclosed in lists if necessary."))
 ((name . ":do")
  (signature
   syntax-rules
   (let)
   ((_ (lb ...) ne1? (ls ...)) generator-macro)
   ((_
     (let (ob ...) oc ...)
     (lb ...)
     ne1?
     (let (ib ...) ic ...)
     ne2?
     (ls ...))))
  (desc . "Defines a generator in terms of a named-let, optionally decorated with inner and outer lets. This generator is for defining other generators. (In fact, the reference implementation transforms any other generator into an instance of fully decorated :do.) The generator is a compromise between expressive power (more flexible loops) and fixed structure (necessary for merging and modifying generators). In the fully decorated form, the syntactic variables <ob> (outer binding), <oc> (outer command), <lb> (loop binding), <ne1?> (not-end1?), <ib> (inner binding), <ic> (inner command), <ne2?> (not-end2?), and <ls> (loop step) define the following loop skeleton:
(let (<ob>*)
     <oc>*
     (let loop (<lb>*)
        (if <ne1?>
            (let (<ib>*)
              <ic>*
              payload
              (if <ne2?>
                  (loop <ls>*)))))),
where <oc>* and <ic>* are syntactically equivalent to <command>*, i.e. they do not begin with a <definition>. The latter requirement allows the code generator to produce more efficient code for special cases by removing empty let-expressions altogether."))
 ((name . ":let")
  (signature
   syntax-rules
   (index)
   ((_ var expression) generator-macro)
   ((_ var1 (index var2) expression) generator-macro))
  (desc . "Runs through the sequence consisting of the value of <expression>, only. This is the same as (:list <vars> (list <expression>)). If an index variable is specified, its value is 0. The :let-generator can be used to introduce an intermediate variable depending on outer generators."))
 ((name . ":parallel")
  (signature syntax-rules () ((_ generator ...) generator-macro))
  (subsigs (generator (value generator-macro)))
  (desc . "Runs several generators in parallel. This means that the next binding in the sequence is obtained by advancing each generator in <generator>* by one step. The parallel generator terminates when any of its component generators terminates. The generators share a common scope for the variables they introduce. This implies that the names of the variables introduced by the various generators must be distinct."))
 ((name . ":while")
  (signature syntax-rules () ((_ generator expression) generator-macro))
  (subsigs (generator (value generator-macro)))
  (desc . "Runs <generator> while <expression> evaluates to non-#f. The guarding expression is included in the scope of the variables introduced by the generator.
Note the distinction between the filter if and the modified generator expressed by :while."))
 ((name . ":until")
  (signature syntax-rules () ((_ generator expression) generator-macro))
  (subsigs (generator (value generator-macro)))
  (desc . "Runs <generator> until after <expression> has evaluated to non-#f. The guarding expression is included in the scope of the variables introduced by the generator.
Note the distinction between :while, stopping at a certain condition, and :until, stopping after a certain condition has occurred. The latter implies that the binding that has triggered termination has been processed by the comprehension.")))
