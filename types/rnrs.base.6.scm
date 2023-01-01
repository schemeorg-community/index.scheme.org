(((name . "*")
  (signature lambda ((number? z) ...) number?)
  (tags pure)
  (desc . "This procedure returns the product of its arguments."))
 ((name . "+")
  (signature lambda ((number? z) ...) number?)
  (tags pure)
  (desc . "This procedure returns the sum of its arguments."))
 ((name . "-")
  (signature lambda ((number? z) ...) number?)
  (tags pure)
  (desc . "With two or more arguments, this procedures returns the difference of its arguments, associating to the left. With one argument, however, it returns the additive inverse of its argument."))
 ((name . "/")
  (signature lambda ((number? z1) (number? z2) ...) number?)
  (tags pure)
  (desc . "If all of the arguments are exact, then the divisors must all be nonzero. With two or more arguments, this procedure returns the quotient of its arguments, associating to the left. With one argument, however, it returns the multiplicative inverse of its argument."))
 ((name . "<")
  (signature lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if its arguments are monotonically increasing, and #f otherwise."))
 ((name . "<=")
  (signature lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if its arguments are monotonically nondecreasing, and #f otherwise."))
 ((name . "=")
  (signature lambda ((number? z1) (number? z2) (number? z3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if its arguments are equal, and #f otherwise."))
 ((name . ">")
  (signature lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if its arguments are decreasing, and #f otherwise."))
 ((name . ">=")
  (signature lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if its arguments are monotonically nonincreasing, and #f otherwise."))
 ((name . "abs")
  (signature lambda ((real? x)) number?)
  (tags pure)
  (desc . "Returns the absolute value of its argument."))
 ((name . "acos")
  (signature lambda ((number? z)) number?)
  (tags pure)
  (desc . "Computes arccosine. The procedure may return inexact results even when given exact arguments."))
 ((name . "and")
  (signature syntax-rules () ((_ test1 ...)))
  (desc . "Semantics: If there are no <test>s, #t is returned. Otherwise, the <test> expressions are evaluated from left to right until a <test> returns #f or the last <test> is reached. In the former case, the and expression returns #f without evaluating the remaining expressions. In the latter case, the last expression is evaluated and its values are returned."))
 ((name . "angle")
  (signature lambda ((complex? z)) real?)
  (tags pure)
  (desc . "Returns angle of a complex number, between -pi and pi."))
 ((name . "append")
  (signature case-lambda (((list? list) ...) list?) (((list? list) ... obj) *))
  (tags pure)
  (desc . "Returns a possibly improper list consisting of the elements of the first list followed by the elements of the other lists, with obj as the cdr of the final pair. An improper list results if obj is not a list.
If append constructs a nonempty chain of pairs, it is always newly allocated. If no pairs are allocated, obj is returned."))
 ((name . "apply")
  (signature lambda ((procedure? proc) arg1 ... (list? rest-args)) *)
  (tags pure)
  (desc . "Rest-args must be a list. Proc should accept n arguments, where n is number of args plus the length of rest-args. The apply procedure calls proc with the elements of the list (append (list arg1 ...) rest-args) as the actual arguments.
If a call to apply occurs in a tail context, the call to proc is also in a tail context."))
 ((name . "asin")
  (signature lambda ((number? z)) number?)
  (tags pure)
  (desc . "Computes arcsine. The procedure may return inexact results even when given exact arguments."))
 ((name . "assertion-violation")
  (signature
   lambda
   (((or string? symbol? #f) who) (string? message) irritant1 ...)
   undefined)
  (desc . "The assertion-violation procedure should be called when an invalid call to a procedure was made, either passing an invalid number of arguments, or passing an argument that it is not specified to handle.
The who argument should describe the procedure or operation that detected the exception. The message argument should describe the exceptional situation. The irritants should be the arguments to the operation that detected the operation.
The condition object provided with the exception (see library chapter on “Exceptions and conditions”) has the following condition types:
* If who is not #f, the condition has condition type &who, with who as the value of its field. In that case, who should be the name of the procedure or entity that detected the exception. If it is #f, the condition does not have condition type &who.
* The condition has condition type &message, with message as the value of its field.
* The condition has condition type &irritants, and its field has as its value a list of the irritants.

Moreover, the condition created by assertion-violation has condition type &assertion."))
 ((name . "assert")
  (signature syntax-rules () ((_ expression)))
  (desc . "An assert form is evaluated by evaluating <expression>. If <expression> returns a true value, that value is returned from the assert expression. If <expression> returns #f, an exception with condition types &assertion and &message is raised. The message provided in the condition object is implementation-dependent."))
 ((name . "atan")
  (signature case-lambda
             (((number? z)) number?)
             (((real? x1) (real? x2)) number?))
  (tags pure)
  (desc . "Computes arctangent. The two-argument variant of atan computes (angle (make-rectangular x2 x1)). The procedure may return inexact results even when given exact arguments."))
 ((name . "begin")
  (signature syntax-rules () ((_ expression-or-definition ...)))
  (desc . " The <begin> keyword has two different roles, depending on its context:
* It may appear as a form in a <body> (see section 11.3), <library body> (see section 7.1), or <top-level body> (see chapter 8), or directly nested in a begin form that appears in a body. In this case, the begin form must have the shape specified in the first header line. This use of begin acts as a splicing form—the forms inside the <body> are spliced into the surrounding body, as if the begin wrapper were not actually present.
A begin form in a <body> or <library body> must be non-empty if it appears after the first <expression> within the body.
* It may appear as an ordinary expression and must have the shape specified in the second header line. In this case, the <expression>s are evaluated sequentially from left to right, and the values of the last <expression> are returned. This expression type is used to sequence side effects such as assignments or input and output."))
 ((name . "boolean=?")
  (signature
   lambda
   ((boolean? boolean1) (boolean? boolean2) (boolean? boolean3) ...)
   boolean?)
  (tags pure)
  (desc . "Returns #t if the booleans are the same."))
 ((name . "boolean?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is either #t or #f and returns #f otherwise."))
 ((name . "caar") (signature lambda ((pair? pair)) *) (tags pure) (desc . "Composition of car and car."))
 ((name . "cadr") (signature lambda ((pair? pair)) *) (tags pure) (desc . "Composition of car and cdr."))
 ((name . "call-with-current-continuation")
  (signature lambda ((procedure? proc)) *)
  (subsigs (proc (lambda ((procedure? k)) *)))
  (desc . "Proc should accept one argument. The procedure call-with-current-continuation (which is the same as the procedure call/cc) packages the current continuation as an \"escape procedure\" and passes it as an argument to proc. The escape procedure is a Scheme procedure that, if it is later called, will abandon whatever continuation is in effect at that later time and will instead reinstate the continuation that was in effect when the escape procedure was created. Calling the escape procedure may cause the invocation of before and after procedures installed using dynamic-wind.
The escape procedure accepts the same number of arguments as the continuation of the original call to call-with-current-continuation.
The escape procedure that is passed to proc has unlimited extent just like any other procedure in Scheme. It may be stored in variables or data structures and may be called as many times as desired.
If a call to call-with-current-continuation occurs in a tail context, the call to proc is also in a tail context.
Note: Calling an escape procedure reenters the dynamic extent of the call to call-with-current-continuation, and thus restores its dynamic environment; see section 5.12."))
 ((name . "call-with-values")
  (signature lambda ((procedure? producer) (procedure? consumer)) *)
  (subsigs (producer (lambda () *)) (consumer (lambda (obj ...) *)))
  (tags pure)
  (desc . "The call-with-values procedure calls producer with no arguments and a continuation that, when passed some values, calls the consumer procedure with those values as arguments. The continuation for the call to consumer is the continuation of the call to call-with-values. If a call to call-with-values occurs in a tail context, the call to consumer is also in a tail context.
Implementation responsibilities: After producer returns, the implementation must check that consumer accepts as many values as consumer has returned."))
 ((name . "call/cc")
  (signature lambda ((procedure? proc)) *)
  (subsigs (proc (lambda ((procedure? k)) *)))
  (desc . "Proc should accept one argument. The procedure call-with-current-continuation (which is the same as the procedure call/cc) packages the current continuation as an \"escape procedure\" and passes it as an argument to proc. The escape procedure is a Scheme procedure that, if it is later called, will abandon whatever continuation is in effect at that later time and will instead reinstate the continuation that was in effect when the escape procedure was created. Calling the escape procedure may cause the invocation of before and after procedures installed using dynamic-wind.
The escape procedure accepts the same number of arguments as the continuation of the original call to call-with-current-continuation.
The escape procedure that is passed to proc has unlimited extent just like any other procedure in Scheme. It may be stored in variables or data structures and may be called as many times as desired.
If a call to call-with-current-continuation occurs in a tail context, the call to proc is also in a tail context.
Note: Calling an escape procedure reenters the dynamic extent of the call to call-with-current-continuation, and thus restores its dynamic environment; see section 5.12."))
 ((name . "car")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Returns the contents of the car field of pair."))
 ((name . "case")
  (signature syntax-rules (=> else) ((_ key clause1 clause2 ...)))
  (subsigs
   (clause
    (pattern
     ((datum1 ...) expression1 expression2 ...)
     ((datum1 ...) => expression)
     (else expression1 expression2 ...))))
  (desc . "The second form, which specifies an \"else clause\", may only appear as the last <case clause>. Each <datum> is an external representation of some object. The data represented by the <datum>s need not be distinct.
Semantics: A case expression is evaluated as follows. <Key> is evaluated and its result is compared using eqv? (see section 11.5) against the data represented by the <datum>s of each <case clause> in turn, proceeding in order from left to right through the set of clauses. If the result of evaluating <key> is equivalent to a datum of a <case clause>, the corresponding <expression>s are evaluated from left to right and the results of the last expression in the <case clause> are returned as the results of the case expression. Otherwise, the comparison process continues. If the result of evaluating <key> is different from every datum in each set, then if there is an else clause its expressions are evaluated and the results of the last are the results of the case expression; otherwise the case expression returns unspecified values."))
 ((name . "cdar") (signature lambda ((pair? pair)) *) (tags pure) (desc . "Composition of cdr and car."))
 ((name . "cddr") (signature lambda ((pair? pair)) *) (tags pure) (desc . "Composition of cdr and cdr."))
 ((name . "cdr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Returns the contents of the cdr field of pair."))
 ((name . "ceiling")
  (signature lambda ((real? x)) real?)
  (tags pure)
  (desc . "The ceiling procedure returns the smallest integer object not smaller than x.
Note:‌ If the argument to the procedure is inexact, then the result is also inexact. If an exact value is needed, the result should be passed to the exact procedure. "))
 ((name . "char->integer")
  (signature lambda ((char? char)) integer?)
  (tags pure)
  (desc . "Given a character, char->integer returns its Unicode scalar value as an exact integer object."))
 ((name . "char<=?")
  (signature lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if the results of passing their arguments to char->integer are monotonically non-decreasing. This predicate is required to be transitive."))
 ((name . "char<?")
  (signature lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if the results of passing their arguments to char->integer are monotonically increasing. This predicate is required to be transitive."))
 ((name . "char=?")
  (signature lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if the results of passing their arguments to char->integer are equal. This predicate is required to be transitive."))
 ((name . "char>=?")
  (signature lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if the results of passing their arguments to char->integer are monotonically non-increasing. This predicate is required to be transitive."))
 ((name . "char>?")
  (signature lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if the results of passing their arguments to char->integer are monotonically decreasing. This predicate is required to be transitive."))
 ((name . "char?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a character, otherwise returns #f."))
 ((name . "complex?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes number?))
 ((name . "cond")
  (signature syntax-rules (else =>) ((_ clause1 clause2 ...)))
  (subsigs
   (clause
    (pattern
     (test expression1 ...)
     (test => receiver)
     (else expression1 expression2 ...)))
   (receiver (value procedure?)))
  (desc . "A cond expression is evaluated by evaluating the <test> expressions of successive <cond clause>s in order until one of them evaluates to a true value(see section 5.7). When a <test> evaluates to a true value, then the remaining <expression>s in its <cond clause> are evaluated in order, and the results of the last <expression> in the <cond clause> are returned as the results of the entire cond expression. If the selected <cond clause> contains only the <test> and no <expression>s, then the value of the <test> is returned as the result. If the selected <cond clause> uses the => alternate form, then the <expression> is evaluated. Its value must be a procedure. This procedure should accept one argument; it is called on the value of the <test> and the values returned by this procedure are returned by the cond expression. If all <test>s evaluate to #f, and there is no else clause, then the conditional expression returns unspecified values; if there is an else clause, then its <expression>s are evaluated, and the values of the last one are returned."))
 ((name . "cons")
  (signature lambda (obj1 obj2) pair?)
  (tags pure)
  (desc . "Returns a newly allocated pair whose car is obj1 and whose cdr is obj2. The pair is guaranteed to be different (in the sense of eqv?) from every existing object."))
 ((name . "cos") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "define")
  (signature
   syntax-rules
   ()
   ((_ variable expression))
   ((_ variable))
   ((_ (variable parameter1 ...) body))
   ((_ (variable parameter1 ... . parameter) body)))
  (desc . "The define form described in this section is a <definition>used to create variable bindings and may appear anywhere other definitions may appear.
The first from of define binds <variable> to a new location before assigning the value of <expression> to it. The continuation of <expression> should not be invoked more than once. Implementation responsibilities: Implementations should detect that the continuation of <expression> is invoked more than once. If the implementation detects this, it must raise an exception with condition type &assertion.
The second form of define is equivalent to (define <variable> <unspecified>) where <unspecified> is a side-effect-free expression returning an unspecified value.
In the third form of define, <formals> must be either a sequence of zero or more variables, or a sequence of one or more variables followed by a dot . and another variable (as in a lambda expression, see section 11.4.2). This form is equivalent to (define <variable>   (lambda (<formals>) <body>)).
In the fourth form of define, <formal> must be a single variable. This form is equivalent to (define <variable> (lambda <formal> <body>))."))
 ((name . "define-syntax")
  (signature syntax-rules () ((_ keyword transformer-spec)))
  (subsigs (transformer-spec (value transformer-spec)))
  (desc . "Binds <keyword> to the value of <expression>, which must evaluate, at macro-expansion time, to a transformer. Macro transformers can be created using the syntax-rules and identifier-syntax forms described in section 11.19. See library section on “Transformers” for a more complete description of transformers. Keyword bindings established by define-syntax are visible throughout the body in which they appear, except where shadowed by other bindings, and nowhere else, just like variable bindings established by define. All bindings established by a set of definitions, whether keyword or variable definitions, are visible within the definitions themselves.
Implementation responsibilities: The implementation should detect if the value of <expression> cannot possibly be a transformer."))
 ((name . "denominator")
  (signature lambda ((rational? q)) integer?)
  (tags pure)
  (desc . "The procedure returns the denominator of its argument; the result is computed as if the argument was represented as a fraction in lowest terms. The denominator is always positive. The denominator of 0 is defined to be 1."))
 ((name . "div")
  (signature lambda ((real? x1) (real? x2)) integer?)
  (tags pure)
  (desc . "Number-theoretic integer division and return the results of the corresponding mathematical operations specified in section 11.7.3.1. x1 must be neither infinite nor a NaN, and x2 must be nonzero; otherwise, an exception with condition type &assertion is raised."))
 ((name . "div0")
  (signature lambda ((real? x1) (real? x2)) integer?)
  (tags pure)
  (desc . "Number-theoretic integer division and return the results of the corresponding mathematical operations specified in section 11.7.3.1. x1 must be neither infinite nor a NaN, and x2 must be nonzero; otherwise, an exception with condition type &assertion is raised."))
 ((name . "div-and-mod")
  (signature lambda ((real? x1) (real? x2)) (values integer? real?))
  (tags pure)
  (desc . "Number-theoretic integer division and return the results of the corresponding mathematical operations specified in section 11.7.3.1. x1 must be neither infinite nor a NaN, and x2 must be nonzero; otherwise, an exception with condition type &assertion is raised."))
 ((name . "div0-and-mod0")
  (signature lambda ((real? x1) (real? x2)) (values integer? real?))
  (tags pure)
  (desc . "Number-theoretic integer division and return the results of the corresponding mathematical operations specified in section 11.7.3.1. x1 must be neither infinite nor a NaN, and x2 must be nonzero; otherwise, an exception with condition type &assertion is raised."))
 ((name . "dynamic-wind")
  (signature
   lambda
   ((procedure? before) (procedure? thunk) (procedure? after))
   *)
  (subsigs
   (before (lambda () undefined))
   (thunk (lambda () *))
   (after (lambda () undefined)))
  (desc . "Before, thunk, and after must be procedures, and each should accept zero arguments. These procedures may return any number of values. The dynamic-wind procedure calls thunk without arguments, returning the results of this call. Moreover, dynamic-wind calls before without arguments whenever the dynamic extent of the call to thunk is entered, and after without arguments whenever the dynamic extent of the call to thunk is exited. Thus, in the absence of calls to escape procedures created by call-with-current-continuation, dynamic-wind calls before, thunk, and after, in that order.
While the calls to before and after are not considered to be within the dynamic extent of the call to thunk, calls to the before and after procedures of any other calls to dynamic-wind that occur within the dynamic extent of the call to thunk are considered to be within the dynamic extent of the call to thunk.
More precisely, an escape procedure transfers control out of the dynamic extent of a set of zero or more active dynamic-wind calls x ... and transfer control into the dynamic extent of a set of zero or more active dynamic-wind calls y .... It leaves the dynamic extent of the most recent x and calls without arguments the corresponding after procedure. If the after procedure returns, the escape procedure proceeds to the next most recent x, and so on. Once each x has been handled in this manner, the escape procedure calls without arguments the before procedure corresponding to the least recent y. If the before procedure returns, the escape procedure reenters the dynamic extent of the least recent y and proceeds with the next least recent y, and so on. Once each y has been handled in this manner, control is transferred to the continuation packaged in the escape procedure.
Implementation responsibilities: The implementation must check the restrictions on thunk and after only if they are actually called."))
 ((name . "eq?")
  (signature lambda (obj1 obj2) boolean?)
  (tags pure)
  (desc . "The eq? predicate is similar to eqv? except that in some cases it is capable of discerning distinctions finer than those detectable by eqv?.
The eq? and eqv? predicates are guaranteed to have the same behavior on symbols, booleans, the empty list, pairs, procedures, non-empty strings, bytevectors, and vectors, and records. The behavior of eq? on number objects and characters is implementation-dependent, but it always returns either #t or #f, and returns #t only when eqv? would also return #t. The eq? predicate may also behave differently from eqv? on empty vectors, empty bytevectors, and empty strings."))
 ((name . "equal?")
  (signature lambda (obj1 obj2) boolean?)
  (tags pure)
  (desc . " The equal? predicate returns #t if and only if the (possibly infinite) unfoldings of its arguments into regular trees are equal as ordered trees.
The equal? predicate treats pairs and vectors as nodes with outgoing edges, uses string=? to compare strings, uses bytevector=? to compare bytevectors (see library chapter on “Bytevectors”), and uses eqv? to compare other nodes."))
 ((name . "eqv?")
  (signature lambda (obj1 obj2) boolean?)
  (tags pure)
  (desc . "The eqv? procedure defines a useful equivalence relation on objects. Briefly, it returns #t if obj1 and obj2 should normally be regarded as the same object and #f otherwise. This relation is left slightly open to interpretation, but the following partial specification of eqv? must hold for all implementations.
The eqv? procedure returns #t if one of the following holds:
* Obj1 and obj2 are both booleans and are the same according to the boolean=? procedure (section 11.8).
* Obj1 and obj2 are both symbols and are the same according to the symbol=? procedure (section 11.10).
* Obj1 and obj2 are both exactnumber objects and are numerically equal (see =, section 11.7).
* Obj1 and obj2 are both inexactnumber objects, are numerically equal (see =, section 11.7), and yield the same results (in the sense of eqv?) when passed as arguments to any other procedure that can be defined as a finite composition of Scheme's standard arithmetic procedures.
* Obj1 and obj2 are both characters and are the same character according to the char=? procedure (section 11.11).
* Both obj1 and obj2 are the empty list.
* Obj1 and obj2 are objects such as pairs, vectors, bytevectors (library chapter on “Bytevectors”), strings, hashtables, records (library chapter on “Records”), ports (library section on “Port I/O”), or hashtables (library chapter on “Hash tables”) that refer to the same locations in the store (section 5.10).
* Obj1 and obj2 are record-type descriptors that are specified to be eqv? in library section on “Procedural layer”.

The eqv? procedure returns #f if one of the following holds:
* Obj1 and obj2 are of different types (section 11.1).
* Obj1 and obj2 are booleans for which the boolean=? procedure returns #f.
* Obj1 and obj2 are symbols for which the symbol=? procedure returns #f.
* One of obj1 and obj2 is an exact number object but the other is an inexact number object.
* Obj1 and obj2 are rational number objects for which the = procedure returns #f.
* Obj1 and obj2 yield different results (in the sense of eqv?) when passed as arguments to any other procedure that can be defined as a finite composition of Scheme's standard arithmetic procedures.
* Obj1 and obj2 are characters for which the char=? procedure returns #f.
* One of obj1 and obj2 is the empty list, but the other is not.
* Obj1 and obj2 are objects such as pairs, vectors, bytevectors (library chapter on “Bytevectors”), strings, records (library chapter on “Records”), ports (library section on “Port I/O”), or hashtables (library chapter on “Hashtables”) that refer to distinct locations.
* Obj1 and obj2 are pairs, vectors, strings, or records, or hashtables, where the applying the same accessor (i.e. car, cdr, vector-ref, string-ref, or record accessors) to both yields results for which eqv? returns #f.
* Obj1 and obj2 are procedures that would behave differently (return different values or have different side effects) for some arguments.

Note:‌ The eqv? procedure returning #t when obj1 and obj2 are number objects does not imply that = would also return #t when called with obj1 and obj2 as arguments. "))
 ((name . "error")
  (signature
   lambda
   (((or string? symbol? #f) who) (string? message) irritant1 ...)
   undefined)
  (desc . "The error procedure should be called when an error has occurred, typically caused by something that has gone wrong in the interaction of the program with the external world or the user.
The who argument should describe the procedure or operation that detected the exception. The message argument should describe the exceptional situation. The irritants should be the arguments to the operation that detected the operation.
The condition object provided with the exception (see library chapter on “Exceptions and conditions”) has the following condition types:
* If who is not #f, the condition has condition type &who, with who as the value of its field. In that case, who should be the name of the procedure or entity that detected the exception. If it is #f, the condition does not have condition type &who.
* The condition has condition type &message, with message as the value of its field.
* The condition has condition type &irritants, and its field has as its value a list of the irritants.

Moreover, the condition created by error has condition type &error."))
 ((name . "even?")
  (signature lambda ((integer? n)) boolean?)
  (tags pure predicate))
 ((name . "exact")
  (signature lambda ((number? z)) exact?)
  (tags pure)
  (desc . "The exact procedure returns an exact representation of z. The value returned is the exact number object that is numerically closest to the argument; in most cases, the result of this procedure should be numerically equal to its argument. If an inexact argument has no reasonably close exact equivalent, an exception with condition type &implementation-violation may be raised."))
 ((name . "exact-integer-sqrt")
  (signature lambda ((integer? k)) (values integer? integer?))
  (tags pure)
  (desc . "The exact-integer-sqrt procedure returns two non-negative exact integer objects s and r where k = s^2 + r and k < (s + 1)^2."))
 ((name . "exact?")
  (signature lambda ((number? z)) boolean?)
  (tags pure)
  (supertypes number?)
  (desc . "Tests for the exactness of a quantity. For any number object, precisely one of exact? or inexact? predicates is true."))
 ((name . "exp")
  (signature lambda ((number? z)) number?)
  (tags pure)
  (desc . "The exp procedure computes the base-e exponential of z."))
 ((name . "expt")
  (signature lambda ((number? z1) (number? z2)) number?)
  (tags pure)
  (desc . "Returns z1 raised to the power z2. For nonzero z1, this is ez2 log z1. 0.0z is 1.0 if z = 0.0, and 0.0 if (real-part z) is positive. For other cases in which the first argument is zero, either an exception is raised with condition type &implementation-restriction, or an unspecified number object is returned.
For an exact real number object z1 and an exact integer object z2, (expt z1 z2) must return an exact result. For all other values of z1 and z2, (expt z1 z2) may return an inexact result, even when both z1 and z2 are exact."))
 ((name . "finite?") (signature lambda ((number? z)) boolean?) (tags pure))
 ((name . "floor")
  (signature lambda ((real? x)) integer?)
  (tags pure)
  (desc . "floor returns the largest integer object not larger than x."))
 ((name . "for-each")
  (signature
   lambda
   ((procedure? proc) (list? list1) (list? list2) ...)
   undefined)
  (subsigs (proc (lambda (obj1 obj2 ...) undefined)))
  (desc . "The lists should all have the same length. Proc should accept as many arguments as there are lists. Proc should not mutate any of the lists.
The for-each procedure applies proc element-wise to the elements of the lists for its side effects, in order from the first elements to the last. Proc is always called in the same dynamic environment as for-each itself. The return values of for-each are unspecified."))
 ((name . "gcd")
  (signature lambda ((integer? n) ...) integer?)
  (tags pure)
  (desc . "Returns the greatest common divisor of its arguments. The result is always non-negative."))
 ((name . "identifier-syntax")
  (signature
   syntax-rules
   (set!)
   ((_ template) transformer-spec)
   ((_ (id1 template1) ((set! id2 pattern) template2))))
  (subsigs
   (pattern
    (pattern
     _
     identifier
     constant
     (pattern ...)
     (pattern pattern ... . pattern)
     (pattern ... pattern ellipsis pattern ...)
     (pattern ... pattern ellipsis pattern ... . pattern)
     (_append "#" (pattern ...))
     (_append "#" (pattern ... pattern ellipsis pattern ...))))
   (template
    (pattern
     identifier
     constant
     (element ...)
     (element element ... . template)
     (ellipsis template)
     (_append "#" (element ...)))))
  (desc . "Semantics: When a keyword is bound to a transformer produced by the first form of identifier-syntax, references to the keyword within the scope of the binding are replaced by <template>.
The second, more general, form of identifier-syntax permits the transformer to determine what happens when set! is used. In this case, uses of the identifier by itself are replaced by <template1>, and uses of set! with the identifier are replaced by <template2>."))
 ((name . "if")
  (signature
   syntax-rules
   ()
   ((_ test consequent))
   ((_ test consequent alternate)))
  (desc . "An if expression is evaluated as follows: first, <test> is evaluated. If it yields a true value(see section 5.7), then <consequent> is evaluated and its values are returned. Otherwise <alternate> is evaluated and its values are returned. If <test> yields #f and no <alternate> is specified, then the result of the expression is unspecified."))
 ((name . "imag-part") (signature lambda ((complex? z)) real?) (tags pure))
 ((name . "inexact")
  (signature lambda ((number? z)) inexact?)
  (tags pure)
  (desc . "The inexact procedure returns an inexact representation of z. If inexact number objects of the appropriate type have bounded precision, then the value returned is an inexact number object that is nearest to the argument. If an exact argument has no reasonably close inexact equivalent, an exception with condition type &implementation-violation may be raised."))
 ((name . "inexact?")
  (signature lambda ((number? z)) boolean?)
  (tags pure)
  (supertypes number?)
  (desc . "Tests for the exactness of a quantity. For any number object, precisely one of exact? or inexact? predicates is true."))
 ((name . "infinite?") (signature lambda ((number? z)) boolean?) (tags pure))
 ((name . "integer-valued?")
  (signature lambda (obj) boolean?)
  (tags pure)
  (desc . "integer-valued? procedures return #t if the object is a number object and is equal in the sense of = to some integer"))
 ((name . "integer->char")
  (signature lambda ((integer? n)) char?)
  (tags pure)
  (desc . "For a Unicode scalar value, integer->char returns its associated character."))
 ((name . "integer?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes rational?))
 ((name . "lambda")
  (signature syntax-rules () ((_ formals body) procedure?))
  (subsigs
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))
  (desc . "Semantics: A lambda expression evaluates to a procedure. The environment in effect when the lambda expression is evaluated is remembered as part of the procedure. When the procedure is later called with some arguments, the environment in which the lambda expression was evaluated is extended by binding the variables in the parameter list to fresh locations, and the resulting argument values are stored in those locations. Then, the expressions in the body of the lambda expression (which may contain definitions and thus represent a letrec* form, see section 11.3) are evaluated sequentially in the extended environment. The results of the last expression in the body are returned as the results of the procedure call.
 <Formals> must have one of the following forms:
* (<variable1> ...): The procedure takes a fixed number of arguments; when the procedure is called, the arguments are stored in the bindings of the corresponding variables.
* <variable>: The procedure takes any number of arguments; when the procedure is called, the sequence of arguments is converted into a newly allocated list, and the list is stored in the binding of the <variable>.
* (<variable1> ... <variablen> . <variablen+1>): If a period . precedes the last variable, then the procedure takes n or more arguments, where n is the number of parameters before the period (there must be at least one). The value stored in the binding of the last variable is a newly allocated list of the arguments left over after all the other arguments have been matched up against the other parameters.
Any <variable> must not appear more than once in <formals>."))
 ((name . "lcm")
  (signature lambda ((integer? n) ...) integer?)
  (tags pure)
  (desc . "Returns the least common multiple of its arguments. The result is always non-negative."))
 ((name . "length")
  (signature lambda ((list? list)) integer?)
  (tags pure)
  (desc . "Returns the length of list."))
 ((name . "let")
  (signature
   syntax-rules
   ()
   ((_ ((var1 init1) ...) body))
   ((_ name ((var1 init1) ...) body)))
  (desc . "The <init>s are evaluated in the current environment (in some unspecified order), the <variable>s are bound to fresh locations holding the results, the <body> is evaluated in the extended environment, and the values of the last expression of <body> are returned. Each binding of a <variable> has <body> as its region.
“Named let” is a variant on the syntax of let that provides a general looping construct and may also be used to express recursion. It has the same syntax and semantics as ordinary let except that <variable> is bound within <body> to a procedure whose parameters are the bound variables and whose body is <body>. Thus the execution of <body> may be repeated by invoking the procedure named by <variable>."))
 ((name . "let*")
  (signature syntax-rules () ((_ bindings body)))
  (subsigs (bindings (pattern ((variable1 init1) ...))))
  (desc . "The let* form is similar to let, but the <init>s are evaluated and bindings created sequentially from left to right, with the regionof each binding including the bindings to its right as well as <body>. Thus the second <init> is evaluated in an environment in which the first binding is visible and initialized, and so on."))
 ((name . "let*-values")
  (signature syntax-rules () ((_ mv-binding-spec body)))
  (subsigs
   (mv-binding-spec (pattern ((formals1 init1) ...)))
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))
  (desc . "The let*-values form is similar to let-values, but the <init>s are evaluated and bindings created sequentially from left to right, with the regionof the bindings of each <formals> including the bindings to its right as well as <body>. Thus the second <init> is evaluated in an environment in which the bindings of the first <formals> is visible and initialized, and so on."))
 ((name . "let-syntax")
  (signature syntax-rules () ((_ bindings form ...)))
  (subsigs
   (bindings (pattern ((keyword transformer-spec) ...)))
   (transformer-spec (value transformer-spec)))
  (desc . "Each <keyword> is an identifier, and each <expression> is an expression that evaluates, at macro-expansion time, to a transformer. Transformers may be created by syntax-rules or identifier-syntax (see section 11.19) or by one of the other mechanisms described in library chapter on “syntax-case”. It is a syntax violation for <keyword> to appear more than once in the list of keywords being bound.
Semantics: The <form>s are expanded in the syntactic environment obtained by extending the syntactic environment of the let-syntax form with macros whose keywords are the <keyword>s, bound to the specified transformers. Each binding of a <keyword> has the <form>s as its region.
The <form>s of a let-syntax form are treated, whether in definition or expression context, as if wrapped in an implicit begin; see section 11.4.7. Thus definitions in the result of expanding the <form>s have the same region as any definition appearing in place of the let-syntax form would have.
Implementation responsibilities: The implementation should detect if the value of <expression> cannot possibly be a transformer."))
 ((name . "let-values")
  (signature syntax-rules () ((_ mv-binding-spec body)))
  (subsigs
   (mv-binding-spec (pattern ((formals1 init1) ...)))
   (formals
    (pattern
     (variable1 ...)
     variable
     (variable1 ... variable_n . variable_n+1))))
  (desc . "The <init>s are evaluated in the current environment (in some unspecified order), and the variables occurring in the <formals> are bound to fresh locations containing the values returned by the <init>s, where the <formals> are matched to the return values in the same way that the <formals> in a lambda expression are matched to the arguments in a procedure call. Then, the <body> is evaluated in the extended environment, and the values of the last expression of <body> are returned. Each binding of a variable has <body> as its region.If the <formals> do not match, an exception with condition type &assertion is raised."))
 ((name . "letrec")
  (signature syntax-rules () ((_ bindings body)))
  (subsigs (bindings (pattern ((variable1 init1) ...))))
  (desc . "The <variable>s are bound to fresh locations, the <init>s are evaluated in the resulting environment (in some unspecified order), each <variable> is assigned to the result of the corresponding <init>, the <body> is evaluated in the resulting environment, and the values of the last expression in <body> are returned. Each binding of a <variable> has the entire letrec expression as its region, making it possible to define mutually recursive procedures."))
 ((name . "letrec*")
  (signature syntax-rules () ((_ bindings body)))
  (subsigs (bindings (pattern ((variable1 init1) ...))))
  (desc . "The <variable>s are bound to fresh locations, each <variable> is assigned in left-to-right order to the result of evaluating the corresponding <init>, the <body> is evaluated in the resulting environment, and the values of the last expression in <body> are returned. Despite the left-to-right evaluation and assignment order, each binding of a <variable> has the entire letrec* expression as its region, making it possible to define mutually recursive procedures."))
 ((name . "letrec-syntax")
  (signature syntax-rules () ((_ bindings form ...)))
  (subsigs (bindings (pattern ((keyword transformer-spec) ...))))
  (desc . "The <form>s of a letrec-syntax form are treated, whether in definition or expression context, as if wrapped in an implicit begin; see section 11.4.7. Thus definitions in the result of expanding the <form>s have the same region as any definition appearing in place of the letrec-syntax form would have.
Implementation responsibilities: The implementation should detect if the value of <expression> cannot possibly be a transformer."))
 ((name . "list")
  (signature lambda (obj ...) list?)
  (tags pure)
  (desc . "Returns a newly allocated list of its arguments."))
 ((name . "list->string")
  (signature lambda ((list? list)) string?)
  (subsigs
   (list (list char?)))
  (tags pure)
  (desc . "The list->string procedure returns a newly allocated string formed from the characters in list."))
 ((name . "list->vector")
  (signature lambda ((list? list)) vector?)
  (tags pure)
  (desc . "The list->vector procedure returns a newly created vector initialized to the elements of the list list."))
 ((name . "list-ref")
  (signature lambda ((list? list) (integer? k)) *)
  (tags pure)
  (desc . "List must be a list whose length is at least k + 1. The list-tail procedure returns the kth element of list."))
 ((name . "list-tail")
  (signature lambda ((list? list) (integer? k)) list?)
  (tags pure)
  (desc . "List should be a list of size at least k. The list-tail procedure returns the subchain of pairs of list obtained by omitting the first k elements."))
 ((name . "list?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a list, #f otherwise. By definition, all lists are chains of pairs that have finite length and are terminated by the empty list."))
 ((name . "log")
  (signature
   case-lambda
   (((number? z)) number?)
   (((number? z1) (number? z2)) number?))
  (tags pure)
  (desc . "The log procedure with a single argument computes the natural logarithm of z (not the base-ten logarithm); (log z1 z2) computes the base-z2 logarithm of z1."))
 ((name . "magnitude") (signature lambda ((complex? z)) real?) (tags pure))
 ((name . "make-polar")
  (signature lambda ((real? x3) (real? x4)) complex?)
  (tags pure))
 ((name . "make-rectangular")
  (signature lambda ((real? x1) (real? x2)) complex?)
  (tags pure))
 ((name . "make-string")
  (signature
   case-lambda
   (((integer? k)) string?)
   (((integer? k) (char? char)) string?))
  (tags pure)
  (desc . "Returns a newly allocated string of length k. If char is given, then all elements of the string are initialized to char, otherwise the contents of the string are unspecified."))
 ((name . "make-vector")
  (signature
   case-lambda
   (((integer? k)) vector?)
   (((integer? k) fill) vector?))
  (tags pure)
  (desc . "Returns a newly allocated vector of k elements. If a second argument is given, then each element is initialized to fill. Otherwise the initial contents of each element is unspecified."))
 ((name . "map")
  (signature lambda ((procedure? proc) (list? list1) (list? list2) ...) list?)
  (subsigs (proc (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . " The lists should all have the same length. Proc should accept as many arguments as there are lists and return a single value. Proc should not mutate any of the lists.
The map procedure applies proc element-wise to the elements of the lists and returns a list of the results, in order. Proc is always called in the same dynamic environment as map itself. The order in which proc is applied to the elements of the lists is unspecified. If multiple returns occur from map, the values returned by earlier returns are not mutated."))
 ((name . "max")
  (signature lambda ((real? x1) (real? x2) ...) real?)
  (tags pure)
  (desc . "Returns the maximum of its arguments."))
 ((name . "min")
  (signature lambda ((real? x1) (real? x2) ...) real?)
  (tags pure)
  (desc . "Returns the minimum of its arguments."))
 ((name . "mod")
  (signature lambda ((real? x1) (real? x2)) real?)
  (tags pure)
  (desc . "Number-theoretic integer division and return the results of the corresponding mathematical operations specified in section 11.7.3.1. x1 must be neither infinite nor a NaN, and x2 must be nonzero; otherwise, an exception with condition type &assertion is raised."))
 ((name . "mod0")
  (signature lambda ((real? x1) (real? x2)) real?)
  (tags pure)
  (desc . "Number-theoretic integer division and return the results of the corresponding mathematical operations specified in section 11.7.3.1. x1 must be neither infinite nor a NaN, and x2 must be nonzero; otherwise, an exception with condition type &assertion is raised."))
 ((name . "nan?") (signature lambda ((number? z)) boolean?) (tags pure))
 ((name . "negative?") (signature lambda ((real? x)) boolean?) (tags pure))
 ((name . "not")
  (signature lambda (obj) boolean?)
  (tags pure)
  (desc . "Returns #t if obj is #f, and returns #f otherwise."))
 ((name . "null?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is the empty list, #f otherwise."))
 ((name . "number->string")
  (signature
   case-lambda
   (((number? z)) string?)
   (((number? z) (integer? radix)) string?)
   (((number? z) (integer? radix) (integer? precision)) string?))
  (tags pure)
  (desc . "Radix must be an exact integer object, either 2, 8, 10, or 16. If omitted, radix defaults to 10. If a precision is specified, then z must be an inexact complex number object, precision must be an exact positive integer object, and radix must be 10. The number->string procedure takes a number object and a radix and returns as a string an external representation of the given number object in the given radix such that
(let ((number z) (radix radix)) (eqv? (string->number (number->string number radix) radix) number))
is true. If no possible result makes this expression true, an exception with condition type &implementation-restriction is raised.
Note:‌ The error case can occur only when z is not a complex number object or is a complex number object with a non-rational real or imaginary part.
If a precision is specified, then the representations of the inexact real components of the result, unless they are infinite or NaN, specify an explicit <mantissa width> p, and p is the least p ≥ precision for which the above expression is true.
If z is inexact, the radix is 10, and the above expression and condition can be satisfied by a result that contains a decimal point, then the result contains a decimal point and is expressed using the minimum number of digits (exclusive of exponent, trailing zeroes, and mantissa width) needed to make the above expression and condition true [4, 7]; otherwise the format of the result is unspecified.
The result returned by number->string never contains an explicit radix prefix."))
 ((name . "number?") (signature lambda (obj) boolean?) (tags pure predicate))
 ((name . "numerator") (signature lambda ((rational? q)) integer?) (tags pure))
 ((name . "odd?")
  (signature lambda ((integer? n)) boolean?)
  (tags pure predicate))
 ((name . "pair?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a pair, and otherwise returns #f."))
 ((name . "positive?") (signature lambda ((real? x)) boolean?) (tags pure))
 ((name . "procedure?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a procedure, otherwise returns #f."))
 ((name . "quasiquote")
  (signature syntax-rules () ((_ qq-template)))
  (desc . "“Backquote” or “quasiquote”expressions are useful for constructing a list or vector structure when some but not all of the desired structure is known in advance.
Syntax: <Qq template> should be as specified by the grammar at the end of this entry.
Semantics: If no unquote or unquote-splicing forms appear within the <qq template>, the result of evaluating (quasiquote <qq template>) is equivalent to the result of evaluating (quote <qq template>).
If an (unquote <expression> ...) form appears inside a <qq template>, however, the <expression>s are evaluated (“unquoted”) and their results are inserted into the structure instead of the unquote form.
If an (unquote-splicing <expression> ...) form appears inside a <qq template>, then the <expression>s must evaluate to lists; the opening and closing parentheses of the lists are then “stripped away” and the elements of the lists are inserted in place of the unquote-splicing form.
Any unquote-splicing or multi-operand unquote form must appear only within a list or vector <qq template>.
As noted in section 4.3.5, (quasiquote <qq template>) may be abbreviated `<qq template>, (unquote <expression>) may be abbreviated ,<expression>, and (unquote-splicing <expression>) may be abbreviated ,@<expression>.
Quasiquote forms may be nested. Substitutions are made only for unquoted components appearing at the same nesting level as the outermost quasiquote. The nesting level increases by one inside each successive quasiquotation, and decreases by one inside each unquotation.
A quasiquote expression may return either fresh, mutable objects or literal structure for any structure that is constructed at run time during the evaluation of the expression. Portions that do not need to be rebuilt are always literal. Thus,

(let ((a 3)) `((1 2) ,a ,4 ,'five 6))

may be equivalent to either of the following expressions:

'((1 2) 3 4 five 6)
(let ((a 3)) 
  (cons '(1 2)
        (cons a (cons 4 (cons 'five '(6))))))

However, it is not equivalent to this expression:
(let ((a 3)) (list (list 1 2) a 4 'five 6))

It is a syntax violation if any of the identifiers quasiquote, unquote, or unquote-splicing appear in positions within a <qq template> otherwise than as described above.
In <quasiquotation>s, a <list qq template D> can sometimes be confused with either an <unquotation D> or a <splicing unquotation D>. The interpretation as an <unquotation> or <splicing unquotation D> takes precedence."))
 ((name . "quote")
  (signature syntax-rules () ((_ datum)))
  (desc . "(quote <datum>) evaluates to the datum value represented by <datum> (see section 4.3). This notation is used to include constants. As noted in section 4.3.5, (quote <datum>) may be abbreviated as '<datum>.  As noted in section 5.10, constants are immutable.
Note:‌ Different constants that are the value of a quote expression may share the same locations. "))
 ((name . "rational-valued?")
  (signature lambda (obj) boolean?)
  (tags pure)
  (desc . "Return #t if the object is a number object and is equal in the sense of = to some rational number."))
 ((name . "rational?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes real?))
 ((name . "rationalize")
  (signature lambda ((real? x1) (real? x2)) rational?)
  (tags pure)
  (desc . "The rationalize procedure returns the a number object representing the simplest rational number differing from x1 by no more than x2. A rational number r1 is simpler than another rational number r2 if r1 = p1/q1 and r2 = p2/q2 (in lowest terms) and |p1| ≤ |p2| and |q1| ≤ |q2|. Thus 3/5 is simpler than 4/7. Although not all rationals are comparable in this ordering (consider 2/7 and 3/5) any interval contains a rational number that is simpler than every other rational number in that interval (the simpler 2/5 lies between 2/7 and 3/5). Note that 0 = 0/1 is the simplest rational of all."))
 ((name . "real?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes complex?))
 ((name . "real-part") (signature lambda ((complex? z)) real?) (tags pure))
 ((name . "real-valued?")
  (signature lambda (obj) boolean?)
  (tags pure)
  (desc . " The real-valued? procedure returns #t if the object is a number object and is equal in the sense of = to some real number object, or if the object is a NaN, or a complex number object whose real part is a NaN and whose imaginary part is zero in the sense of zero?."))
 ((name . "reverse")
  (signature lambda ((list? list)) list?)
  (tags pure)
  (desc . "Returns a newly allocated list consisting of the elements of list in reverse order."))
 ((name . "round")
  (signature lambda ((real? x)) integer?)
  (tags pure)
  (desc . "The round procedure returns the closest integer object to x, rounding to even when x represents a number halfway between two integers."))
 ((name . "set!")
  (signature syntax-rules () ((_ variable expression)))
  (desc . "<Expression> is evaluated, and the resulting value is stored in the location to which <variable> is bound. <Variable> must be bound either in some regionenclosing the set! expression or at the top level. The result of the set! expression is unspecified.
It is a syntax violation if <variable> refers to an immutable binding."))
 ((name . "sin") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "sqrt")
  (signature lambda ((number? z)) number?)
  (tags pure)
  (desc . "Returns the principal square root of z. For rational z, the result has either positive real part, or zero real part and non-negative imaginary part. With log defined as in section 11.7.3.2, the value of (sqrt z) could be expressed as e^(log z/2).
The sqrt procedure may return an inexact result even when given an exact argument."))
 ((name . "string")
  (signature lambda ((char? char) ...) string?)
  (tags pure)
  (desc . "Returns a newly allocated string composed of the arguments."))
 ((name . "string->list")
  (signature
   lambda ((string? string)) list?)
  (tags pure)
  (desc . "The string->list procedure returns a newly allocated list of the characters that make up the given string."))
 ((name . "string->number")
  (signature
   case-lambda
   (((string? string)) number?)
   (((string? string) (integer? radix)) number?))
  (tags pure)
  (desc . "Returns a number object with maximally precise representation expressed by the given string. Radix must be an exact integer object, either 2, 8, 10, or 16. If supplied, radix is a default radix that may be overridden by an explicit radix prefix in string (e.g., \"#o177\"). If radix is not supplied, then the default radix is 10. If string is not a syntactically valid notation for a number object or a notation for a rational number object with a zero denominator, then string->number returns #f."))
 ((name . "string->symbol")
  (signature lambda ((string? string)) symbol?)
  (tags pure)
  (desc . "Returns the symbol whose name is string."))
 ((name . "string-append")
  (signature lambda ((string? string) ...) string?)
  (tags pure)
  (desc . "Returns a newly allocated string whose characters form the concatenation of the given strings."))
 ((name . "string-copy")
  (signature
   lambda
   ((string? string)) string?)
  (tags pure)
  (desc . "Returns a newly allocated copy of the given string."))
 ((name . "string-for-each")
  (signature
   lambda
   ((procedure? proc) (string? string1) (string? string2) ...)
   undefined)
  (subsigs (proc (lambda ((string? string) ...) undefined)))
  (desc . "The strings must all have the same length. Proc should accept as many arguments as there are strings. The string-for-each procedure applies proc element-wise to the characters of the strings for its side effects, in order from the first characters to the last. Proc is always called in the same dynamic environment as string-for-each itself. The return values of string-for-each are unspecified.
Analogous to for-each.
Implementation responsibilities: The implementation must check the restrictions on proc to the extent performed by applying it as described. An implementation may check whether proc is an appropriate argument before applying it."))
 ((name . "string-length")
  (signature lambda ((string? string)) integer?)
  (tags pure)
  (desc . "Returns the number of characters in the given string as an exact integer object."))
 ((name . "string-ref")
  (signature lambda ((string? string) (integer? k)) char?)
  (tags pure)
  (desc . " K must be a valid index of string. The string-ref procedure returns character
k of string using zero-origin indexing.
Note:‌ Implementors should make string-ref run in constant time. "))
 ((name . "string<=?")
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure)
  (desc . "Lexicographic extension to strings of the corresponding orderings on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string is considered to be lexicographically less than the longer string."))
 ((name . "string<?")
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure)
  (desc . "Lexicographic extension to strings of the corresponding orderings on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string is considered to be lexicographically less than the longer string."))
 ((name . "string=?")
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure)
  (desc . "Returns #t if the strings are the same length and contain the same characters in the same positions. Otherwise, the string=? procedure returns #f."))
 ((name . "string>=?")
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure)
  (desc . "Lexicographic extension to strings of the corresponding orderings on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string is considered to be lexicographically less than the longer string."))
 ((name . "string>?")
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure)
  (desc . "Lexicographic extension to strings of the corresponding orderings on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string is considered to be lexicographically less than the longer string."))
 ((name . "string?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a string, otherwise returns #f."))
 ((name . "substring")
  (signature lambda ((string? string) (integer? start) (integer? end)) string?)
  (tags pure)
  (desc . " String must be a string, and start and end must be exact integer objects satisfying
0 ≤ start ≤ end ≤ (string-length string).

The substring procedure returns a newly allocated string formed from the characters of string beginning with index start (inclusive) and ending with index end (exclusive)."))
 ((name . "symbol->string")
  (signature lambda ((symbol? symbol)) string?)
  (tags pure)
  (desc . "Returns the name of symbol as an immutable string."))
 ((name . "symbol=?")
  (signature
   lambda
   ((symbol? symbol1) (symbol? symbol2) (symbol? symbol3) ...)
   boolean?)
  (tags pure)
  (desc . "Returns #t if the symbols are the same, i.e., if their names are spelled the same."))
 ((name . "symbol?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a symbol, otherwise returns #f."))
 ((name . "syntax-rules")
  (signature
   syntax-rules
   (_)
   ((_ (literal ...) syntax-rule ...) transformer-spec))
  (subsigs
   (syntax-rule (pattern (pattern template)))
   (pattern
    (pattern
     _
     identifier
     constant
     (pattern ...)
     (pattern pattern ... . pattern)
     (pattern ... pattern ellipsis pattern ...)
     (pattern ... pattern ellipsis pattern ... . pattern)
     (_append "#" (pattern ...))
     (_append "#" (pattern ... pattern ellipsis pattern ...))))
   (template
    (pattern
     identifier
     constant
     (element ...)
     (element element ... . template)
     (ellipsis template)
     (_append "#" (element ...))))
   (element (pattern template (_append template ellipsis))))
  (desc . "Semantics: An instance of syntax-rules evaluates, at macro-expansion time, to a new macro transformer by specifying a sequence of hygienic rewrite rules. A use of a macro whose keyword is associated with a transformer specified by syntax-rules is matched against the patterns contained in the <syntax rule>s, beginning with the leftmost <syntax rule>. When a match is found, the macro use is transcribed hygienically according to the template. It is a syntax violation when no match is found.
An identifier appearing within a <pattern> may be an underscore ( _ ), a literal identifier listed in the list of literals (<literal> ...), or an ellipsis ( ... ). All other identifiers appearing within a <pattern> are pattern variables. It is a syntax violation if an ellipsis or underscore appears in (<literal> ...).
While the first subform of <srpattern> may be an identifier, the identifier is not involved in the matching and is not considered a pattern variable or literal identifier.
Pattern variables match arbitrary input subforms and are used to refer to elements of the input. It is a syntax violation if the same pattern variable appears more than once in a <pattern>.
Underscores also match arbitrary input subforms but are not pattern variables and so cannot be used to refer to those elements. Multiple underscores may appear in a <pattern>.
A literal identifier matches an input subform if and only if the input subform is an identifier and either both its occurrence in the input expression and its occurrence in the list of literals have the same lexical binding, or the two identifiers have the same name and both have no lexical binding.
A subpattern followed by an ellipsis can match zero or more elements of the input.
More formally, an input form F matches a pattern P if and only if one of the following holds:

* P is an underscore ( _ ).
* P is a pattern variable.
* P is a literal identifier and F is an identifier such that both P and F would refer to the same binding if both were to appear in the output of the macro outside of any bindings inserted into the output of the macro. (If neither of two like-named identifiers refers to any binding, i.e., both are undefined, they are considered to refer to the same binding.)
* P is of the form (P1 ... Pn) and F is a list of n elements that match P1 through Pn.
* P is of the form (P1 ... Pn . Px) and F is a list or improper list of n or more elements whose first n elements match P1 through Pn and whose nth cdr matches Px.
* P is of the form (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn), where <ellipsis> is the identifier ... and F is a list of n elements whose first k elements match P1 through Pk, whose next m−k elements each match Pe, and whose remaining n−m elements match Pm+1 through Pn.
* P is of the form (P1 ... Pk Pe <ellipsis> Pm+1 ... Pn . Px), where <ellipsis> is the identifier ... and F is a list or improper list of n elements whose first k elements match P1 through Pk, whose next m−k elements each match Pe, whose next n−m elements match Pm+1 through Pn, and whose nth and final cdr matches Px.
* P is of the form #(P1 ... Pn) and F is a vector of n elements that match P1 through Pn.
* P is of the form #(P1 ... Pk Pe <ellipsis> Pm+1 ... Pn), where <ellipsis> is the identifier ... and F is a vector of n or more elements whose first k elements match P1 through Pk, whose next m−k elements each match Pe, and whose remaining n−m elements match Pm+1 through Pn.
* P is a pattern datum (any nonlist, nonvector, nonsymbol datum) and F is equal to P in the sense of the equal? procedure.

When a macro use is transcribed according to the template of the matching <syntax rule>, pattern variables that occur in the template are replaced by the subforms they match in the input.
Pattern data and identifiers that are not pattern variables or ellipses are copied into the output. A subtemplate followed by an ellipsis expands into zero or more occurrences of the subtemplate. Pattern variables that occur in subpatterns followed by one or more ellipses may occur only in subtemplates that are followed by (at least) as many ellipses. These pattern variables are replaced in the output by the input subforms to which they are bound, distributed as specified. If a pattern variable is followed by more ellipses in the subtemplate than in the associated subpattern, the input form is replicated as necessary. The subtemplate must contain at least one pattern variable from a subpattern followed by an ellipsis, and for at least one such pattern variable, the subtemplate must be followed by exactly as many ellipses as the subpattern in which the pattern variable appears. (Otherwise, the expander would not be able to determine how many times the subform should be repeated in the output.) It is a syntax violation if the constraints of this paragraph are not met.
A template of the form (<ellipsis> <template>) is identical to <template>, except that ellipses within the template have no special meaning. That is, any ellipses contained within <template> are treated as ordinary identifiers. In particular, the template (... ...) produces a single ellipsis, .... This allows syntactic abstractions to expand into forms containing ellipses."))
 ((name . "tan") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "truncate")
  (signature lambda ((real? x)) integer?)
  (tags pure)
  (desc . "The truncate procedure returns the integer object closest to x whose absolute value is not larger than the absolute value of x."))
 ((name . "unquote")
  (signature syntax-rules () ((_ expression)))
  (desc . "“Backquote” or “quasiquote”expressions are useful for constructing a list or vector structure when some but not all of the desired structure is known in advance.
Syntax: <Qq template> should be as specified by the grammar at the end of this entry.
Semantics: If no unquote or unquote-splicing forms appear within the <qq template>, the result of evaluating (quasiquote <qq template>) is equivalent to the result of evaluating (quote <qq template>).
If an (unquote <expression> ...) form appears inside a <qq template>, however, the <expression>s are evaluated (“unquoted”) and their results are inserted into the structure instead of the unquote form.
If an (unquote-splicing <expression> ...) form appears inside a <qq template>, then the <expression>s must evaluate to lists; the opening and closing parentheses of the lists are then “stripped away” and the elements of the lists are inserted in place of the unquote-splicing form.
Any unquote-splicing or multi-operand unquote form must appear only within a list or vector <qq template>.
As noted in section 4.3.5, (quasiquote <qq template>) may be abbreviated `<qq template>, (unquote <expression>) may be abbreviated ,<expression>, and (unquote-splicing <expression>) may be abbreviated ,@<expression>.
Quasiquote forms may be nested. Substitutions are made only for unquoted components appearing at the same nesting level as the outermost quasiquote. The nesting level increases by one inside each successive quasiquotation, and decreases by one inside each unquotation.
A quasiquote expression may return either fresh, mutable objects or literal structure for any structure that is constructed at run time during the evaluation of the expression. Portions that do not need to be rebuilt are always literal. Thus,

(let ((a 3)) `((1 2) ,a ,4 ,'five 6))

may be equivalent to either of the following expressions:

'((1 2) 3 4 five 6)
(let ((a 3)) 
  (cons '(1 2)
        (cons a (cons 4 (cons 'five '(6))))))

However, it is not equivalent to this expression:
(let ((a 3)) (list (list 1 2) a 4 'five 6))

It is a syntax violation if any of the identifiers quasiquote, unquote, or unquote-splicing appear in positions within a <qq template> otherwise than as described above.
In <quasiquotation>s, a <list qq template D> can sometimes be confused with either an <unquotation D> or a <splicing unquotation D>. The interpretation as an <unquotation> or <splicing unquotation D> takes precedence."))
 ((name . "unquote-splicing")
  (signature syntax-rules () ((_ expression)))
  (desc . "“Backquote” or “quasiquote”expressions are useful for constructing a list or vector structure when some but not all of the desired structure is known in advance.
Syntax: <Qq template> should be as specified by the grammar at the end of this entry.
Semantics: If no unquote or unquote-splicing forms appear within the <qq template>, the result of evaluating (quasiquote <qq template>) is equivalent to the result of evaluating (quote <qq template>).
If an (unquote <expression> ...) form appears inside a <qq template>, however, the <expression>s are evaluated (“unquoted”) and their results are inserted into the structure instead of the unquote form.
If an (unquote-splicing <expression> ...) form appears inside a <qq template>, then the <expression>s must evaluate to lists; the opening and closing parentheses of the lists are then “stripped away” and the elements of the lists are inserted in place of the unquote-splicing form.
Any unquote-splicing or multi-operand unquote form must appear only within a list or vector <qq template>.
As noted in section 4.3.5, (quasiquote <qq template>) may be abbreviated `<qq template>, (unquote <expression>) may be abbreviated ,<expression>, and (unquote-splicing <expression>) may be abbreviated ,@<expression>.
Quasiquote forms may be nested. Substitutions are made only for unquoted components appearing at the same nesting level as the outermost quasiquote. The nesting level increases by one inside each successive quasiquotation, and decreases by one inside each unquotation.
A quasiquote expression may return either fresh, mutable objects or literal structure for any structure that is constructed at run time during the evaluation of the expression. Portions that do not need to be rebuilt are always literal. Thus,

(let ((a 3)) `((1 2) ,a ,4 ,'five 6))

may be equivalent to either of the following expressions:

'((1 2) 3 4 five 6)
(let ((a 3)) 
  (cons '(1 2)
        (cons a (cons 4 (cons 'five '(6))))))

However, it is not equivalent to this expression:
(let ((a 3)) (list (list 1 2) a 4 'five 6))

It is a syntax violation if any of the identifiers quasiquote, unquote, or unquote-splicing appear in positions within a <qq template> otherwise than as described above.
In <quasiquotation>s, a <list qq template D> can sometimes be confused with either an <unquotation D> or a <splicing unquotation D>. The interpretation as an <unquotation> or <splicing unquotation D> takes precedence."))
 ((name . "values")
  (signature lambda (obj ...) (values * ...))
  (tags pure)
  (desc . "Delivers all of its arguments to its continuation.  The continuations of all non-final expressions within a sequence of expressions, such as in lambda, begin, let, let*, letrec, letrec*, let-values, let*-values, case, and cond forms, usually take an arbitrary number of values. Except for these and the continuations created by call-with-values, let-values, and let*-values, continuations implicitly accepting a single value, such as the continuations of <operator> and <operand>s of procedure calls or the <test> expressions in conditionals, take exactly one value. The effect of passing an inappropriate number of values to such a continuation is undefined."))
 ((name . "vector")
  (signature lambda (obj ...) vector?)
  (tags pure)
  (desc . "Returns a newly allocated vector whose elements contain the given arguments. Analogous to list."))
 ((name . "vector->list")
  (signature
   lambda ((vector? vector)) list?)
  (tags pure)
  (desc . "The vector->list procedure returns a newly allocated list of the objects contained in the elements of vector."))
 ((name . "vector-fill!")
  (signature
   lambda
   ((vector? vector) fill) undefined)
  (desc . "Stores fill in every element of vector and returns unspecified values."))
 ((name . "vector-for-each")
  (signature
   lambda
   ((procedure? proc) (vector? vector1) (vector? vector2) ...)
   undefined)
  (subsigs (proc (lambda (obj ...) undefined)))
  (desc . "The vectors must all have the same length. Proc should accept as many arguments as there are vectors. The vector-for-each procedure applies proc element-wise to the elements of the vectors for its side effects, in order from the first elements to the last. Proc is always called in the same dynamic environment as vector-for-each itself. The return values of vector-for-each are unspecified.
Analogous to for-each.
Implementation responsibilities: The implementation must check the restrictions on proc to the extent performed by applying it as described. An implementation may check whether proc is an appropriate argument before applying it. "))
 ((name . "vector-length")
  (signature lambda ((vector? vector)) integer?)
  (tags pure)
  (desc . "Returns the number of elements in vector as an exact integer object."))
 ((name . "vector-map")
  (signature
   lambda
   ((procedure? proc) (vector? vector1) (vector? vector2) ...)
   vector?)
  (subsigs (proc (lambda (obj ...) *)))
  (tags pure)
  (desc . "The vectors must all have the same length. Proc should accept as many arguments as there are vectors and return a single value.
The vector-map procedure applies proc element-wise to the elements of the vectors and returns a vector of the results, in order. Proc is always called in the same dynamic environment as vector-map itself. The order in which proc is applied to the elements of the vectors is unspecified. If multiple returns occur from vector-map, the return values returned by earlier returns are not mutated.
Analogous to map.
Implementation responsibilities: The implementation must check the restrictions on proc to the extent performed by applying it as described. An implementation may check whether proc is an appropriate argument before applying it. "))
 ((name . "vector-ref")
  (signature lambda ((vector? vector) (integer? k)) *)
  (tags pure)
  (desc . "K must be a valid index of vector. The vector-ref procedure returns the contents of element k of vector."))
 ((name . "vector-set!")
  (signature lambda ((vector? vector) (integer? k) obj) undefined)
  (desc . "K must be a valid index of vector. The vector-set! procedure stores obj in element k of vector, and returns unspecified values. Passing an immutable vector to vector-set! should cause an exception with condition type &assertion to be raised."))
 ((name . "vector?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a vector. Otherwise the procedure returns #f."))
 ((name . "zero?")
  (signature lambda ((number? z)) boolean?)
  (tags pure predicate)))
