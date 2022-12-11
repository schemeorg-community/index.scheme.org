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
  (desc . "With two or more arguments, this procedure returns the difference of its arguments, associating to the left. With one argument, however, it returns the additive inverse of its argument."))
 ((name . "/")
  (signature lambda ((number? z1) (number? z2) ...) number?)
  (tags pure)
  (desc . "With two or more arguments, this procedure returns the quotient of its arguments, associating to the left. With one argument, however, it returns the multiplicative inverse of its argument."))
 ((name . "<")
  (signature lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if its arguments are monotonically increasing."))
 ((name . "<=")
  (signature lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if its arguments are monotonically non-decreasing."))
 ((name . "=")
  (signature lambda ((number? z1) (number? z2) (number? z3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if its arguments are equal."))
 ((name . ">")
  (signature lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if its arguments are monotonically decreasing."))
 ((name . ">=")
  (signature lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
  (tags pure)
  (desc . "This procedure returns #t if its arguments are monotonically non-increasing."))
 ((name . "abs")
  (signature lambda ((real? x)) number?)
  (tags pure)
  (desc . "Abs returns the absolute value of its argument."))
 ((name . "and")
  (signature syntax-rules () ((_ test1 ...)))
  (desc . "The 〈test〉 expressions are evaluated from left to right, and the value of the first expression that evaluates to a false value (see section 6.3.1) is returned. Any remaining ex- pressions are not evaluated. If all the expressions evaluate to true values, the value of the last expression is returned. If there are no expressions then #t is returned."))
 ((name . "append")
  (signature case-lambda (((list? list) ...) list?) (((list? list) ... obj) *))
  (tags pure)
  (desc . "eturns a list consisting of the elements of the first list followed by the elements of the other lists.
The resulting list is always newly allocated, except that it shares structure with the last list argument. The last argument may actually be any object; an improper list results if the last argument is not a proper list."))
 ((name . "apply")
  (signature lambda ((procedure? proc) arg1 ... (list? args)) *)
  (tags pure)
  (desc . "Proc must be a procedure and args must be a list. Calls proc with the elements of the list (append (list arg1 . . . ) args) as the actual arguments."))
 ((name . "assoc")
  (signature lambda (obj (list? alist)) (or pair? #f))
  (tags pure)
  (desc . "Alist (for \"association list\") must be a list of pairs. These procedures find the first pair in alist whose car field is obj, and returns that pair. If no pair in alist has obj as its car,then #f (not the empty list) is returned. Assoc uses equal? to compare obj with the car fields of the pairs in alist."))
 ((name . "assq")
  (signature lambda (obj (list? alist)) (or pair? #f))
  (tags pure)
  (desc . "Alist (for \"association list\") must be a list of pairs. These procedures find the first pair in alist whose car field is obj, and returns that pair. If no pair in alist has obj as its car,then #f (not the empty list) is returned. Assq uses eq? to compare obj with the car fields of the pairs in alist."))
 ((name . "assv")
  (signature lambda (obj (list? alist)) (or pair? #f))
  (tags pure)
  (desc . "Alist (for \"association list\") must be a list of pairs. These procedures find the first pair in alist whose car field is obj, and returns that pair. If no pair in alist has obj as its car,then #f (not the empty list) is returned. Assv uses eqv? to compare obj with the car fields of the pairs in alist."))
 ((name . "begin")
  (signature syntax-rules () ((_ expression-or-definition ...)))
  (desc . "The 〈expression〉s are evaluated sequentially from left to right, and the value(s) of the last 〈expression〉 is(are) returned. This expression type is used to sequence side effects such as input and output."))
 ((name . "boolean?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Boolean? returns #t if obj is either #t or #f and returns #f otherwise."))
 ((name . "caar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car and car."))
 ((name . "cadr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car and cdr."))
 ((name . "call-with-current-continuation")
  (signature lambda ((procedure? proc)) *)
  (subsigs (proc (lambda ((procedure? k)) *)))
  (desc . "The procedure call-with-current-continuation packages up the current continuation (see the rationale below) as an \"escape procedure\" and passes it as an argument to proc. The escape procedure is a Scheme procedure that, if it is later called, will abandon whatever continuation is in effect at that later time and will instead use the continuation that was in effect when the escape procedure was created. Calling the escape procedure may cause the invocation of before and after thunks installed using dynamic-wind.
The escape procedure accepts the same number of arguments as the continuation to the original call to call-with-current-continuation. Except for continuations created by the call-with-values procedure, all continuations take exactly one value. The effect of passing no value or more than one value to continuations that were not created by call-with-values is unspecified.
The escape procedure that is passed to proc has unlimited extent just like any other procedure in Scheme. It may be stored in variables or data structures and may be called as many times as desired.
Rationale: A common use of call-with-current-continuation is for structured, non-local exits from loops or procedure bodies, but in fact call-with-current-continuation is extremely useful for implementing a wide variety of advanced control structures.
Whenever a Scheme expression is evaluated there is a continuation wanting the result of the expression. The continuation represents an entire (default) future for the computation. If the expression is evaluated at top level, for example, then the continuation might take the result, print it on the screen, prompt for the next input, evaluate it, and so on forever. Most of the time the continuation includes actions specified by user code, as in a continuation that will take the result, multiply it by the value stored in a local variable, add seven, and give the answer to the top level continuation to be printed. Normally these ubiquitous continuations are hidden behind the scenes and programmers do not think much about them. On rare occasions, however, a programmer may need to deal with continuations explicitly. Call-with-current-continuation allows Scheme programmers to do that by creating a procedure that acts just like the current continuation.
Most programming languages incorporate one or more special purpose escape constructs with names like exit, return, or even goto. In 1965, however, Peter Landin [16] invented a general purpose escape operator called the J-operator. John Reynolds [24] described a simpler but equally powerful construct in 1972. The catch special form described by Sussman and Steele in the 1975 report on Scheme is exactly the same as Reynolds’s construct, though its name came from a less general construct in MacLisp. Several Scheme implementors noticed that the full power of the catch construct could be provided by a procedure instead of by a special syntactic construct, and the name call-with-current-continuation was coined in 1982. This name is descriptive, but opinions differ on the merits of such a long name, and some people use the name call/cc instead."))
 ((name . "call-with-values")
  (signature lambda ((procedure? producer) (procedure? consumer)) *)
  (subsigs (producer (lambda () *)) (consumer (lambda (obj ...) *)))
  (tags pure)
  (desc . "Calls its producer argument with no values and a continuation that, when passed some values, calls the consumer procedure with those values as arguments. The continuation for the call to consumer is the continuation of the call to call-with-values."))
 ((name . "car")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Returns the contents of the car field of pair. Note that it is an error to take the car of the empty list."))
 ((name . "case")
  (signature syntax-rules (else) ((_ key clause1 clause2 ...)))
  (subsigs
   (clause
    (pattern
     ((datum1 ...) expression1 expression2 ...)
     (else expression1 expression2 ...))))
  (desc . "A case expression is evaluated as follows. 〈Key〉 is evaluated and its result is compared against each 〈datum〉. If the result of evaluating 〈key〉 is equivalent (in the sense of eqv?; see section 6.1) to a 〈datum〉, then the expressions in the corresponding 〈clause〉 are evaluated from left to right and the result(s) of the last expression in the 〈clause〉 is(are) returned as the result(s) of the case expression. If the result of evaluating 〈key〉 is different from every 〈datum〉, then if there is an else clause its expressions are evaluated and the result(s) of the last is(are) the result(s) of the case expression; otherwise the result of the case expression is unspecified."))
 ((name . "cdar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr and car"))
 ((name . "cddr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr and cdr"))
 ((name . "cdr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Returns the contents of the cdr field of pair . Note that it is an error to take the cdr of the empty list."))
 ((name . "ceiling")
  (signature lambda ((real? x)) real?)
  (tags pure)
  (desc . "Ceiling returns the smallest integer not smaller than x. Note: If the argument to one of these procedures is inexact,then the result will also be inexact. If an exact value is needed, the result should be passed to the inexact->exact procedure."))
 ((name . "char->integer")
  (signature lambda ((char? char)) integer?)
  (tags pure)
  (desc . "Given a character, char->integer returns an exact integer representation of the character. This procedure implements order-preserving isomorphisms between the set of characters under the char<=? ordering and some subset of the integers under the <= ordering"))
 ((name . "char-ready?")
  (signature case-lambda (() boolean?) (((input-port? port)) boolean?))
  (desc . "Returns #t if a character is ready on the input port and returns #f otherwise. If char-ready returns #t then the next read-char operation on the given port is guaranteed not to hang. If the port is at end of file then char-ready? returns #t. Port may be omitted, in which case it defaults to the value returned by current-input-port.
Rationale: Char-ready? exists to make it possible for a program to accept characters from interactive ports without getting stuck waiting for input. Any input editors associated with such ports must ensure that characters whose existence has been asserted by char-ready? cannot be rubbed out. If char-ready? were to return #f at end of file, a port at end of file would be indistinguishable from an interactive port that has no ready characters."))
 ((name . "char<=?")
  (signature lambda ((char? char1) (char? char2)) boolean?)
  (tags pure)
  (desc . "This procedure imposes a total ordering on the set of characters. It is guaranteed that under this ordering:
* The upper case characters are in order.
* The lower case characters are in order.
* The digits are in order.
* Either all the digits precede all the upper case letters,
or vice versa.
* Either all the digits precede all the lower case letters,
or vice versa.

Some implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicate."))
 ((name . "char<?")
  (signature lambda ((char? char1) (char? char2)) boolean?)
  (tags pure)
  (desc . "This procedure imposes a total ordering on the set of characters. It is guaranteed that under this ordering:
* The upper case characters are in order.
* The lower case characters are in order.
* The digits are in order.
* Either all the digits precede all the upper case letters,
or vice versa.
* Either all the digits precede all the lower case letters,
or vice versa.

Some implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicate."))
 ((name . "char=?")
  (signature lambda ((char? char1) (char? char2)) boolean?)
  (tags pure)
  (desc . "This procedure imposes a total ordering on the set of characters. It is guaranteed that under this ordering:
* The upper case characters are in order.
* The lower case characters are in order.
* The digits are in order.
* Either all the digits precede all the upper case letters,
or vice versa.
* Either all the digits precede all the lower case letters,
or vice versa.

Some implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicate."))
 ((name . "char>=?")
  (signature lambda ((char? char1) (char? char2)) boolean?)
  (tags pure)
  (desc . "This procedure imposes a total ordering on the set of characters. It is guaranteed that under this ordering:
* The upper case characters are in order.
* The lower case characters are in order.
* The digits are in order.
* Either all the digits precede all the upper case letters,
or vice versa.
* Either all the digits precede all the lower case letters,
or vice versa.

Some implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicate."))
 ((name . "char>?")
  (signature lambda ((char? char1) (char? char2)) boolean?)
  (tags pure)
  (desc . "This procedure imposes a total ordering on the set of characters. It is guaranteed that under this ordering:
* The upper case characters are in order.
* The lower case characters are in order.
* The digits are in order.
* Either all the digits precede all the upper case letters,
or vice versa.
* Either all the digits precede all the lower case letters,
or vice versa.

Some implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicate."))
 ((name . "char?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a character, otherwise returns #f."))
 ((name . "close-input-port")
  (signature lambda ((input-port? port)) undefined)
  (desc . "Closes the file associated with port, rendering the port incapable of delivering characters. These routine has no effect if the file has already been closed."))
 ((name . "close-output-port")
  (signature lambda ((output-port? port)) undefined)
  (desc . "Closes the file associated with port, rendering the port incapable of accepting characters. These routine has no effect if the file has already been closed."))
 ((name . "complex?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes number?)
  (desc . "The numerical type predicate can be applied to any kind of argument, including non-numbers. It returns #t if the object is of the named type, and otherwise they return #f. In general, if a type predicate is true of a number then all higher type predicates are also true of that number. Consequently, if a type predicate is false of a number, then all lower type predicates are also false of that number."))
 ((name . "cond")
  (signature syntax-rules (else =>) ((_ clause1 clause2 ...)))
  (subsigs
   (clause
    (pattern
     (test expression1 ...)
     (test => receiver)
     (else expression1 expression2 ...)))
   (receiver (value procedure?)))
  (desc . "A cond expression is evaluated by evaluating the 〈test〉 expressions of successive 〈clause〉s in order until one of them evaluates to a true value (see section 6.3.1). When a 〈test〉 evaluates to a true value, then the remaining 〈expression〉s in its 〈clause〉 are evaluated in order,and the result(s) of the last 〈expression〉 in the 〈clause〉 is(are) returned as the result(s) of the entire cond expression. If the selected 〈clause〉 contains only the 〈test〉 and no 〈expression〉s, then the value of the 〈test〉 is returned as the result. If the selected 〈clause〉 uses the => alternate form, then the 〈expression〉 is evaluated. Its value must be a procedure that accepts one argument; this procedure is then called on the value of the 〈test〉 and the value(s) returned by this procedure is(are) returned by the cond expression. If all 〈test〉s evaluate to false values, and there is no else clause, then the result of the conditional expression is unspecified; if there is an else clause, then its 〈expression〉s are evaluated, and the value(s) of the last one is(are) returned."))
 ((name . "cons")
  (signature lambda (obj1 obj2) pair?)
  (tags pure)
  (desc . "Returns a newly allocated pair whose car is obj1 and whose cdr is obj2. The pair is guaranteed to be different (in the sense of eqv?) from every existing object."))
 ((name . "current-input-port")
  (signature lambda () input-port?)
  (desc . "Returns the current default input port."))
 ((name . "current-output-port")
  (signature lambda () output-port?)
  (desc . "Returns the current default output port."))
 ((name . "define")
  (signature
   syntax-rules
   ()
   ((_ variable expression))
   ((_ (variable parameter1 ...) body))
   ((_ (variable parameter1 ... . parameter) body)))
  (desc . "At the top level of a program, a definition (define 〈variable〉 〈expression〉) has essentially the same effect as the assignment expression (set! 〈variable〉 〈expression〉) if 〈variable〉 is bound. If 〈variable〉 is not bound, however, then the definition will bind 〈variable〉 to a new location before performing the assignment, whereas it would be an error to perform a set! on an unbound variable.
Some implementations of Scheme use an initial environment in which all possible variables are bound to locations, most of which contain undefined values. Top level definitions in such an implementation are truly equivalent to assignments."))
 ((name . "define-syntax")
  (signature syntax-rules () ((_ keyword transformer-spec)))
  (subsigs (transformer-spec (value transformer-spec)))
  (desc . "Syntax definitions are valid only at the top level of a 〈program〉. 〈Keyword〉 is an identifier, and the 〈transformer spec〉 should be an instance of syntax-rules. The top-level syntactic environment is extended by binding the 〈keyword〉 to the specified transformer. There is no define-syntax analogue of internal definitions. Although macros may expand into definitions and syntax definitions in any context that permits them, it is an error for a definition or syntax definition to shadow a syntactic keyword whose meaning is needed to determine whether some form in the group of forms that contains the shadowing definition is in fact a definition, or, for internal definitions, is needed to determine the boundary between the group and the expressions that follow the group."))
 ((name . "denominator")
  (signature lambda ((rational? q)) integer?)
  (tags pure)
  (desc . "This procedure returns the denominator of its argument; the result is computed as if the argument was represented as a fraction in lowest terms. The denominator is always positive. The denominator of 0 is defined to be 1."))
 ((name . "do")
  (signature
   syntax-rules
   ()
   ((_ (variable-decl1 ...) (test expression ...) command ...)))
  (subsigs (variable-decl (pattern (variable init step) (variable init))))
  (desc . "Do is an iteration construct. It specifies a set of variables to be bound, how they are to be initialized at the start, and how they are to be updated on each iteration. When a termination condition is met, the loop exits after evaluating the 〈expression〉s.
Do expressions are evaluated as follows: The 〈init〉 expressions are evaluated (in some unspecified order), the 〈variable〉s are bound to fresh locations, the results of the 〈init〉 expressions are stored in the bindings of the 〈variable〉s, and then the iteration phase begins.
Each iteration begins by evaluating 〈test〉; if the result is false (see section 6.3.1), then the 〈command〉 expressions are evaluated in order for effect, the 〈step〉 expressions are evaluated in some unspecified order, the 〈variable〉s are bound to fresh locations, the results of the 〈step〉s are stored in the bindings of the 〈variable〉s, and the next iteration begins.
If 〈test〉 evaluates to a true value, then the 〈expression〉s are evaluated from left to right and the value(s) of the last 〈expression〉 is(are) returned. If no 〈expression〉s are present, then the value of the do expression is unspecified.
The region of the binding of a 〈variable〉 consists of the entire do expression except for the 〈init〉s. It is an error for a 〈variable〉 to appear more than once in the list of do variables.
A 〈step〉 may be omitted, in which case the effect is the same as if (〈variable〉 〈init〉 〈variable〉) had been written instead of (〈variable〉 〈init〉)."))
 ((name . "dynamic-wind") (signature
   lambda
   ((procedure? before) (procedure? thunk) (procedure? after))
   *)
  (subsigs
   (before (lambda () undefined))
   (thunk (lambda () *))
   (after (lambda () undefined)))
  (desc . "Calls thunk without arguments, returning the result(s) of this call. Before and after are called, also without arguments, as required by the following rules (note that in the absence of calls to continuations captured using call-with-current-continuation the three arguments are called once each, in order). Before is called whenever execution enters the dynamic extent of the call to thunk and after is called whenever it exits that dynamic extent. The dynamic extent of a procedure call is the period between when the call is initiated and when it returns. In Scheme, because of call-with-current-continuation,the dynamic extent of a call may not be a single, connected time period. It is defined as follows:
* The dynamic extent is entered when execution of the body of the called procedure begins.
* The dynamic extent is also entered when execution is not within the dynamic extent and a continuation is invoked that was captured (using call-with-current-continuation) during the dynamic extent.
* It is exited when the called procedure returns.
* It is also exited when execution is within the dynamic extent and a continuation is invoked that was captured while not within the dynamic extent.
If a second call to dynamic-wind occurs within the dynamic extent of the call to thunk and then a continuation is invoked in such a way that the after s from these two invocations of dynamic-wind are both to be called, then the after associated with the second (inner) call to dynamic-wind is called first.
If a second call to dynamic-wind occurs within the dynamic extent of the call to thunk and then a continuation is invoked in such a way that the befores from these two invocations of dynamic-wind are both to be called, then the before associated with the first (outer) call to dynamic-wind is called first.
If invoking a continuation requires calling the before from one call to dynamic-wind and the after from another, then the after is called first.
The effect of using a captured continuation to enter or exit the dynamic extent of a call to before or after is undefined."))
 ((name . "eof-object?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is an end of file object, otherwise returns #f. The precise set of end of file objects will vary among implementations, but in any case no end of file object will ever be an object that can be read in using read."))
 ((name . "eq?")
  (signature lambda (obj1 obj2) boolean?)
  (tags pure)
  (desc . "Eq? is similar to eqv? except that in some cases it is capable of discerning distinctions finer than those detectable by eqv?.
Eq? and eqv? are guaranteed to have the same behavior on symbols, booleans, the empty list, pairs, procedures, and non-empty strings and vectors. Eq?’s behavior on numbers and characters is implementation-dependent, but it will always return either true or false, and will return true only when eqv? would also return true. Eq? may also behave differently from eqv? on empty vectors and empty strings.
Rationale: It will usually be possible to implement eq? much more efficiently than eqv?, for example, as a simple pointer comparison instead of as some more complicated operation. One reason is that it may not be possible to compute eqv? of two numbers in constant time, whereas eq? implemented as pointer comparison will always finish in constant time. Eq? may be used like eqv? in applications using procedures to implement objects with state since it obeys the same constraints as eqv?."))
 ((name . "equal?")
  (signature lambda (obj1 obj2) boolean?)
  (tags pure)
  (desc . "Equal? recursively compares the contents of pairs, vectors, and strings, applying eqv? on other objects such as numbers and symbols. A rule of thumb is that objects are generally equal? if they print the same. Equal? may fail to terminate if its arguments are circular data structures."))
 ((name . "eqv?")
  (signature lambda (obj1 obj2) boolean?)
  (tags pure)
  (desc . "The eqv? procedure defines a useful equivalence relation on objects. Briefly, it returns #t if obj1 and obj2 should normally be regarded as the same object. This relation is left slightly open to interpretation, but the following partial specification of eqv? holds for all implementations of Scheme. The eqv? procedure returns #t if:
* obj1 and obj2 are both #t or both #f.
* obj1 and obj2 are both symbols and (string=? (symbol->string obj1) (symbol->string obj2)) => #t. Note: This assumes that neither obj1 nor obj2 is an \"uninterned symbol\" as alluded to in section 6.3.3. This report does not presume to specify the behavior of eqv? on implementation-dependent extensions.
* obj1 and obj2 are both numbers, are numerically equal (see =, section 6.2), and are either both exact or both inexact.
* obj1 and obj2 are both characters and are the same character according to the char=? procedure (section 6.3.4).
* both obj1 and obj2 are the empty list.
* obj1 and obj2 are pairs, vectors, or strings that denote the same locations in the store (section 3.4).
* obj1 and obj2 are procedures whose location tags are equal (section 4.1.4).

The eqv? procedure returns #f if:
* obj1 and obj2 are of different types (section 3.2).
* one of obj1 and obj2 is #t but the other is #f.
* obj1 and obj2 are symbols but (string=? (symbol->string obj1) (symbol->string obj2)) => #f
* one of obj1 and obj2 is an exact number but the other is an inexact number.
* obj1 and obj2 are numbers for which the = procedure returns #f.
* obj1 and obj2 are characters for which the char=? procedure returns #f.
* one of obj1 and obj2 is the empty list but the other is not.
* obj1 and obj2 are pairs, vectors, or strings that denote distinct locations.
* obj1 and obj2 are procedures that would behave differently (return different value(s) or have different side effects) for some arguments."))
 ((name . "even?")
  (signature lambda ((integer? n)) boolean?)
  (tags pure predicate))
 ((name . "inexact->exact")
  (signature lambda ((number? z)) exact?)
  (tags pure)
  (desc . "Inexact->exact returns an exact representation of z. The value returned is the exact number that is numerically closest to the argument. If an inexact argument has no reasonably close exact equivalent, then a violation of an implementation restriction may be reported.
The procedure implements the natural one-to-one correspondence between exact and inexact integers throughout an implementation-dependent range. See section 6.2.3."))
 ((name . "exact?")
  (signature lambda ((number? z)) boolean?)
  (tags pure)
  (supertypes number?)
  (desc . "This numerical predicate provides test for the exactness of a quantity. For any Scheme number, precisely one of exact?, inexact? predicates is true."))
 ((name . "expt")
  (signature lambda ((number? z1) (number? z2)) number?)
  (tags pure)
  (desc . "Returns z1 raised to the power z2. 0^z is 1 if z = 0 and 0 otherwise."))
 ((name . "floor")
  (signature lambda ((real? x)) integer?)
  (tags pure)
  (desc . "Floor returns the largest integer not larger than x. Note: If the argument to the procedure is inexact, then the result will also be inexact. If an exact value is needed, the result should be passed to the inexact->exact procedure."))
 ((name . "for-each")
  (signature
   lambda
   ((procedure? proc) (list? list1) (list? list2) ...)
   undefined)
  (subsigs (proc (lambda (obj1 obj2 ...) undefined)))
  (desc . "The arguments to for-each are like the arguments to map, but for-each calls proc for its side effects rather than for its values. Unlike map, for-each is guaranteed to call proc on the elements of the lists in order from the first element(s) to the last, and the value returned by for-each is unspecified."))
 ((name . "gcd")
  (signature lambda ((integer? n) ...) integer?)
  (tags pure)
  (desc . "The procedure returns the greatest common divisor of its arguments. The result is always non-negative."))
 ((name . "if")
  (signature
   syntax-rules
   ()
   ((_ test consequent))
   ((_ test consequent alternate)))
  (desc . "An if expression is evaluated as follows: first,〈test〉 is evaluated. If it yields a true value (see section 6.3.1), then 〈consequent〉 is evaluated and its value(s) is(are) returned. Otherwise 〈alternate〉 is evaluated and its value(s) is(are) returned. If 〈test〉 yields a false value and no 〈alternate〉 is specified, then the result of the expression is unspecified."))
 ((name . "exact->inexact")
  (signature lambda ((number? z)) inexact?)
  (tags pure))
 ((name . "inexact?")
  (signature lambda ((number? z)) boolean?)
  (tags pure)
  (supertypes number?)
  (desc . "This numerical predicate provides test for the exactness of a quantity. For any Scheme number, precisely one of exact?, inexact? predicates is true."))
 ((name . "input-port?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes port?)
  (desc . "Returns #t if obj is an input port, otherwise returns #f"))
 ((name . "integer->char")
  (signature lambda ((integer? n)) char?)
  (tags pure)
  (desc . "Given an exact integer that is the image of a character under char->integer,integer->char returns that character. The procedure implements order-preserving isomorphisms between the set of characters under the char<=? ordering and some subset of the integers under the <= ordering"))
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
  (desc . "A lambda expression evaluates to a procedure. The environment in effect when the lambda expression was evaluated is remembered as part of the procedure. When the procedure is later called with some actual arguments, the environment in which the lambda expression was evaluated will be extended by binding the variables in the formal argument list to fresh locations, the corresponding actual argument values will be stored in those locations, and the expressions in the body of the lambda expression will be evaluated sequentially in the extended environment. The result(s) of the last expression in the body will be returned as the result(s) of the procedure call.
〈Formals〉 should have one of the following forms:
* (〈variable1〉 . . . ): The procedure takes a fixed number of arguments; when the procedure is called, the arguments will be stored in the bindings of the corresponding variables.
* 〈variable〉: The procedure takes any number of arguments; when the procedure is called, the sequence of actual arguments is converted into a newly allocated list, and the list is stored in the binding of the 〈variable〉.
* (〈variable1〉 . . . 〈variablen〉 . 〈variablen+1〉): If a space-delimited period precedes the last variable, then the procedure takes n or more arguments, where n is the number of formal arguments before the period (there must be at least one). The value stored in the binding of the last variable will be a newly allocated list of the actual arguments left over after all the other actual arguments have been matched up against the other formal arguments.

It is an error for a 〈variable〉 to appear more than once in 〈formals〉. Each procedure created as the result of evaluating a lambda expression is (conceptually) tagged with a storage location, in order to make eqv? and eq? work on procedures (see section 6.1)."))
 ((name . "lcm")
  (signature lambda ((integer? n) ...) integer?)
  (tags pure)
  (desc . "This procedure returns the least common multiple of its arguments. The result is always non-negative."))
 ((name . "length")
  (signature lambda ((list? list)) integer?)
  (tags pure)
  (desc . "Returns the length of list."))
 ((name . "let")
  (signature syntax-rules ()
             ((_ bindings body))
             ((_ variable bindings body)))
  (subsigs (bindings (pattern ((variable1 init1) ...))))
  (desc . "The 〈init〉s are evaluated in the current environment (in some unspecified order), the 〈variable〉s are bound to fresh locations holding the results, the 〈body〉 is evaluated in the extended environment, and the value(s) of the last expression of 〈body〉 is(are) returned. Each bind ing of a 〈variable〉 has 〈body〉 as its region.
\"Named let\" is a variant on the syntax of let which provides a more general looping construct than do and may also be used to express recursions. It has the same syntax and semantics as ordinary let except that 〈variable〉 is bound within 〈body〉 to a procedure whose formal arguments are the bound variables and whose body is 〈body〉. Thus the execution of 〈body〉 may be repeated by invoking the procedure named by 〈variable〉."))
 ((name . "let*")
  (signature syntax-rules () ((_ bindings body)))
  (subsigs (bindings (pattern ((variable1 init1) ...))))
  (desc . "Let* is similar to let, but the bindings are performed sequentially from left to right, and the region of a binding indicated by (〈variable〉 〈init〉) is that part of the let* expression to the right of the binding. Thus the second binding is done in an environment in which the first binding is visible, and so on."))
 ((name . "let-syntax")
  (signature syntax-rules () ((_ bindings body)))
  (subsigs
   (bindings (pattern ((keyword transformer-spec) ...)))
   (transformer-spec (value transformer-spec)))
  (desc . "Each 〈keyword〉 is an identifier, each 〈transformer spec〉 is an instance of syntax-rules, and 〈body〉 should be a sequence of one or more expressions. It is an error for a 〈keyword〉 to appear more than once in the list of keywords being bound.
Semantics: The 〈body〉 is expanded in the syntactic environment obtained by extending the syntactic environment of the let-syntax expression with macros whose keywords are the 〈keyword〉s, bound to the specified transformers. Each binding of a 〈keyword〉 has 〈body〉 as its region."))
 ((name . "letrec")
  (signature syntax-rules () ((_ bindings body)))
  (subsigs (bindings (pattern ((variable1 init1) ...))))
  (desc . "The 〈variable〉s are bound to fresh locations holding undefined values, the 〈init〉s are evaluated in the resulting environment (in some unspecified order), each 〈variable〉 is assigned to the result of the corresponding 〈init〉, the 〈body〉 is evaluated in the resulting environment, and the value(s) of the last expression in 〈body〉 is(are) returned. Each binding of a 〈variable〉 has the entire letrec expression as its region, making it possible to define mutually recursive procedures.
One restriction on letrec is very important: it must be possible to evaluate each 〈init〉 without assigning or referring to the value of any 〈variable〉. If this restriction is violated, then it is an error. The restriction is necessary because Scheme passes arguments by value rather than by name. In the most common uses of letrec, all the 〈init〉s are lambda expressions and the restriction is satisfied automatically."))
 ((name . "letrec-syntax")
  (signature syntax-rules () ((_ bindings body)))
  (subsigs (bindings (pattern ((keyword transformer-spec) ...))))
  (desc . "The 〈body〉 is expanded in the syntactic environment obtained by extending the syntactic environment of the letrec-syntax expression with macros whose key- words are the 〈keyword〉s, bound to the specified transformers. Each binding of a 〈keyword〉 has the 〈bindings〉 as well as the 〈body〉 within its region, so the transformers can transcribe expressions into uses of the macros introduced by the letrec-syntax expression."))
 ((name . "list")
  (signature lambda (obj ...) list?)
  (tags pure)
  (desc . "Returns a newly allocated list of its arguments."))
 ((name . "list->string")
  (signature lambda ((list? list)) string?)
  (tags pure)
  (desc . "List->string returns a newly allocated string formed from the characters in the list list, which must be a list of characters. String->list and list->string are inverses so far as equal? is concerned."))
 ((name . "list->vector")
  (signature lambda ((list? list)) vector?)
  (tags pure)
  (desc . "List->vector returns a newly created vector initialized to the elements of the list list."))
 ((name . "list-ref")
  (signature lambda ((list? list) (integer? k)) *)
  (tags pure)
  (desc . "Returns the kth element of list. (This is the same as the car of (list-tail list k).) It is an error if list has fewer than k elements."))
 ((name . "list-tail")
  (signature lambda ((list? list) (integer? k)) list?)
  (tags pure)
  (desc . "Returns the sublist of list obtained by omitting the first k elements. It is an error if list has fewer than k elements."))
 ((name . "list?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a list, otherwise returns #f. By definition, all lists have finite length and are terminated by the empty list."))
 ((name . "make-string")
  (signature
   case-lambda
   (((integer? k)) string?)
   (((integer? k) (char? char)) string?))
  (tags pure)
  (desc . "Make-string returns a newly allocated string of length k. If char is given, then all elements of the string are initialized to char, otherwise the contents of the string are unspecified"))
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
  (desc . "The lists must be lists, and proc must be a procedure taking as many arguments as there are lists and returning a single value. If more than one list is given, then they must all be the same length. Map applies proc element-wise to the elements of the lists and returns a list of the results, in order. The dynamic order in which proc is applied to the elements of the lists is unspecified."))
 ((name . "max")
  (signature lambda ((real? x1) (real? x2) ...) real?)
  (tags pure)
  (desc . "This procedure returns the maximum of its arguments. Note: If any argument is inexact, then the result will also be inexact (unless the procedure can prove that the inaccuracy is not large enough to affect the result, which is possible only in unusual implementations). If min or max is used to compare numbers of mixed exactness, and the numerical value of the result cannot be represented as an inexact number without loss of accuracy, then the procedure may report a violation of an implementation restriction."))
 ((name . "member")
  (signature lambda (obj (list? list)) (or #f list?))
  (tags pure)
  (desc . "This procedure returns the first sublist of list whose car is obj , where the sublists of list are the non-empty lists returned by (list-tail list k ) for k less than the length of list. If obj does not occur in list, then #f (not the empty list) is returned. Member uses equal? to compare obj with the elements of list."))
 ((name . "memq")
  (signature lambda (obj (list? list)) (or #f list?))
  (tags pure)
  (desc . "This procedure returns the first sublist of list whose car is obj , where the sublists of list are the non-empty lists returned by (list-tail list k ) for k less than the length of list. If obj does not occur in list, then #f (not the empty list) is returned. Memq uses eq? to compare obj with the elements of list."))
 ((name . "memv")
  (signature lambda (obj (list? list)) (or #f list?))
  (tags pure)
  (desc . "This procedure returns the first sublist of list whose car is obj , where the sublists of list are the non-empty lists returned by (list-tail list k ) for k less than the length of list. If obj does not occur in list, then #f (not the empty list) is returned. Memv uses eqv? to compare obj with the elements of list."))
 ((name . "min")
  (signature lambda ((real? x1) (real? x2) ...) real?)
  (tags pure)
  (desc . "This procedure returns the minimum of its arguments. Note: If any argument is inexact, then the result will also be inexact (unless the procedure can prove that the inaccuracy is not large enough to affect the result, which is possible only in unusual implementations). If min or max is used to compare numbers of mixed exactness, and the numerical value of the result cannot be represented as an inexact number without loss of accuracy, then the procedure may report a violation of an implementation restriction."))
 ((name . "modulo")
  (signature lambda ((integer? n1) (integer? n2)) integer?)
  (tags pure)
  (desc . "Number-theoretic (integer) division. n2 should be non-zero."))
 ((name . "negative?") (signature lambda ((real? x)) boolean?) (tags pure))
 ((name . "newline")
  (signature case-lambda (() undefined) (((output-port? port)) undefined))
  (desc . "Writes an end of line to port. Exactly how this is done differs from one operating system to another. Returns an unspecified value. The port argument may be omitted, in which case it defaults to the value returned by current-output-port."))
 ((name . "not")
  (signature lambda (obj) boolean?)
  (tags pure)
  (desc . "Not returns #t if obj is false, and returns #f otherwise."))
 ((name . "null?")
  (signature lambda (obj) boolean?)
  (supertypes list?)
  (tags pure predicate)
  (desc . "Returns #t if obj is the empty list, otherwise returns #f"))
 ((name . "number->string")
  (signature
   case-lambda
   (((number? z)) string?)
   (((number? z) (integer? radix)) string?))
  (tags pure)
  (desc . "Radix must be an exact integer, either 2, 8, 10, or 16. If omitted, radix defaults to 10. The procedure number-> string takes a number and a radix and returns as a string an external representation of the given number in the given radix such that
(let ((number number) (radix radix)) (eqv? number (string->number (number->string number radix) radix)))
is true. It is an error if no possible result makes this expression true.
If z is inexact, the radix is 10, and the above expression can be satisfied by a result that contains a decimal point, then the result contains a decimal point and is expressed using the minimum number of digits (exclusive of exponent and trailing zeroes) needed to make the above expression true [3, 5]; otherwise the format of the result is unspecified. The result returned by number->string never contains an explicit radix prefix.
Note: The error case can occur only when z is not a complex number or is a complex number with a non-rational real or imaginary part.
Rationale: If z is an inexact number represented using flonums,and the radix is 10, then the above expression is normally satisfied by a result containing a decimal point. The unspecified case allows for infinities, NaNs, and non-flonum representations."))
 ((name . "number?") (signature lambda (obj) boolean?) (tags pure predicate))
 ((name . "numerator")
  (signature lambda ((rational? q)) integer?)
  (tags pure)
  (desc . "This procedure returns the numerator of its argument; the result is computed as if the argument was represented as a fraction in lowest terms."))
 ((name . "odd?")
  (signature lambda ((integer? n)) boolean?)
  (tags pure predicate))
 ((name . "or")
  (signature syntax-rules () ((_ test1 ...)))
  (desc . "The 〈test〉 expressions are evaluated from left to right, and the value of the first expression that evaluates to a true value (see section 6.3.1) is returned. Any remaining expressions are not evaluated. If all expressions evaluate to false values, the value of the last expression is returned. If there are no expressions then #f is returned."))
 ((name . "output-port?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes port?)
  (desc . "Returns #t if obj is an output port, otherwise returns #f."))
 ((name . "pair?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Pair? returns #t if obj is a pair, and otherwise returns #f."))
 ((name . "peek-char")
  (signature
   case-lambda
   (() (or eof-object? char?))
   (((input-port? port)) (or eof-object? char?)))
  (desc . "Returns the next character available from the input port, without updating the port to point to the following character. If no more characters are available, an end of file object is returned. Port may be omitted, in which case it defaults to the value returned by current-input-port.
Note: The value returned by a call to peek-char is the same as the value that would have been returned by a call to read-char with the same port. The only difference is that the very next call to read-char or peek-char on that port will return the value returned by the preceding call to peek-char. In particular, a call to peek-char on an interactive port will hang waiting for input whenever a call to read-char would have hung."))
 ((name . "positive?") (signature lambda ((real? x)) boolean?) (tags pure))
 ((name . "procedure?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a procedure, otherwise returns #f."))
 ((name . "quasiquote")
  (signature syntax-rules () ((_ qq-template)))
  (desc . "\"Backquote\" or \"quasiquote\" expressions are useful for constructing a list or vector structure when most but not all of the desired structure is known in advance. If no commas appear within the 〈qq template〉, the result of evaluating `〈qq template〉 is equivalent to the result of evaluating ’〈qq template〉. If a comma appears within the 〈qq template〉, however, the expression following the comma is evaluated (\"unquoted\") and its result is inserted into the structure instead of the comma and the expression. If a comma appears followed immediately by an at-sign (@), then the following expression must evaluate to a list; the opening and closing parentheses of the list are then “stripped away” and the elements of the list are inserted in place of the comma at-sign expression sequence. A comma at-sign should only appear within a list or vector 〈qq template〉.
Quasiquote forms may be nested. Substitutions are made only for unquoted components appearing at the same nesting level as the outermost backquote. The nesting level increases by one inside each successive quasiquotation, and decreases by one inside each unquotation.
The two notations `〈qq template〉 and (quasiquote 〈qq template〉) are identical in all respects. ,〈expression〉 is identical to (unquote 〈expression〉), and ,@〈expression〉 is identical to (unquote-splicing 〈expression〉). The external syntax generated by write for two-element lists whose car is one of these symbols may vary between implementations.
Unpredictable behavior can result if any of the symbols quasiquote, unquote, or unquote-splicing appear in positions within a 〈qq template〉 otherwise than as described above."))
 ((name . "quote")
  (signature syntax-rules () ((_ datum)))
  (desc . "(quote 〈datum〉) evaluates to 〈datum〉. 〈Datum〉 may be any external representation of a Scheme object (see section 3.3). This notation is used to include literal constants in Scheme code.
(quote 〈datum〉) may be abbreviated as ’〈datum〉. The two notations are equivalent in all respects.
Numerical constants, string constants, character constants,and boolean constants evaluate \"to themselves\"; they need not be quoted.
As noted in section 3.4, it is an error to alter a constant (i.e. the value of a literal expression) using a mutation procedure like set-car! or string-set!."))
 ((name . "quotient")
  (signature lambda ((integer? n1) (integer? n2)) integer?)
  (tags pure)
  (desc . "Implements number-theoretic (integer) division. n2 should be non-zero"))
 ((name . "rational?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes real?))
 ((name . "rationalize")
  (signature lambda ((real? x) (real? y)) rational?)
  (tags pure)
  (desc . "Rationalize returns the simplest rational number differing from x by no more than y."))
 ((name . "read-char")
  (signature
   case-lambda
   (() (or eof-object? char?))
   (((input-port? port)) (or eof-object? char?)))
  (desc . "Returns the next character available from the input port, updating the port to point to the following character. If no more characters are available, an end of file object is returned. Port may be omitted, in which case it defaults to the value returned by current-input-port."))
 ((name . "real?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes complex?))
 ((name . "remainder")
  (signature lambda ((integer? n1) (integer? n2)) integer?)
  (tags pure)
  (desc . "Implements number-theoretic (integer) division. n2 should be non-zero"))
 ((name . "reverse")
  (signature lambda ((list? list)) list?)
  (tags pure)
  (desc . "Returns a newly allocated list consisting of the elements of list in reverse order."))
 ((name . "round")
  (signature lambda ((real? x)) integer?)
  (tags pure)
  (desc . "Round returns the closest integer to x, rounding to even when x is halfway between two integers.
Rationale: Round rounds to even for consistency with the default rounding mode specified by the IEEE floating point standard.
Note: If the argument to the procedure is inexact, then the result will also be inexact. If an exact value is needed, the result should be passed to the inexact->exact procedure."))
 ((name . "set!")
  (signature syntax-rules () ((_ variable expression)))
  (desc . "〈Expression〉 is evaluated, and the resulting value is stored in the location to which 〈variable〉 is bound. 〈Variable〉 must be bound either in some region enclosing the set! expression or at top level. The result of the set! expression is unspecified."))
 ((name . "set-car!")
  (signature lambda ((pair? pair) obj) undefined)
  (desc . "Stores obj in the car field of pair. "))
 ((name . "set-cdr!")
  (signature lambda ((pair? pair) obj) undefined)
  (desc . "Stores obj in the cdr field of pair. "))
 ((name . "string")
  (signature lambda ((char? char) ...) string?)
  (tags pure)
  (desc . "Returns a newly allocated string composed of the arguments."))
 ((name . "string->list")
  (signature lambda ((string? string)) list?)
  (tags pure)
  (desc . "String->list returns a newly allocated list of the characters that make up the given string. String->list and list->string are inverses so far as equal? is concerned."))
 ((name . "string->number")
  (signature
   case-lambda
   (((string? string)) (or number? #f))
   (((string? string) (integer? radix)) (or number? #f)))
  (tags pure)
  (desc . "Returns a number of the maximally precise representation expressed by the given string. Radix must be an exact integer, either 2, 8, 10, or 16. If supplied, radix is a default radix that may be overridden by an explicit radix prefix in string (e.g. \"#o177\"). If radix is not supplied, then the default radix is 10. If string is not a syntactically valid notation for a number, then string->number returns #f.
Note: The domain of string->number may be restricted by implementations in the following ways. String->number is permitted to return #f whenever string contains an explicit radix prefix. If all numbers supported by an implementation are real, then string->number is permitted to return #f whenever string uses the polar or rectangular notations for complex numbers. If all numbers are integers, then string->number may return #f whenever the fractional notation is used. If all numbers are exact, then string->number may return #f whenever an exponent marker or explicit exactness prefix is used, or if a # appears in place of a digit. If all inexact numbers are integers, then string->number may return #f whenever a decimal point is used."))
 ((name . "string->symbol")
  (signature lambda ((string? string)) symbol?)
  (tags pure)
  (desc . "Returns the symbol whose name is string. This procedure can create symbols with names containing special characters or letters in the non-standard case, but it is usually a bad idea to create such symbols because in some implementations of Scheme they cannot be read as themselves. See symbol->string."))
 ((name . "string-append")
  (signature lambda ((string? string) ...) string?)
  (tags pure)
  (desc . "Returns a newly allocated string whose characters form the concatenation of the given strings."))
 ((name . "string-copy")
  (signature lambda ((string? string)) string?)
  (tags pure)
  (desc . "Returns a newly allocated copy of the given string."))
 ((name . "string-fill!")
  (signature lambda ((string? string) (char? fill)) undefined)
  (desc . "Stores char in every element of the given string and returns an unspecified value."))
 ((name . "string-length")
  (signature lambda ((string? string)) integer?)
  (tags pure)
  (desc . "Returns the number of characters in the given string."))
 ((name . "string-ref")
  (signature lambda ((string? string) (integer? k)) char?)
  (tags pure)
  (desc . "k must be a valid index of string. String-ref returns character k of string using zero-origin indexing."))
 ((name . "string-set!")
  (signature lambda ((string? string) (integer? k) (char? char)) undefined)
  (desc . "k must be a valid index of string. String-set! stores char in element k of string and returns an unspecified value."))
 ((name . "string<=?")
  (signature
   lambda
   ((string? string1) (string? string2))
   boolean?)
  (tags pure)
  (desc . "string<=? is the lexicographic ordering on strings induced by the ordering char<=? on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string is considered to be lexicographically less than the longer string. Implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicates."))
 ((name . "string<?")
  (signature
   lambda
   ((string? string1) (string? string2))
   boolean?)
  (tags pure)
  (desc . "string<? is the lexicographic ordering on strings induced by the ordering char<? on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string is considered to be lexicographically less than the longer string. Implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicates."))
 ((name . "string=?")
  (signature
   lambda
   ((string? string1) (string? string2))
   boolean?)
  (tags pure)
  (desc . "Returns #t if the two strings are the same length and contain the same characters in the same positions, otherwise returns #f."))
 ((name . "string>=?")
  (signature
   lambda
   ((string? string1) (string? string2))
   boolean?)
  (tags pure)
  (desc . "string>=? is the lexicographic ordering on strings induced by the ordering char>=? on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string is considered to be lexicographically less than the longer string. Implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicates."))
 ((name . "string>?")
  (signature
   lambda
   ((string? string1) (string? string2))
   boolean?)
  (tags pure)
  (desc . "string>? is the lexicographic ordering on strings induced by the ordering char>? on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string is considered to be lexicographically less than the longer string. Implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicates."))
 ((name . "string?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a string, otherwise returns #f"))
 ((name . "substring")
  (signature lambda ((string? string) (integer? start) (integer? end)) string?)
  (tags pure)
  (desc . "Substring returns a newly allocated string formed from the characters of string beginning with index start (inclusive) and ending with index end (exclusive)."))
 ((name . "symbol->string")
  (signature lambda ((symbol? symbol)) string?)
  (tags pure)
  (desc . "Returns the name of symbol as a string. If the symbol was part of an object returned as the value of a literal expression (section 4.1.2) or by a call to the read procedure, and its name contains alphabetic characters, then the string returned will contain characters in the implementation’s preferred standard case—some implementations will prefer upper case, others lower case. If the symbol was returned by string->symbol, the case of characters in the string returned will be the same as the case in the string that was passed to string->symbol. It is an error to apply mutation procedures like string-set! to strings returned by this procedure."))
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
  (desc . "An instance of syntax-rules produces a new macro transformer by specifying a sequence of hygienic rewrite rules. A use of a macro whose keyword is associated with a transformer specified by syntax-rules is matched against the patterns contained in the 〈syntax rule〉s, beginning with the leftmost 〈syntax rule〉. When a match is found, the macro use is transcribed hygienically according to the template.
An identifier that appears in the pattern of a 〈syntax rule〉 is a pattern variable, unless it is the keyword that begins the pattern, is listed in 〈literals〉, or is the identifier “...”. Pattern variables match arbitrary input elements and are used to refer to elements of the input in the template. It is an error for the same pattern variable to appear more than once in a 〈pattern〉.
The keyword at the beginning of the pattern in a 〈syntax rule〉 is not involved in the matching and is not considered a pattern variable or literal identifier.
Rationale: The scope of the keyword is determined by the expression or syntax definition that binds it to the associated macro transformer. If the keyword were a pattern variable or literal identifier, then the template that follows the pattern would be within its scope regardless of whether the keyword were bound by let-syntax or by letrec-syntax.
Identifiers that appear in 〈literals〉 are interpreted as literal identifiers to be matched against corresponding subforms of the input. A subform in the input matches a literal identifier if and only if it is an identifier and either both its occurrence in the macro expression and its occurrence in the macro definition have the same lexical binding, or the two identifiers are equal and both have no lexical binding.
A subpattern followed by ... can match zero or more elements of the input. It is an error for ... to appear in 〈literals〉. Within a pattern the identifier ... must follow the last element of a nonempty sequence of subpatterns.
More formally, an input form F matches a pattern P if and only if:
* P is a non-literal identifier; or
* P is a literal identifier and F is an identifier with the same binding; or
* P is a list (P1 . . . Pn) and F is a list of n forms that match P1 through Pn, respectively; or
* P is an improper list (P1 P2 . . . Pn . Pn+1) and F is a list or improper list of n or more forms that match P1 through Pn, respectively, and whose nth \"cdr\" matches Pn+1; or
* P is of the form (P1 . . . Pn Pn+1 〈ellipsis〉) where 〈ellipsis〉 is the identifier ... and F is a proper list of at least n forms, the first n of which match P1 through Pn, respectively, and each remaining element of F matches Pn+1; or
* P is a vector of the form #(P1 . . . Pn) and F is a vector of n forms that match P1 through Pn; or
* P is of the form #(P1 . . . Pn Pn+1 〈ellipsis〉) where 〈ellipsis〉 is the identifier ... and F is a vector of n or more forms the first n of which match P1 through Pn, respectively, and each remaining element of F matches Pn+1; or
* P is a datum and F is equal to P in the sense of the equal? procedure.

It is an error to use a macro keyword, within the scope of its binding, in an expression that does not match any of the patterns.
When a macro use is transcribed according to the template of the matching 〈syntax rule〉, pattern variables that occur in the template are replaced by the subforms they match in the input. Pattern variables that occur in subpatterns followed by one or more instances of the identifier ... are allowed only in subtemplates that are followed by as many instances of .... They are replaced in the output by all of the subforms they match in the input, distributed as indicated. It is an error if the output cannot be built up as specified.
Identifiers that appear in the template but are not pattern variables or the identifier ... are inserted into the output as literal identifiers. If a literal identifier is inserted as a free identifier then it refers to the binding of that identifier within whose scope the instance of syntax-rules appears. If a literal identifier is inserted as a bound identifier then it is in effect renamed to prevent inadvertent captures of free identifiers."))
 ((name . "truncate")
  (signature lambda ((real? x)) integer?)
  (tags pure)
  (desc . "Truncate returns the integer closest to x whose absolute value is not larger than the absolute value of x. Note: If the argument to the procedure is inexact, then the result will also be inexact. If an exact value is needed, the result should be passed to the inexact->exact procedure."))
 ((name . "unquote")
  (signature syntax-rules () ((_ expression)))
  (desc . "\"Backquote\" or \"quasiquote\" expressions are useful for constructing a list or vector structure when most but not all of the desired structure is known in advance. If no commas appear within the 〈qq template〉, the result of evaluating `〈qq template〉 is equivalent to the result of evaluating ’〈qq template〉. If a comma appears within the 〈qq template〉, however, the expression following the comma is evaluated (\"unquoted\") and its result is inserted into the structure instead of the comma and the expression. If a comma appears followed immediately by an at-sign (@), then the following expression must evaluate to a list; the opening and closing parentheses of the list are then “stripped away” and the elements of the list are inserted in place of the comma at-sign expression sequence. A comma at-sign should only appear within a list or vector 〈qq template〉.
Quasiquote forms may be nested. Substitutions are made only for unquoted components appearing at the same nesting level as the outermost backquote. The nesting level increases by one inside each successive quasiquotation, and decreases by one inside each unquotation.
The two notations `〈qq template〉 and (quasiquote 〈qq template〉) are identical in all respects. ,〈expression〉 is identical to (unquote 〈expression〉), and ,@〈expression〉 is identical to (unquote-splicing 〈expression〉). The external syntax generated by write for two-element lists whose car is one of these symbols may vary between implementations.
Unpredictable behavior can result if any of the symbols quasiquote, unquote, or unquote-splicing appear in positions within a 〈qq template〉 otherwise than as described above."))
 ((name . "unquote-splicing")
  (signature syntax-rules () ((_ expression)))
  (desc . "\"Backquote\" or \"quasiquote\" expressions are useful for constructing a list or vector structure when most but not all of the desired structure is known in advance. If no commas appear within the 〈qq template〉, the result of evaluating `〈qq template〉 is equivalent to the result of evaluating ’〈qq template〉. If a comma appears within the 〈qq template〉, however, the expression following the comma is evaluated (\"unquoted\") and its result is inserted into the structure instead of the comma and the expression. If a comma appears followed immediately by an at-sign (@), then the following expression must evaluate to a list; the opening and closing parentheses of the list are then “stripped away” and the elements of the list are inserted in place of the comma at-sign expression sequence. A comma at-sign should only appear within a list or vector 〈qq template〉.
Quasiquote forms may be nested. Substitutions are made only for unquoted components appearing at the same nesting level as the outermost backquote. The nesting level increases by one inside each successive quasiquotation, and decreases by one inside each unquotation.
The two notations `〈qq template〉 and (quasiquote 〈qq template〉) are identical in all respects. ,〈expression〉 is identical to (unquote 〈expression〉), and ,@〈expression〉 is identical to (unquote-splicing 〈expression〉). The external syntax generated by write for two-element lists whose car is one of these symbols may vary between implementations.
Unpredictable behavior can result if any of the symbols quasiquote, unquote, or unquote-splicing appear in positions within a 〈qq template〉 otherwise than as described above."))
 ((name . "values")
  (signature lambda (obj ...) (values * ...))
  (tags pure)
  (desc . "Delivers all of its arguments to its continuation. Except for continuations created by the call-with-values procedure, all continuations take exactly one value."))
 ((name . "vector")
  (signature lambda (obj ...) vector?)
  (tags pure)
  (desc . "Returns a newly allocated vector whose elements contain the given arguments. Analogous to list."))
 ((name . "vector->list")
  (signature lambda ((vector? vector)) list?)
  (tags pure)
  (desc . "Vector->list returns a newly allocated list of the objects contained in the elements of vector."))
 ((name . "vector-fill!")
  (signature lambda ((vector? vector) fill) undefined)
  (desc . "Stores fill in every element of vector . The value returned by vector-fill! is unspecified."))
 ((name . "vector-length")
  (signature lambda ((vector? vector)) integer?)
  (tags pure)
  (desc . "Returns the number of elements in vector as an exact integer."))
 ((name . "vector-ref")
  (signature lambda ((vector? vector) (integer? k)) *)
  (tags pure)
  (desc . "k must be a valid index of vector . Vector-ref returns the contents of element k of vector ."))
 ((name . "vector-set!")
  (signature lambda ((vector? vector) (integer? k) obj) undefined)
  (desc . "k must be a valid index of vector . Vector-set! stores obj in element k of vector . The value returned by vector-set! is unspecified."))
 ((name . "vector?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a vector, otherwise returns #f."))
 ((name . "write-char")
  (signature
   case-lambda
   (((char? char)) undefined)
   (((char? char) (output-port? port)) undefined))
  (desc . "Writes the character char (not an external representation of the character) to the given port and returns an unspecified value. The port argument may be omitted, in which case it defaults to the value returned by current-output-port."))
 ((name . "zero?")
  (signature lambda ((number? z)) boolean?)
  (tags pure predicate))
 ((name . "angle") (signature lambda ((complex? z)) real?) (tags pure))
 ((name . "imag-part") (signature lambda ((complex? z)) real?) (tags pure))
 ((name . "magnitude") (signature lambda ((complex? z)) real?) (tags pure))
 ((name . "make-polar")
  (signature lambda ((real? x3) (real? x4)) complex?)
  (tags pure))
 ((name . "make-rectangular")
  (signature lambda ((real? x1) (real? x2)) complex?)
  (tags pure))
 ((name . "real-part") (signature lambda ((complex? z)) real?) (tags pure))
 ((name . "char-alphabetic?")
  (signature lambda ((char? char)) boolean?)
  (tags pure))
 ((name . "char-ci<=?")
  (signature lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
  (tags pure)
  (desc . "The procedure is similar to char<=?, but treats upper case and lower case letters as the same."))
 ((name . "char-ci<?")
  (signature lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
  (tags pure)
  (desc . "The procedure is similar to char<?, but treats upper case and lower case letters as the same."))
 ((name . "char-ci=?")
  (signature lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
  (tags pure)
  (desc . "The procedure is similar to char=?, but treats upper case and lower case letters as the same."))
 ((name . "char-ci>=?")
  (signature lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
  (tags pure)
  (desc . "The procedure is similar to char>=?, but treats upper case and lower case letters as the same."))
 ((name . "char-ci>?")
  (signature lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
  (tags pure)
  (desc . "The procedure is similar to char>?, but treats upper case and lower case letters as the same."))
 ((name . "char-downcase")
  (signature lambda ((char? char)) char?)
  (tags pure)
  (desc . "The procedure returns a character char2 such that (char-ci=? char char2). In addition, if char is alphabetic, then the result of char-downcase is lower case."))
 ((name . "char-lower-case?")
  (signature lambda ((char? char)) boolean?)
  (tags pure))
 ((name . "char-numeric?")
  (signature lambda ((char? char)) boolean?)
  (tags pure))
 ((name . "char-upcase")
  (signature lambda ((char? char)) char?)
  (tags pure)
  (desc . "The procedure returns a character char2 such that (char-ci=? char char2). In addition, if char is alphabetic, then the result of char-upcase is upper case."))
 ((name . "char-upper-case?")
  (signature lambda ((char? char)) boolean?)
  (tags pure))
 ((name . "char-whitespace?")
  (signature lambda ((char? char)) boolean?)
  (tags pure))
 ((name . "string-ci<=?")
  (signature
   lambda
   ((string? string1) (string? string2))
   boolean?)
  (tags pure)
  (desc . "string-ci<=? is the lexicographic ordering on strings induced by the ordering char-ci<=? on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string is considered to be lexicographically less than the longer string. Implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicates."))
 ((name . "string-ci<?")
  (signature
   lambda
   ((string? string1) (string? string2))
   boolean?)
  (tags pure)
  (desc . "string-ci<? is the lexicographic ordering on strings induced by the ordering char-ci<? on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string is considered to be lexicographically less than the longer string. Implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicates."))
 ((name . "string-ci=?")
  (signature
   lambda
   ((string? string1) (string? string2))
   boolean?)
  (tags pure)
  (desc . "Returns #t if the two strings are the same length and contain the same characters in the same positions, otherwise returns #f. String-ci=? treats upper and lower case letters as though they were the same character."))
 ((name . "string-ci>=?")
  (signature
   lambda
   ((string? string1) (string? string2))
   boolean?)
  (tags pure)
  (desc . "string-ci>=? is the lexicographic ordering on strings induced by the ordering char-ci>=? on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string is considered to be lexicographically less than the longer string. Implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicates."))
 ((name . "string-ci>?")
  (signature
   lambda
   ((string? string1) (string? string2))
   boolean?)
  (tags pure)
  (desc . "string-ci>? is the lexicographic ordering on strings induced by the ordering char-ci>? on characters. If two strings differ in length but are the same up to the length of the shorter string, the shorter string is considered to be lexicographically less than the longer string. Implementations may generalize the procedure to take more than two arguments, as with the corresponding numerical predicates."))
 ((name . "caaaar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car, car, car, car."))
 ((name . "caaadr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car, car, car, cdr."))
 ((name . "caaar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car, car, car."))
 ((name . "caadar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car, car, cdr, car."))
 ((name . "caaddr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car, car, cdr, cdr."))
 ((name . "caadr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car, car, cdr."))
 ((name . "cadaar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car, cdr, car, car."))
 ((name . "cadadr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car, cdr, car, cdr."))
 ((name . "cadar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car, cdr, car."))
 ((name . "caddar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car, cdr, cdr, car."))
 ((name . "cadddr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car, cdr, cdr, cdr."))
 ((name . "caddr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of car, cdr, cdr."))
 ((name . "cdaaar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr, car, car, car."))
 ((name . "cdaadr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr, car, car, cdr."))
 ((name . "cdaar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr, car, car."))
 ((name . "cdadar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr, car, cdr, car."))
 ((name . "cdaddr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr, car, cdr, cdr."))
 ((name . "cdadr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr, car, cdr."))
 ((name . "cddaar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr, cdr, car, car."))
 ((name . "cddadr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr, cdr, car, cdr."))
 ((name . "cddar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr, cdr, car."))
 ((name . "cdddar")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr, cdr, cdr, car."))
 ((name . "cddddr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr, cdr, cdr, cdr."))
 ((name . "cdddr")
  (signature lambda ((pair? pair)) *)
  (tags pure)
  (desc . "Composition of cdr, cdr, cdr."))
 ((name . "eval")
  (signature lambda (expr-or-def (environment environment-specifier)) *)
  (desc . "Evaluates expression in the specified environment and returns its value. Expression must be a valid Scheme expression represented as data, and environment-specifier must be a value returned by one of the three procedures described below. Implementations may extend eval to allow non-expression programs (definitions) as the first argument and to allow other values as environments, with the restriction that eval is not allowed to create new bindings in the environments associated with null-environment or scheme-report-environment."))
 ((name . "scheme-report-environment")
  (signature lambda ((integer? version)) environment)
  (desc . "Version must be the exact integer 5, corresponding to this revision of the Scheme report (the Revised5 Report on Scheme). Scheme-report-environment returns a specifier for an environment that is empty except for all bindings defined in this report that are either required or both optional and supported by the implementation.
Other values of version can be used to specify environments matching past revisions of this report, but their support is not required. An implementation will signal an error if version is neither 5 nor another value supported by the implementation.
The effect of assigning (through the use of eval) a variable bound in a scheme-report-environment (for example car) is unspecified. Thus the environments specified by scheme-report-environment may be immutable."))
 ((name . "null-environment")
  (signature lambda ((integer? version)) environment)
  (desc . "Version must be the exact integer 5, corresponding to this revision of the Scheme report (the Revised5 Report on Scheme).
Null-environment returns a specifier for an environment that is empty except for the (syntactic) bindings for all syntactic keywords defined in this report that are either required or both optional and supported by the implementation."))
 ((name . "acos") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "asin") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "atan")
  (signature
   case-lambda
   (((number? z)) number?)
   (((real? y) (real? x)) number?))
  (tags pure))
 ((name . "cos") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "exp") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "log") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "sin") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "sqrt")
  (signature lambda ((number? z)) number?)
  (tags pure)
  (desc . "Returns the principal square root of z. The result will have either positive real part, or zero real part and non-negative imaginary part."))
 ((name . "tan") (signature lambda ((number? z)) number?) (tags pure))
 ((name . "call-with-input-file")
  (signature lambda ((string? string) (procedure? proc)) *)
  (subsigs (proc (lambda ((input-port? port)) *)))
  (desc . "String should be a string naming a file, and proc should be a procedure that accepts one argument. For call-with-input-file, the file should already exist. The procedure calls proc with one argument: the port obtained by opening the named file for input. If the file cannot be opened, an error is signalled. If proc returns, then the port is closed automatically and the value(s) yielded by the proc is(are) returned. If proc does not return, then the port will not be closed automatically unless it is possible to prove that the port will never again be used for a read or write operation."))
 ((name . "call-with-output-file")
  (signature lambda ((string? string) (procedure? proc)) *)
  (subsigs (proc (lambda ((output-port? port)) *)))
  (desc . "String should be a string naming a file, and proc should be a procedure that accepts one argument. For call-with-output-file, the effect is unspecified if the file already exists. The procedure calls proc with one argument: the port obtained by opening the named file for output. If the file cannot be opened, an error is signalled. If proc returns, then the port is closed automatically and the value(s) yielded by the proc is(are) returned. If proc does not return, then the port will not be closed automatically unless it is possible to prove that the port will never again be used for a read or write operation."))
 ((name . "open-input-file")
  (signature lambda ((string? string)) input-port?)
  (desc . "Takes a string naming an existing file and returns an input port capable of delivering characters from the file. If the file cannot be opened, an error is signalled."))
 ((name . "open-output-file")
  (signature lambda ((string? string)) output-port?)
  (desc . "Takes a string naming an output file to be created and returns an output port capable of writing characters to a new file by that name. If the file cannot be opened, an error is signalled. If a file with the given name already exists, the effect is unspecified."))
 ((name . "with-input-from-file")
  (signature lambda ((string? string) (procedure? thunk)) *)
  (subsigs (thunk (lambda () *)))
  (desc . "String should be a string naming a file, and proc should be a procedure that accepts one argument. For call-with-input-file, the file should already exist. The file is opened for input, an input port connected to it is made the default value returned by current-input-port, and the thunk is called with no arguments. When the thunk returns, the port is closed and the previous default is restored. With-input-from-file returns the value(s) yielded by thunk . If an escape procedure is used to escape from the continuation of the procedure, its behavior is implementation dependent."))
 ((name . "with-output-to-file")
  (signature lambda ((string? string) (procedure? thunk)) *)
  (subsigs (thunk (lambda () *)))
  (desc . "String should be a string naming a file, and proc should be a procedure that accepts one argument. For call-with-output-file, the effect is unspecified if the file already exists. The file is opened for output, an output port connected to it is made the default value returned by current-output-port, and the thunk is called with no arguments. When the thunk returns, the port is closed and the previous default is restored. With-output-from-file returns the value(s) yielded by thunk . If an escape procedure is used to escape from the continuation of the procedure, its behavior is implementation dependent."))
 ((name . "delay")
  (signature syntax-rules () ((_ expression) promise?))
  (desc . "The delay construct is used together with the procedure force to implement lazy evaluation or call by need. (delay 〈expression〉) returns an object called a promise which at some point in the future may be asked (by the force procedure) to evaluate 〈expression〉, and deliver the resulting value. The effect of 〈expression〉 returning multiple values is unspecified."))
 ((name . "force")
  (signature lambda ((promise? promise)) *)
  (desc . "Forces the value of promise (see delay, section 4.2.5). If no value has been computed for the promise, then a value is computed and returned. The value of the promise is cached (or “memoized”) so that if it is forced a second time, the previously computed value is returned."))
 ((name . "load")
  (signature lambda ((string? filename)) undefined)
  (desc . "Filename should be a string naming an existing file containing Scheme source code. The load procedure reads expressions and definitions from the file and evaluates them sequentially. It is unspecified whether the results of the expressions are printed. The load procedure does not affect the values returned by current-input-port and current-output-port. Load returns an unspecified value.
Rationale: For portability, load must operate on source files. Its operation on other kinds of files necessarily varies among implementations."))
 ((name . "read") (signature case-lambda (() *) (((input-port? port)) *)))
 ((name . "transcript-on")
  (signature lambda ((string? filename)) undefined)
  (desc . "Filename must be a string naming an output file to be created. The effect of transcript-on is to open the named file for output, and to cause a transcript of subsequent interaction between the user and the Scheme system to be written to the file. The transcript is ended by a call to transcript-off, which closes the transcript file. Only one transcript may be in progress at any time, though some implementations may relax this restriction. The values returned by these procedures are unspecified."))
 ((name . "transcript-off")
  (signature lambda () undefined)
  (desc . "Ends transcript and closes transcript file. See transcript-on."))
 ((name . "display")
  (signature
   case-lambda
   ((obj) undefined)
   ((obj (output-port? port)) undefined))
  (desc . "Writes a representation of obj to the given port. Strings that appear in the written representation are not enclosed in doublequotes, and no characters are escaped within those strings. Character objects appear in the representation as if written by write-char instead of by write. Display returns an unspecified value. The port argument may be omitted, in which case it defaults to the value returned by current-output-port.
Rationale: Write is intended for producing machine-readable output and display is for producing human-readable output. Implementations that allow “slashification” within symbols will probably want write but not display to slashify funny characters in symbols."))
 ((name . "write")
  (signature
   case-lambda
   ((obj) undefined)
   ((obj (output-port? port)) undefined))
  (desc . "Writes a written representation of obj to the given port. Strings that appear in the written representation are enclosed in doublequotes, and within those strings backslash and doublequote characters are escaped by backslashes. Character objects are written using the #\\ notation. Write returns an unspecified value. The port argument may be omitted, in which case it defaults to the value returned by current-output-port.")))
