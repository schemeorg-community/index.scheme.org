(((name . "check-arg")
  (signature syntax-rules ()
             ((_ predicate arg caller))
             ((_ predicate arg)))
  (subsigs (predicate (lambda (obj) *)))
  (desc . "Guarantees that the arg (evaluated) conforms to the predicate (evaluated). Implementations can enforce the predicate check in all the code that follows, but are not required to. It is an error if predicate returns #f when called on arg. Otherwise, return value is unspecified. Implementations may use optional caller (evaluated) argument as the error who/origin if/when signaling a checking error."))
 ((name . "values-checked")
  (signature syntax-rules ()
             ((_ (predicate ...) value ...)
              (values * ...)))
  (subsigs (predicate (lambda (obj) *)))
  (desc . "Guarantees that the values (evaluated) abide by the given predicates (the number of values and predicates should match) and returns them as multiple values. It is an error if any of the predicates returns false. Implementations may choose to coerce the values when the types are compatible (e.g. integer -> inexact)."))
 ((name . "check-case")
  (signature syntax-rules (=> else) 
             ((_ key (predicate expression ...) ...))
             ((_ key (predicate expression ...) ... (else expression ...))))
  (subsigs
   (predicate (lambda (obj) *)))
  (desc . "check-case checks whether the value satisfies one of the predicates. If any of the predicates is satisfied, it evaluates the body corresponding to the first one that is satisfied. If none of the predicates is satisfied and there’s an else clause, it evaluates to the body of that clause; and if there’s it is an error if there’s no else clause and no other clause is satisfied."))
 ((name . "lambda-checked")
  (signature syntax-rules ()
             ((_ (arg ...) body ...)
              procedure?))
  (subsigs
    (arg 
      (pattern
        name
        (name predicate)))
    (predicate (lambda (obj) *)))
  (desc . "A regular lambda, but with any argument (except the rest argument) optionally having the form (name predicate) (as compared to default single-symbol form). Arguments of this extended form are guaranteed to satisfy the respective (evaluated) predicate. At least on procedure application time. This guarantee might be extended for all the procedure body, including for any modification and shadowing, at implementors' will. It is an error if either of the arguments does not satisfy the predicate."))
 ((name . "case-lambda-checked")
  (signature syntax-rules ()
             ((_ clause ...)
              procedure?))
  (subsigs
   (clause
    (pattern
      ((arg ...) body ...)
      ((arg arg ... . args) body ...)
      (args body ...)))
   (arg
     (pattern
       name
       (name predicate)))
   (predicate (lambda (obj) *)))
  (desc . "Same as case-lambda, but with any argument taking a form of (name predicate) to be checked. See lambda-checked for other details."))
 ((name . "define-checked")
  (signature syntax-rules ()
             ((_ (proc-name arg ...) body ...)
              procedure?)
             ((_ name predicate value)))
  (subsigs
    (arg 
      (pattern
        name
        (name predicate)))
    (predicate (lambda (obj) *)))
  (desc . "Defines a procedure or variable satisfying the given predicates. For procedures, effectively equal to define+lambda-checked. For variables, checks the value (and, if implementation supports that, all the subsequent modifications) for the predicate match."))
 ((name . "define-record-type-checked")
  (signature syntax-rules ()
             ((_ type-name (constructor arg-name ...) predicate-name field ...)))
  (subsigs
   (field
    (pattern
     (name predicate accessor)
     (name predicate accessor modifier)))
   (predicate (lambda (obj) *)))
  (desc . "Defines a record type with checked constructor and field accessors/modifiers. type-name, constructor, and predicate are the same as R7RS define-record-type's (note especially the constructor—checks are not allowed in it, only arg-name symbols!) Fields are either of the form (field-name predicate accessor-name) or (field-name predicate accessor-name modifier-name). These ensure that accessor and modifier return checked data and check new data respectively. It is an error if any of the checks are not successful.")))
