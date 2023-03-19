(((name . "&condition")
  (signature value record-type-descriptor?)
  (desc . "Simple conditions are records of subtypes of the &condition record type. The &condition type has no fields and is neither sealed nor opaque."))
 ((name . "condition")
  (signature lambda ((condition? condition1) ...) condition?)
  (tags pure)
  (desc . "The condition procedure returns a condition object with the components of the conditions as its components, in the same order, i.e., with the components of condition1 appearing first in the same order as in condition1, then with the components of condition2, and so on. The returned condition is compound if the total number of components is zero or greater than one. Otherwise, it may be compound or simple."))
 ((name . "simple-conditions")
  (signature lambda ((condition? condition)) list?)
  (tags pure)
  (desc . "The simple-conditions procedure returns a list of the components of condition, in the same order as they appeared in the construction of condition. The returned list is immutable. If the returned list is modified, the effect on condition is unspecified.
Note: Because condition decomposes its arguments into simple conditions, simple-conditions always returns a “flattened” list of simple conditions."))
 ((name . "condition?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a (simple or compound) condition, otherwise returns #f."))
 ((name . "condition-predicate")
  (signature lambda ((record-type-descriptor? rtd)) procedure?)
  (subsigs (return (lambda (obj) boolean?)))
  (tags pure)
  (desc . "Rtd must be a record-type descriptor of a subtype of &condition. The condition-predicate procedure returns a procedure that takes one argument. This procedure returns #t if its argument is a condition of the condition type represented by rtd, i.e., if it is either a simple condition of that record type (or one of its subtypes) or a compound conditition with such a simple condition as one of its components, and #f otherwise."))
 ((name . "condition-accessor")
  (signature
   lambda
   ((record-type-descriptor? rtd) (procedure? proc))
   procedure?)
  (subsigs (proc (lambda (record) *)) (return (lambda (record) *)))
  (tags pure)
  (desc . "Rtd must be a record-type descriptor of a subtype of &condition. Proc should accept one argument, a record of the record type of rtd. The condition-accessor procedure returns a procedure that accepts a single argument, which must be a condition of the type represented by rtd. This procedure extracts the first component of the condition of the type represented by rtd, and returns the result of applying proc to that component."))
 ((name . "define-condition-type")
  (signature
   syntax-rules
   ()
   ((_ condition-type supertype constructor predicate field-spec1 ...)))
  (subsigs (field-spec (pattern (field accessor))))
  (desc . "The define-condition-type form expands into a record-type definition for a record type <condition-type> (see section 6.2). The record type will be non-opaque, non-sealed, and its fields will be immutable. It will have <supertype> has its parent type. The remaining identifiers will be bound as follows:
* <Constructor> is bound to a default constructor for the type (see section 6.3): It accepts one argument for each of the record type's complete set of fields (including parent types, with the fields of the parent coming before those of the extension in the arguments) and returns a condition object initialized to those arguments.
* <Predicate> is bound to a predicate that identifies conditions of type <condition-type> or any of its subtypes.
* Each <accessor> is bound to a procedure that extracts the corresponding field from a condition of type <condition-type>."))
 ((group
    ((name . "&message")
     (signature value record-type-descriptor?))
    ((name . "make-message-condition")
     (signature lambda (message) message-condition?)
     (tags pure))
    ((name . "message-condition?")
     (signature lambda (obj) boolean?)
     (tags pure predicate))
    ((name . "condition-message")
     (signature lambda ((message-condition? condition)) *)
     (tags pure)))
  (desc . "This condition type could be defined by
(define-condition-type &message &condition
  make-message-condition message-condition?
  (message condition-message))

It carries a message further describing the nature of the condition to humans."))
 ((group
    ((name . "&warning") 
     (signature value record-type-descriptor?))
    ((name . "make-warning") (signature lambda () warning?) (tags pure))
    ((name . "warning?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &warning &condition
  make-warning warning?)

This type describes conditions that do not, in principle, prohibit immediate continued execution of the program, but may interfere with the program's execution later."))
 ((group
    ((name . "&serious") 
     (signature value record-type-descriptor?))
    ((name . "make-serious-condition") (signature lambda () serious?) (tags pure))
    ((name . "serious-condition?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &serious &condition
  make-serious-condition serious-condition?)

This type describes conditions serious enough that they cannot safely be ignored. This condition type is primarily intended as a supertype of other condition types."))
 ((group
    ((name . "&error") 
     (signature value record-type-descriptor?))
    ((name . "make-error") (signature lambda () error?) (tags pure))
    ((name . "error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . " This condition type could be defined by
(define-condition-type &error &serious
  make-error error?)

This type describes errors, typically caused by something that has gone wrong in the interaction of the program with the external world or the user."))
 ((group
    ((name . "&violation") 
     (signature value record-type-descriptor?))
    ((name . "make-violation") (signature lambda () violation?) (tags pure))
    ((name . "violation?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &violation &serious
  make-violation violation?)

This type describes violations of the language standard or a library standard, typically caused by a programming error."))
 ((group
    ((name . "&assertion") 
     (signature value record-type-descriptor?))
    ((name . "make-assertion-violation")
     (signature lambda () assertion-violation?)
     (tags pure))
    ((name . "assertion-violation?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &assertion &violation
  make-assertion-violation assertion-violation?)

This type describes an invalid call to a procedure, either passing an invalid number of arguments, or passing an argument of the wrong type."))
 ((group
    ((name . "&irritants") 
     (signature value record-type-descriptor?))
    ((name . "make-irritants-condition")
     (signature lambda ((list? irritants)) irritants-condition?)
     (tags pure))
    ((name . "irritants-condition?")
     (signature lambda (obj) boolean?)
     (tags pure predicate))
    ((name . "condition-irritants")
     (signature lambda ((irritants-condition? condition)) list?)
     (tags pure)))
  (desc . "This condition type could be defined by
(define-condition-type &irritants &condition
  make-irritants-condition irritants-condition?
  (irritants condition-irritants))

Irritants should be a list of objects. This condition provides additional information about a condition, typically the argument list of a procedure that detected an exception. Conditions of this type are created by the error and assertion-violation procedures of report section on \"Errors and violations\"."))
 ((group
    ((name . "&who") 
     (signature value record-type-descriptor?))
    ((name . "make-who-condition")
     (signature lambda (((or string? symbol?) who)) who-condition?)
     (tags pure))
    ((name . "who-condition?")
     (signature lambda (obj) boolean?)
     (tags pure predicate))
    ((name . "condition-who")
     (signature lambda ((who-condition? condition)) (or string? symbol?))
     (tags pure)))
  (desc . "This condition type could be defined by
(define-condition-type &who &condition
  make-who-condition who-condition?
  (who condition-who))

Who should be a symbol or string identifying the entity reporting the exception. Conditions of this type are created by the error and assertion-violation procedures (report section on \"Errors and violations\"), and the syntax-violation procedure (section on \"Syntax violations\")."))
 ((group
    ((name . "&non-continuable") 
     (signature value record-type-descriptor?))
    ((name . "make-non-continuable-violation")
     (signature lambda () non-continuable-violation?)
     (tags pure))
    ((name . "non-continuable-violation?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &non-continuable &violation
  make-non-continuable-violation
  non-continuable-violation?)

This type indicates that an exception handler invoked via raise has returned."))
 ((group
    ((name . "&implementation-restriction")
     (signature value record-type-descriptor?))
    ((name . "make-implementation-restriction-violation")
     (signature lambda () implementation-restriction-violation?)
     (tags pure))
    ((name . "implementation-restriction-violation?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &implementation-restriction
    &violation
  make-implementation-restriction-violation
  implementation-restriction-violation?)

This type describes a violation of an implementation restriction allowed by the specification, such as the absence of representations for NaNs and infinities. (See section 11.3.)"))
 ((group
    ((name . "&lexical") 
     (signature value record-type-descriptor?))
    ((name . "make-lexical-violation")
     (signature lambda () lexical-violation?)
     (tags pure))
    ((name . "lexical-violation?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . " This condition type could be defined by
(define-condition-type &lexical &violation
  make-lexical-violation lexical-violation?)

This type describes syntax violations at the level of the datum syntax."))
 ((group
    ((name . "&syntax") 
     (signature value record-type-descriptor?))
    ((name . "make-syntax-violation")
     (signature lambda (form subform) syntax-violation?)
     (tags pure))
    ((name . "syntax-violation?")
     (signature lambda (obj) boolean?)
     (tags pure predicate))
    ((name . "syntax-violation-form")
     (signature lambda ((syntax-violation? condition)) *)
     (tags pure))
    ((name . "syntax-violation-subform")
     (signature lambda ((syntax-violation? condition)) *)
     (tags pure)))
  (desc . " This condition type could be defined by
(define-condition-type &syntax &violation
  make-syntax-violation syntax-violation?
  (form syntax-violation-form)
  (subform syntax-violation-subform))

This type describes syntax violations. Form should be the erroneous syntax object or a datum representing the code of the erroneous form. Subform should be an optional syntax object or datum within the erroneous form that more precisely locates the violation. It can be #f to indicate the absence of more precise information."))
 ((group
    ((name . "&undefined") 
     (signature value record-type-descriptor?)
     (desc . "This type describes unbound identifiers in the program."))
    ((name . "make-undefined-violation")
     (signature lambda () undefined-violation?)
     (tags pure))
    ((name . "undefined-violation?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &undefined &violation
  make-undefined-violation undefined-violation?)

This type describes unbound identifiers in the program.")))
