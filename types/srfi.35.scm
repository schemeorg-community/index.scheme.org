(((name . "make-condition-type")
  (signature
   lambda
   ((symbol? id) (condition-type? parent) (list? field-names))
   condition-type?)
  (tags pure)
  (desc . "Make-condition-type returns a new condition type. Id must be a symbol that serves as a symbolic name for the condition type. Parent must itself be a condition type. Field-names must be a list of symbols. It identifies the fields of the conditions associated with the condition type.
Field-names must be disjoint from the field names of parent and its ancestors."))
 ((name . "condition-type?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Condition-type? is a predicate for condition types: it returns #t if thing is a condition type, and #f otherwise"))
 ((name . "make-condition")
  (signature
   lambda
   ((condition-type? type) (symbol? field-name1) value1 ...)
   condition?)
  (tags pure)
  (desc . "Make-condition creates a condition value belonging condition type type. The following arguments must be, in turn, a field name and an arbitrary value. There must be such a pair for each field of type and its direct and indirect supertypes. Make-condition returns the condition value, with the argument values associated with their respective fields."))
 ((name . "condition?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Condition? is a predicate for conditions: it returns #t if thing is a condition type, and #f otherwise"))
 ((name . "condition-has-type?")
  (signature lambda ((condition? condition) (condition-type? type)) boolean?)
  (tags pure)
  (desc . "Condition-has-type? tests if condition condition belongs to condition type condition-type. It returns #t if any of condition 's types includes condition-type either directly or as an ancestor and #f otherwise. It is an error if condition is not a condition, or if condition-type is not a condition type."))
 ((name . "condition-ref")
  (signature lambda ((condition? condition) (symbol? field-name)) *)
  (tags pure)
  (desc . "Condition must be a condition, and field-name a symbol. Moreover, condition must belong to a condition type which has a field name called field-name, or one of its (direct or indirect) supertypes must have the field. Condition-ref returns the value associated with field-name.
It is an error to refer to a field the condition does not have."))
 ((name . "make-compound-condition")
  (signature
   lambda
   ((condition? condition1) (condition? condition2) ...)
   condition?)
  (tags pure)
  (desc . "Make-compound-condition returns a compound condition belonging to all condition types that the conditioni belong to.
Condition-ref, when applied to a compound condition will return the value from the first of the conditioni that has such a field."))
 ((name . "extract-condition")
  (signature lambda ((condition? condition) (condition-type? type)) condition?)
  (tags pure)
  (desc . "Condition must be a condition belonging to condition-type. Extract-condition returns a condition of condition type condition-type with the field values specified by condition.
If condition is a compound condition, extract-condition extracts the field values from the subcondition belonging to condition-type that appeared first in the call to make-compound-condition that created the the condition. The returned condition may be newly created"))
 ((name . "define-condition-type")
  (signature
   syntax-rules
   ()
   ((_ condition-type supertype predicate field-spec ...)))
  (subsigs
   (field-spec (pattern (field accessor)))
   (supertype (value condition-type?)))
  (desc . "This defines a new condition type. <Condition-type>, <supertypes>, and <predicate> must all be identifiers. Define-condition-type defines an identifier <condition-type> to some value describing the condition type. <supertype> must be the name of a previously defined condition type.
Define-condition-type also defines <predicate> to a predicate that identifies conditions associated with that type, or with any of its subtypes.
Each <field-spec> must be of the form ( <field> <accessor>) where both <field> and <accessor> must be identifiers. Define-condition-type defines each <accessor> to a procedure which extracts the value of the named field from a condition associated with this condition type."))
 ((name . "condition")
  (signature syntax-rules () ((_ type-field-binding ...) condition?))
  (subsigs
   (type-field-binding (pattern (condition-type field-binding)))
   (field-binding (pattern (field exp)))
   (condition-type (value condition-type?)))
  (desc . "This creates a condition value. Each <type-field-binding> must be of the form ( <condition-type> <field-binding> ...) Each <field-binding> must be of the form ( <field> <exp>) where <field> is a field identifier from the definition of <condition-type>.
The <exp> are evaluated in some unspecified order; their values can later be extracted from the condition object via the accessors of the associated condition types or their supertypes.
The condition returned by condition is created by a call of form
(make-compound-condition
  (make-condition <condition-type> '<field-name> <value>...)
  ...)

with the condition types retaining their order from thecondition form. The field names and values are duplicated as necessary as described below.
Each <type-field-binding> must contain field bindings for all fields of <condition-type> without duplicates. There is an exception to this rule: if a field binding is missing, and the field belongs to a supertype shared with one of the other <type-field-binding> subforms, then the value defaults to that of the first such binding in the condition form."))
 ((name . "&condition")
  (signature value condition-type?)
  (desc . "This is the root of the entire condition type hierarchy. It has a no fields."))
 ((group
    ((name . "&message") (signature value condition-type?))
    ((name . "message-condition?")
     (signature lambda (obj) boolean?)
     (tags pure predicate))
    ((name . "condition-message")
     (signature lambda ((message-condition? condition)) *)
     (tags pure)))
  (desc . "This condition type could be defined by
(define-condition-type &message &condition
  message-condition?
  (message condition-message))

It carries a message further describing the nature of the condition to humans."))
 ((group
    ((name . "&serious") (signature value condition-type?))
    ((name . "serious-condition?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &serious &condition
  serious-condition?)

This type describes conditions serious enough that they cannot safely be ignored. This condition type is primarily intended as a supertype of other condition types."))
 ((group
    ((name . "&error") (signature value condition-type?))
    ((name . "error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &error &serious
  error?)

This condition describes errors, typically caused by something that has gone wrong in the interaction of the program with the external world or the user.")))
