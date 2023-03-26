(((name . "define-record-type")
  (signature syntax-rules () ((_ type-spec constructor predicate field ...)))
  (subsigs
   (type-spec (pattern type-name (type-name parent)))
   (constructor
    (pattern #f constructor-name (constructor-name field-name ...)))
   (predicate (pattern #f predicate-name))
   (field (pattern
           field-name
           (field-name accessor-name)
           (field-name accessor-name modifier-name)))
   (parent (value rtd?)))
  (desc . "The semantics of a record type definition is the same as in R7RS-small (or SRFI 9, except that record types are generative). The record type definition macro-expands into a cluster of definitions that:
* define the <type name> as the record-type descriptor for the new record-type;
* defines a constructor for instances of the new record-type (unless the constructor spec is #f);
* defines a predicate that recognizes instances of the new record-type and its subtypes (unless the predicate spec is #f);
* defines an accessor for each field name;
* defines a mutator for each mutable field name. 

A record type definition extends R7RS-small with the following additional options:
* If a <parent> expression is specified, then it must evaluate to a record-type descriptor that serves as the parent record-type for the record-type being defined.
* If #f is specified for the constructor or predicate, then no constructor or predicate procedure is defined. (This is useful when the record-type being defined will be used as an abstract base class.)
* If the constructor name is specified as an identifier, then the constructor's arguments correspond to the fields of the parent (if any) followed by the new fields added by this record-type definition in the specified order. 

When a constructor spec is of the form (<constructor name> <field name> ...):
* Each of the field names can be either a field name declared in the same define-record-type form, or any of its ancestors' field names.
* If the record definition contains the same field name as one of its ancestors, it shadows the ancestor's field name for the purposes of the constructor. The constructor's argument initializes the child's slot, and the ancestor's slot of the same name is left uninitialized.
* It is an error if the same identifier appears more than once in the field names of the constructor spec.

These are not explicit in SRFI 99's syntactic layer section, but can be inferred from its description of the procedural layer.")))
