(((name . "define-record-type")
  (signature syntax-rules () ((_ type-spec constructor predicate field ...)))
  (subsigs
   (type-spec (pattern type-name (type-name parent)))
   (constructor
    (pattern #f #t constructor-name (constructor-name field-name ...)))
   (predicate (pattern #f #t predicate-name))
   (field (pattern
           field-name
           (field-name)
           (field-name accessor-name)
           (field-name accessor-name modifier-name)))
   (parent (value rtd?)))
  (desc . "The semantics of a record type definition is the same as in SRFI 9: the record type definition macro-expands into a cluster of definitions that
* define the <type name> as the record-type descriptor for the new record-type;
* defines a constructor for instances of the new record-type (unless the constructor spec is #f);
* defines a predicate that recognizes instances of the new record-type and its subtypes (unless the predicate spec is #f);
* defines an accessor for each field name;
* defines a mutator for each mutable field name.

An ERR5RS record type definition extends SRFI 9 with the following additional options:
* If a <parent> expression is specified, then it must evaluate to an rtd that serves as the parent record-type for the record-type being defined.
* If #f is specified for the constructor or predicate, then no constructor or predicate procedure is defined. (This is useful when the record-type being defined will be used as an abstract base class.)
* If #t is specified for the constructor or predicate, then the name of the constructor is the type name prefixed by make-, and the name of the predicate is the type name followed by a question mark (?).
* If the constructor name is specified as #t or as an identifier, then the constructor's arguments correspond to the fields of the parent (if any) followed by the new fields added by this record-type definition.
* If a field spec consists of a single identifier, then
 - the field is immutable;
 - the name of its accessor is the type name followed by a hyphen (-) followed by the field name.
* If a field spec consists of a list of one identifier, then
 - the field is mutable;
 - the name of its accessor is the type name followed by a hyphen (-) followed by the field name;
 - the name of its mutator is the type name followed by a hyphen (-) followed by the field name followed by -set!.")))
