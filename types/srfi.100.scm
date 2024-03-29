(((name . "define-lambda-object")
  (signature syntax-rules ()
             ((_ group-spec field-spec)))
  (subsigs
    (group-spec (pattern group
                         (group parent-group ...)))
    (parent-group (pattern group
                           (group)))
    (field-spec (pattern (_append "required-field... " "optional-field... " "automatic-field...")))
    (required-field (pattern field
                             (field)))
    (optional-field (pattern (field default)
                             ((field) default)
                             ('field default)))
    (automatic-field (pattern (,field default)
                              ((,field) default)
                              (',field default)
                              (`,field default)
                              (,,field default)
                              ((,,field) default))))
  (desc . "This SRFI introduces a macro, DEFINE-LAMBDA-OBJECT which defines a set of procedures, that is, a group, two constructors, and a predicate. The constructors also make a group of procedures, namely lambda objects. The macro extends DEFINE-RECORD-TYPE (SRFI 9) in being more general but much less general than DEFCLASS (CLOS). The macro has no explicit field accessors and mutators but parent groups, required fields, optional fields, automatic fields, read-write fields, read-only fields, inaccessible hidden fields, immutable virtual fields, and common sharing fields.
The name of <constructor> is generated by prefixing `make-' to the group name, or by prefixing `make-' and postfixing `-by-name' to the group name. The name of <predicate> is generated by adding a question mark (`?') to the end of the group name.
The <group> and <field> must be identifiers.
Each <default> is an <expression> that is evaluated in an environment that the values of all the previous <field>s are visible. There is one exception to this rule. The <default>s of <automatic common field>s are evaluated in the outer environment of the define-lambda-object form, and their values are visible as the <default>s of the other fields are evaluated.
The define-lambda-object form is a definition and can appear anywhere any other <definition> can appear. Each time define-lambda-object form is evaluated, a new group is created with distinct <group>, <constructor>, and <predicate> procedures.
The <group> is bound to a procedure of one argument. Like a gene, it has information on its <parent group>s, <constructor>s, <predicate>, and the number and properties of <field>s. And they are checked out whenever define-lambda-object form is evaluated. In case of inheritance, all the <field>s of <parent group>s must exist in the <field spec> of the child group, irrespectively of the order. Otherwise an error is signaled. In addition, the properties (mutability, sort of field, and default expression) of <field>s of unamendable groups must be preserved in contrast with those of amendable groups. Otherwise an error is signaled.
The <constructor> is bound to a procedure that takes at least as many arguments as the number of <required field>s. Whenever it is called, it returns an object of the <group>, namely a procedure, which has information on its own group and all that goes with it. Its first argument must be a symbol of the same name as <field>. Otherwise an error is signaled. The object becomes an accessor procedure of each <field> in case of one argument and a mutator procedure of each <field> in case of two arguments where the second argument is a new field value.
The names of <field>s are used to access the <field>s as symbols of the same names. So they must be distinct. Otherwise an error is signaled. The read-write fields can be modified, whereas any attempt to modify the values of the read-only fields via mutators signals an error. Note: The read-only fields are not immutable. Their values, for instance, can be modified by other fields whose values work like their mutators.
The <required field> is initialized to the first one of the remaining arguments. If there are no more remaining arguments, an error is signaled.
The initialization of the <optional field>s is done by two types of <constructor>s:
1. <make-`group-name'> constructor The initialization method of <optional field>s is the same as that of <required field>s except that the field is bound to the <default> instead of signaling an error if there are no more remaining arguments.
2. <make-`group-name'-by-name> constructor The name used at a call site for the corresponding <optional field> is a symbol of the same name as the <field>. The remaining arguments are sequentially interpreted as a series of pairs, where the first member of each pair is a field name and the second is the corresponding value. If there is no element for a particular field name, the field is initialized to the <default>.
The <automatic common field>s are initialized to each corresponding <default> that is evaluated at the time the define-lambda-object form is evaluated, and the values are shared with all the lambda objects that are maded by the constructors of the define-lambda-object form. The other <automatic field>s except <automatic virtual field>s are initialized to each corresponding <default> that is evaluated at the time the lambda object is made by a constructor. The <hidden field> is an externally nonexistent field, that is, the field is invisible outside of the define-lambda-object form but visible inside of it. On the contrary, the <virtual field> is an internally nonexistent field whose <default> is evaluated each time when the field is accessed.

The <predicate> is a predicate procedure that returns #t for objects constructed by <constructor> or <constructor>s for child groups and #f for everything else.")))
