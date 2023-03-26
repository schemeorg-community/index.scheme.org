(((name . "make-rtd")
  (signature
   case-lambda
   (((symbol? name) (vector? fieldspecs)) rtd?)
   (((symbol? name) (vector? fieldspecs) ((or rtd? #f) parent)) rtd?))
  (tags pure)
  (desc . "name is a symbol, which matters only to the rtd-name procedure of the inspection layer. fieldspecs is a vector of field specifiers, where each field specifier is one of
* a symbol naming the (mutable) field;
* a list of the form (mutable name), where name is a symbol naming the mutable field;
* a list of the form (immutable name), where name is a symbol naming the immutable field.

The optional parent is an rtd or #f. It is an error for any of the symbols in fieldspecs to name more than one of the fields specified by fieldspecs, but the field names in fieldspecs may shadow field names in the parent record-type.

Implementations may wish to extend this procedure to support the non-generative, sealed, and/or opaque features of the R6RS. The recommended way to support those features is to allow any combination of the following arguments to follow the optional parent argument:
* The symbol sealed means the new rtd cannot be used as the parent of other rtds.
* The symbol opaque means the record? predicate will not recognize instances of the new rtd.
* The symbol uid, following by another symbol id, means the new rtd is non-generative with uid id. The semantics of this extension is the same as described by the R6RS.

The recommendation above is not binding on implementations of SRFI 99. There are other ways to realize sealed, opaque, or non-generative rtds.
Returns an R6RS-compatible record-type descriptor."))
 ((name . "rtd?") (signature lambda (obj) boolean?) (tags pure predicate) (desc . "Equivalent to the record-type-descriptor? procedure of the R6RS."))
 ((name . "rtd-constructor")
  (signature
   case-lambda
   (((rtd? rtd)) procedure?)
   (((rtd? rtd) (vector? fieldspec)) procedure?))
  (subsigs (return (lambda (fieldvalue ...) record?)))
  (tags pure)
  (desc . "rtd is a record-type descriptor, and fieldspecs is an optional vector of symbols.
If no fieldspecs argument is supplied, then rtd-constructor returns a procedure that expects one argument for each field of the record-type described by rtd and returns an instance of that record-type with its fields initialized to the corresponding arguments. Arguments that correspond to the fields of the record-type's parent (if any) come first.
If fieldspecs is supplied, then rtd-constructor returns a procedure that expects one argument for each element of fieldspecs and returns an instance of the record-type described by rtd with the named fields initialized to the corresponding arguments.
It is an error if some symbol occurs more than once in fieldspecs. Fields of a derived record-type shadow fields of the same name in its parent; the fieldspecs argument cannot be used to initialize a shadowed field."))
 ((name . "rtd-predicate")
  (signature lambda ((rtd? rtd)) procedure?)
  (subsigs (return (lambda (obj) boolean?)))
  (tags pure)
  (desc . "Equivalent to the record-predicate procedure of the R6RS."))
 ((name . "rtd-accessor")
  (signature lambda ((rtd? rtd) (symbol? field)) procedure?)
  (subsigs (return (lambda ((record? rec)) *)))
  (tags pure)
  (desc . "field is a symbol that names a field of the record-type described by the record-type descriptor rtd. Returns a unary procedure that accepts instances of rtd (or any record-type that inherits from rtd) and returns the current value of the named field.
Fields in derived record-types shadow fields of the same name in a parent record-type."))
 ((name . "rtd-mutator")
  (signature lambda ((rtd? rtd) (symbol? field)) procedure?)
  (subsigs (return (lambda ((record? rec) value) undefined)))
  (tags pure)
  (desc . "field is a symbol that names a field of the record-type described by the record-type descriptor rtd. Returns a binary procedure that accepts instances of rtd (or any record-type that inherits from rtd) and a new value to be stored into the named field, performs that side effect, and returns an unspecified value.
Fields in derived record-types shadow fields of the same name in a parent record-type.")))
