(((name . "make-record-type-descriptor")
  (signature
   lambda
   ((symbol? name)
    ((or #f record-type-descriptor?) parent)
    ((or #f symbol?) uid)
    (boolean? sealed?)
    (boolean? opaque?)
    (vector? fields))
   record-type-descriptor?)
  (desc . "Returns a record-type descriptor, or rtd, representing a record type distinct from all built-in types and other record types.
The name argument must be a symbol. It names the record type, and is intended purely for informational purposes and may be used for printing by the underlying Scheme system.
The parent argument must be either #f or an rtd. If it is an rtd, the returned record type, t, extends the record type p represented by parent. An exception with condition type &assertion is raised if parent is sealed (see below).
The uid argument must be either #f or a symbol. If uid is a symbol, the record-creation operation is nongenerative i.e., a new record type is created only if no previous call to make-record-type-descriptor was made with the uid. If uid is #f, the record-creation operation is generative, i.e., a new record type is created even if a previous call to make-record-type-descriptor was made with the same arguments.
If make-record-type-descriptor is called twice with the same uid symbol, the parent arguments in the two calls must be eqv?, the fields arguments equal?, the sealed? arguments boolean-equivalent (both #f or both true), and the opaque? arguments boolean-equivalent. If these conditions are not met, an exception with condition type &assertion is raised when the second call occurs. If they are met, the second call returns, without creating a new record type, the same record-type descriptor (in the sense of eqv?) as the first call.
Note: Users are encouraged to use symbol names constructed using the UUID namespace [10] (for example, using the record-type name as a prefix) for the uid argument. 
The sealed? flag must be a boolean. If true, the returned record type is sealed, i.e., it cannot be extended.
The opaque? flag must be a boolean. If true, the record type is opaque. If passed an instance of the record type, record? returns #f. Moreover, if record-rtd (see “Inspection” below) is called with an instance of the record type, an exception with condition type &assertion is raised. The record type is also opaque if an opaque parent is supplied. If opaque? is #f and an opaque parent is not supplied, the record is not opaque.
The fields argument must be a vector of field specifiers. Each field specifier must be a list of the form (mutable name) or a list of the form (immutable name). Each name must be a symbol and names the corresponding field of the record type; the names need not be distinct. A field identified as mutable may be modified, whereas, when a program attempts to obtain a mutator for a field identified as immutable, an exception with condition type &assertion is raised. Where field order is relevant, e.g., for record construction and field access, the fields are considered to be ordered as specified, although no particular order is required for the actual representation of a record instance.
The specified fields are added to the parent fields, if any, to determine the complete set of fields of the returned record type. If fields is modified after make-record-type-descriptor has been called, the effect on the returned rtd is unspecified.
A generative record-type descriptor created by a call to make-record-type-descriptor is not eqv? to any record-type descriptor (generative or nongenerative) created by another call to make-record-type-descriptor. A generative record-type descriptor is eqv? only to itself, i.e., (eqv? rtd1 rtd2) iff (eq? rtd1 rtd2). Also, two nongenerative record-type descriptors are eqv? iff they were created by calls to make-record-type-descriptor with the same uid arguments."))
 ((name . "record-type-descriptor?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if the argument is a record-type descriptor, #f otherwise."))
 ((name . "make-record-constructor-descriptor")
  (signature
   lambda
   ((record-type-descriptor? rtd)
    ((or constructor-descriptor #f) parent-constructor-descriptor)
    ((or #f procedure?) protocol))
   constructor-descriptor)
  (subsigs (protocol (lambda ((procedure? p)) procedure?)))
  (desc . "Returns a record-constructor descriptor (or constructor descriptor for short) that specifies a record constructor (or constructor for short), that can be used to construct record values of the type specified by rtd, and which can be obtained via record-constructor. A constructor descriptor can also be used to create other constructor descriptors for subtypes of its own record type. Rtd must be a record-type descriptor. Protocolmust be a procedure or #f. If it is #f, a default protocol procedure is supplied.
If protocol is a procedure, it is handled analogously to the protocol expression in a define-record-type form.
If rtd is a base record type and protocol is a procedure, parent-constructor-descriptor must be #f. In this case, protocol is called by record-constructor with a single argument p. P is a procedure that expects one argument for every field of rtd and returns a record with the fields of rtd initialized to these arguments. The procedure returned by protocol should call p once with the number of arguments p expects and return the resulting record as shown in the simple example below:
(lambda (p)
  (lambda (v1 v2 v3)
    (p v1 v2 v3)))

Here, the call to p returns a record whose fields are initialized with the values of v1, v2, and v3. The expression above is equivalent to (lambda (p) p). Note that the procedure returned by protocol is otherwise unconstrained; specifically, it can take any number of arguments.
If rtd is an extension of another record type parent-rtd and protocol is a procedure, parent-constructor-descriptor must be a constructor descriptor of parent-rtd or #f. If parent-constructor-descriptor is a constructor descriptor, protocol it is called by record-constructor with a single argument n, which is a procedure that accepts the same number of arguments as the constructor of parent-constructor-descriptor and returns a procedure p that, when called, constructs the record itself. The p procedure expects one argument for every field of rtd (not including parent fields) and returns a record with the fields of rtd initialized to these arguments, and the fields of parent-rtd and its parents initialized as specified by parent-constructor-descriptor.
The procedure returned by protocol should call n once with the number of arguments n expects, call the procedure p it returns once with the number of arguments p expects and return the resulting record. A simple protocol in this case might be written as follows:
(lambda (n)
  (lambda (v1 v2 v3 x1 x2 x3 x4)
    (let ((p (n v1 v2 v3)))
      (p x1 x2 x3 x4))))

This passes arguments v1, v2, v3 to n for parent-constructor-descriptor and calls p with x1, ..., x4 to initialize the fields of rtd itself.
Thus, the constructor descriptors for a record type form a sequence of protocols parallel to the sequence of record-type parents. Each constructor descriptor in the chain determines the field values for the associated record type. Child record constructors need not know the number or contents of parent fields, only the number of arguments accepted by the parent constructor.
Protocol may be #f, specifying a default constructor that accepts one argument for each field of rtd (including the fields of its parent type, if any). Specifically, if rtd is a base type, the default protocol procedure behaves as if it were (lambda (p) p). If rtd is an extension of another type, then parent-constructor-descriptor must be either #f or itself specify a default constructor, and the default protocol procedure behaves as if it were:
(lambda (n)
  (lambda (v1 ... vj x1 ... xk)
    (let ((p (n v1 ... vj)))
      (p x1 ... xk))))

The resulting constructor accepts one argument for each of the record type's complete set of fields (including those of the parent record type, the parent's parent record type, etc.) and returns a record with the fields initialized to those arguments, with the field values for the parent coming before those of the extension in the argument list. (In the example, j is the complete number of fields of the parent type, and k is the number of fields of rtd itself.)
If rtd is an extension of another record type, and parent-constructor-descriptor or the protocol of parent-constructor-descriptor is #f, protocol must also be #f, and a default constructor descriptor as described above is also assumed."))
 ((name . "record-constructor")
  (signature
   lambda
   ((constructor-descriptor constructor-descriptor))
   procedure?)
  (desc . "Calls the protocol of constructor-descriptor (as described for make-record-constructor-descriptor) and returns the resulting constructor constructor for records of the record type associated with constructor-descriptor."))
 ((name . "record-predicate")
  (signature lambda ((record-type-descriptor? rtd)) procedure?)
  (subsigs (return (lambda (obj) boolean?)))
  (desc . "Returns a procedure that, given an object obj, returns #t if obj is a record of the type represented by rtd, and #f otherwise."))
 ((name . "record-accessor")
  (signature lambda ((record-type-descriptor? rtd) (integer? k)) procedure?)
  (subsigs (return (lambda (record) *)))
  (desc . "K must be a valid field index of rtd. The record-accessor procedure returns a one-argument procedure whose argument must be a record of the type represented by rtd. This procedure returns the value of the selected field of that record.
The field selected corresponds to the kth element (0-based) of the fields argument to the invocation of make-record-type-descriptor that created rtd. Note that k cannot be used to specify a field of any type rtd extends."))
 ((name . "record-mutator")
  (signature lambda ((record-type-descriptor? rtd) (integer? k)) procedure?)
  (subsigs (return (lambda (record obj) undefined)))
  (desc . "K must be a valid field index of rtd. The record-mutator procedure returns a two-argument procedure whose arguments must be a record record r of the type represented by rtd and an object obj. This procedure stores obj within the field of r specified by k. The k argument is as in record-accessor. If k specifies an immutable field, an exception with condition type &assertion is raised. The mutator returns unspecified values.")))
