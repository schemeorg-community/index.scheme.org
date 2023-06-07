(((name . "ref")
  (signature case-lambda
             ((object field) *)
             ((object field default) *))
  (tags pure)
  (desc . "Returns the value for field in object. It is an error if object has no field identified by field. If object is of a \"sparse\" type, meaning its fields can be \"empty\" or \"unassigned\" (e.g. a hashtable), and the requested field is empty, then the value of default is returned. It is an error if the default argument is not provided in this case. If object is not of a sparse type, then providing the default argument is an error. Valid types for object are: bytevectors, hashtables, pairs, strings, vectors, non-opaque record types, SRFI-4 vectors, and SRFI-111 boxes. Only hashtables are a sparse type. Implementations are encouraged to expand this list of types with any further types they support."))
 ((group
    ((name . "ref*")
     (signature lambda (object field ...) *)
     (tags pure))
    ((name . "~")
     (signature lambda (object field ...) *)
     (tags pure)))
  (desc. "ref* (and its synonym ~) is like ref but for chained access.")))
