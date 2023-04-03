(((name . "make-type")
  (signature lambda (type-payload) (values procedure? procedure? procedure? procedure? procedure?))
  (desc . "(make-type type-payload) → type-accessor constructor predicate accessor make-subtype
Calling make-type on type-payload, which can be any Scheme object, returns five values, all of which are procedures. They are distinct (in the sense of eqv?) from each other and from any other procedures returned by other calls to make-type. In brief, the five functions:
    return type-payload 
    return newly allocated objects of a disjoint type known as instances, each associated with an instance payload 
    return #t iff an object is an instance of this type 
    return the instance payload 
    return five more procedures associated with a subtype of this type 

The type payload might contain metadata (such as field names or class variables) associated with the type as a whole.
For the purposes of this section, we will suppose that
(define-values (reia-metadata make-reia reia? reia-ref make-reia-subtype) (make-type 'reia))

has been evaluated, and document each of the five variables that it binds. \"Reia\" is an acronym for \"remarkably 'evil' in appearance\", and has no particular significance. Fnord!

(reia-metadata) → object
Returns the symbol reia.

(make-reia instance-payload)→ reia
Returns a newly allocated instance associated with instance-payload. This association is single and immutable, but it is possible to make use of an appropriate container payload in order to effectively associate the instance with more than one value. To make the association effectively mutable, use a mutable payload such as a box, list or vector. Instances belong to a type that is disjoint from any existing Scheme type, including types created by other calls to make-type.

(reia? object)→ boolean
Returns #t iff object was returned by a call to make-reia or any constructor created as part of a direct or indirect subtype of the reia type.

(reia-ref reia)→ object
Returns the instance payload of reia. It is an error if reia does not satisfy reia?.

(make-reia-subtype type-payload)→ type-accessor constructor predicate accessor make-subtype
Returns five new procedures with the same semantics as make-type, such that the objects returned by constructor satisfy reia? and their payload can be accessed using reia-ref.")))
