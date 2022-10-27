(((name . "define-record-type")
  (signature
   syntax-rules
   (fields
    immutable
    mutable
    parent
    protocol
    sealed
    opaque
    nongenerative
    parent-rtd)
   ((_ name-spec record-clause ...)))
  (subsigs
   (name-spec
    (pattern (record-name constructor-name predicate-name) record-name))
   (record-clause
    (pattern
     (fields field-spec ...)
     (parent parent-name)
     (protocol expression)
     (sealed #t)
     (sealed #f)
     (opaque #t)
     (opaque #f)
     (nongenerative uid)
     (nongenerative)
     (parent-rtd parentrtd parentcd)))
   (field-spec
    (pattern
     (immutable field-name accessor-name)
     (mutable field-name accessor-name mutator-name)
     (immutable field-name)
     (mutable field-name)
     field-name))))
 ((name . "record-type-descriptor")
  (signature syntax-rules () ((_ record-name))))
 ((name . "record-constructor-descriptor")
  (signature syntax-rules () ((_ record-name)))))
