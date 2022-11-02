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
   (parent (value rtd?)))))
