(
 ;;TODO specify new identifiers being generated / inserted into environment?
 ((name . define-record-type)
  (signature syntax-rules () ((_ type-spec constructor predicate field ...)))
  (subsigs
    (type-spec type-name
               (type-name parent))
    (constructor #f
                 #t
                 constructor-name
                 (constructor-name field-name ...))
    (predicate #f
               #t
               predicate-name)
    (field field-name
           (field-name)
           (field-name accessor-name)
           (field-name accessor-name modifier-name)))
 (syntax-param-signatures
   (parent rtd?)))
 
)
