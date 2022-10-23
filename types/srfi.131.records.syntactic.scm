(
 ;;TODO specify new identifiers being generated / inserted into evn?
 ((name . define-record-type)
  (signature syntax-rules () ((_ type-spec constructor predicate field ...)))
  (subsigs
    (type-spec type-name
               (type-name parent))
    (constructor #f
                 constructor-name
                 (constructor-name field-name ...))
    (predicate #f
               predicate-name)
    (field field-name
           (field-name accessor-name)
           (field-name accessor-name modifier-name)))
 (syntax-param-signatures
   (parent rtd?)))
 
)
