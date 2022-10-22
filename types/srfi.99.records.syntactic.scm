(
 ;;TODO specify new identifiers being generated / inserted into evn?
 ((name . define-record-type)
  (signature syntax-rules () ((type-spec constructor pred field ...)))
  (subsigs
    (type-spec type-name
               (type-name parent))
    (constructor #f
                 #t
                 constructor-name
                 (constructor-name field-name ...))
    (field field-name
           (field-name)
           (field-name accessor-name)
           (field-name accessor-name modifier-name)))
 (syntax-param-signatures
   (parent rtd?)))
 
)
