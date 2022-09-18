(((name . define-record-type)
  (signature syntax-rules () ((name constructor pred field ...)))
  (subsigs
   (constructor (constructor-name field-name ...))
   (field (field-name accessor-name)
          (field-name accessor-name modifier-name)))))
