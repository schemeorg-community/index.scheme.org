(((name . "define-record-type")
  (signature syntax-rules () ((_ name constructor pred field ...)))
  (subsigs
   (constructor (pattern (constructor-name field-name ...)))
   (field (pattern
           (field-name accessor-name)
           (field-name accessor-name modifier-name))))))
