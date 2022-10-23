(((name . define-record-type)
  (signature syntax-rules () ((_ name constructor pred field ...)))
  (subsigs
   (constructor (constructor-name field-name ...))
   (field (field-name accessor-name)
          (field-name accessor-name modifier-name)))))
