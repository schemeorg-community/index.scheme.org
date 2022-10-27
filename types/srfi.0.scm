(((name . "cond-expand")
  (signature
   syntax-rules
   (library and or not else)
   ((_ ce-clause1 ce-clause2 ...)))
  (subsigs
   (ce-clause (pattern (feature-requirement expression ...) (else expression)))
   (feature-requirement
    (pattern
     feature-identifier
     (library library-name)
     (and feature-requirement ...)
     (or feature-requirement ...)
     (not feature-requirement))))))
