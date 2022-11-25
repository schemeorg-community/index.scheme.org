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
     (not feature-requirement))))
  (desc . "The cond-expand form tests for the existence of features at macro-expansion time. It either expands into the body of one of its clauses or signals an error during syntactic processing. cond-expand expands into the body of the first clause whose feature requirement is currently satisfied (the else clause, if present, is selected if none of the previous clauses is selected). A feature requirement has an obvious interpretation as a logical formula, where the <feature identifier> variables have meaning TRUE if the feature corresponding to the feature identifier, as specified in the SRFI registry, is in effect at the location of the cond-expand form, and FALSE otherwise. A feature requirement is satisfied if its formula is true under this interpretation. ")))
