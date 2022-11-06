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
  (desc . "Each implementation maintains a list of feature identifiers which are present, as well as a list of libraries which can be imported. The value of a hfeature requirementi is determined by replacing each feature identifier and (library <library name>) on the implementation’s lists with #t, and all other feature identifiers and library names with #f, then evaluating the resulting expression as a Scheme boolean expression under the normal interpretation of and, or, and not. A cond-expand is then expanded by evaluating the <feature requirement>s of successive <ce-clause>s in order until one of them returns #t. When a true clause is found, the corresponding <expression>s are expanded to a begin, and the remaining clauses are ignored. If none of the <feature requirement>s evaluate to #t, then if there is an else clause, its <expression>s are included. Otherwise, the behavior of the cond-expand is unspecified. Unlike cond, cond-expand does not depend on the value of any variables. The exact features provided are implementation-defined, but for portability a core set of features is given in appendix B.")))
