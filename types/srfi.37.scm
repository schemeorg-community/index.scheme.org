(((name . "option")
  (signature
   lambda
   ((list? names)
    (boolean? required-arg?)
    (boolean? optional-arg?)
    (procedure? option-proc))
   option)
  (subsigs
   (option-proc
    (lambda ((option option) ((or char? string?) name) (string? arg) seed ...)
      (values * ...))))
  (tags pure)
  (desc . "Return an option. NAMES is a list of short (character) and long (string) option names. REQUIRED-ARG? specifies if this options requires an option-argument (boolean). OPTIONAL-ARG? specifies if this option can accept an option-argument (boolean). OPTION-PROC is a procedure (following the option-processor prototype) used to process this option."))
 ((group
    ((name . "option-names")
     (signature lambda ((option option)) list?)
     (tags pure))
    ((name . "option-required-arg?")
     (signature lambda ((option option)) boolean?)
     (tags pure))
    ((name . "option-optional-arg?")
     (signature lambda ((option option)) boolean?)
     (tags pure))
    ((name . "option-processor")
     (signature lambda ((option option)) procedure?)
     (subsigs
       (return
         (lambda ((option option) ((or char? string?) name) (string? arg) seed ...)
           (values * ...))))
     (tags pure)))
  (desc . "Return the contents of corresponding fields of OPTION."))
 ((name . "args-fold")
  (signature
   lambda
   ((list? args)
    (list? options)
    (procedure? unrecognized-option-proc)
    (procedure? operand-proc)
    seed
    ...)
   (values * ...))
  (subsigs
   (unrecognized-option-proc
    (lambda ((option option) ((or char? string?) name) (string? arg) seed ...)
      (values * ...)))
   (operand-proc (lambda ((string? operand) seed ...) (values * ...))))
  (tags pure)
  (desc . "Parse argument strings left-to-right, calling the appropriate processors in-order (for the parsed known options, unknown options, and operands), passing the seed values from one processor to the next and returning the final seeds values as results. ARGS is a list of strings. OPTIONS is a list of options. UNRECOGNIZED-OPTION-PROC is a procedure (following the option-processor prototype) for unrecognized options. NOTE: args-fold will create temporary options as necessary for the UNRECOGNIZED-OPTION-PROC. OPERAND-PROC is a procedure (following the operand-processor prototype) for operands.")))
