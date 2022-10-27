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
  (tags pure))
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
  (tags pure))
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
  (tags pure)))
