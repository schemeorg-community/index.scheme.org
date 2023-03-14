(((name . "assume") 
  (signature syntax-rules () ((_ obj message) *))
  (desc . "This special form is an expression that evaluates to the value of obj if obj evaluates to a true value. It is an error if obj evaluates to a false value. In this case, implementations are encouraged to report this error together with the messages to the user, at least when the implementation is in debug or non-optimizing mode. In case of reporting the error, an implementation is also encouraged to report the source location of the source of the error.")))
