(((name . syntax-rules)
  (signature
   syntax-rules
   (_)
   ((_ (literal ...) syntax-rule ...) transformer-spec)
   ((_ ellipsis (literal ...) syntax-rule ...)  transformer-spec))
  (subsigs
   (syntax-rule (pattern template))
   (pattern
    _
    identifier
    constant
    (pattern ...)
    (pattern pattern ... . pattern)
    (pattern ... pattern ellipsis pattern ...)
    (pattern ... pattern ellipsis pattern ... . pattern)
    (_append "#" (pattern ...))
    (_append "#" (pattern ... pattern ellipsis pattern ...)))
   (template
    identifier
    constant
    (element ...)
    (element element ... . template)
    (ellipsis template)
    (_append "#" (element ...)))
   (element template (_append template ellipsis)))))
