(((name . "syntax-rules")
  (signature
   syntax-rules
   (_)
   ((_ (literal ...) syntax-rule ...) transformer-spec)
   ((_ ellipsis (literal ...) syntax-rule ...) transformer-spec))
  (subsigs
   (syntax-rule (pattern (pattern template)))
   (pattern
    (pattern
     _
     identifier
     constant
     (pattern ...)
     (pattern pattern ... . pattern)
     (pattern ... pattern ellipsis pattern ...)
     (pattern ... pattern ellipsis pattern ... . pattern)
     (_append "#" (pattern ...))
     (_append "#" (pattern ... pattern ellipsis pattern ...))))
   (template
    (pattern
     identifier
     constant
     (element ...)
     (element element ... . template)
     (ellipsis template)
     (_append "#" (element ...))))
   (element (pattern template (_append template ellipsis))))))
