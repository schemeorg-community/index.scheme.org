(((name . "cut")
  (signature
   syntax-rules
   (<> <...>)
   ((_ slot-or-expr slot-or-expr ...) procedure?)
   ((_ slot-or-expr slot-or-expr ... <...>) procedure?))
  (subsigs (slot-or-expr (pattern <> expression))))
 ((name . "cute")
  (signature
   syntax-rules
   (<> <...>)
   ((_ slot-or-expr slot-or-expr ...) procedure?)
   ((_ slot-or-expr slot-or-expr ... <...>) procedure?))
  (subsigs (slot-or-expr (pattern <> expression)))))
