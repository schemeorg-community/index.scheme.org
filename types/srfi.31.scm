(((name . "rec")
  (signature
   syntax-rules
   ()
   ((_ (name variables ...) body ...) procedure?)
   ((_ name expression)))
  (desc . "(define-syntax rec
  (syntax-rules ()
    ((rec (NAME . VARIABLES) . BODY)
     (letrec ( (NAME (lambda VARIABLES . BODY)) ) NAME))
    ((rec NAME EXPRESSION)
     (letrec ( (NAME EXPRESSION) ) NAME))))")))
