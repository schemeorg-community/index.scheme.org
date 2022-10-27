(((name . "let")
  (signature
   syntax-rules
   ()
   ((_ ((var1 init1) ...) body))
   ((_ name ((var1 init1) ...) body))
   ((_ name ((var1 init1) ... var-rest rest-init ...) body))
   ((_ (name (var1 init1) ...) body))
   ((_ (name (var1 init1) ... var-rest rest-init ...) body)))))
