(((name . "define")
  (signature
   syntax-rules
   ()
   ((_ variable expression))
   ((_ (variable parameter1 ...) body))
   ((_ (variable parameter1 ... . parameter) body))
   ((_ ((variable inner-param1 ...) outter-param1 ...) body))
   ((_ ((variable inner-param1 ...) outter-param1 ... . outter-rest) body))
   ((_ ((variable inner-param1 ... . inner-rest) outter-param1 ...) body))
   ((_
     ((variable inner-param1 ... . inner-rest) outter-param1 ... . outter-rest)
     body)))))
