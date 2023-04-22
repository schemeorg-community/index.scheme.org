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
     body)))
  (desc . "The shorthand version of define behaves as follows:
(define symbol expr)
defines symbol with the value expr

(define (symbol . args) expr ...)
defines symbol with the value (lambda args expr ...)

(define ((symbol . args1) . args) expr ...)
defines symbol with the value (lambda args1 (lambda args expr ...))

(define (((symbol . args2) . args1) . args) expr ...)
defines symbol with the value (lambda args2 (lambda args1 (lambda args expr ...)))

and so on.")))
