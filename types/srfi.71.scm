(((name . "let")
  (signature
   syntax-rules
   (values)
   ((_ (binding-spec ...) body))
   ((_ name (binding-spec ...) body)))
  (subsigs
   (binding-spec
    (pattern
     (var1 var2 ... expression)
     ((values var ...) expression)
     ((values var ... . var-rest) expression)))))
 ((name . "let*")
  (signature syntax-rules (values) ((_ (binding-spec ...) body)))
  (subsigs
   (binding-spec
    (pattern
     (var1 var2 ... expression)
     ((values var ...) expression)
     ((values var ... . var-rest) expression)))))
 ((name . "letrec")
  (signature syntax-rules (values) ((_ (binding-spec ...) body)))
  (subsigs
   (binding-spec
    (pattern
     (var1 var2 ... expression)
     ((values var ...) expression)
     ((values var ... . var-rest) expression)))))
 ((name . "uncons") (signature lambda ((pair? pair)) (values * *)) (tags pure))
 ((name . "uncons-2")
  (signature lambda ((list? lst)) (values * * *))
  (tags pure))
 ((name . "uncons-3")
  (signature lambda ((list? lst)) (values * * * *))
  (tags pure))
 ((name . "uncons-4")
  (signature lambda ((list? lst)) (values * * * * *))
  (tags pure))
 ((name . "uncons-cons")
  (signature lambda ((list? alist)) (values * * *))
  (tags pure))
 ((name . "unlist")
  (signature lambda ((list? lst)) (values * ...))
  (tags pure))
 ((name . "unvector")
  (signature lambda ((vector? vec)) (values * ...))
  (tags pure))
 ((name . "values->list") (signature syntax-rules () ((_ expression) list?)))
 ((name . "values->vector")
  (signature syntax-rules () ((_ expression) vector?))))
