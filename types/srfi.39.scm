(((name . "make-parameter")
  (signature
   case-lambda
   ((obj) procedure?)
   ((obj (procedure? converter)) procedure?))
  (subsigs (converter (lambda (obj) *)))
  (tags pure))
 ((name . "parameterize")
  (signature syntax-rules () ((_ ((param1 value1) ...) body)))))
