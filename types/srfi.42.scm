(((name . "do-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... command)))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))))
 ((name . "list-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) list?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))))
 ((name . "append-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) list?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value list?))))
 ((name . "string-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) string?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value char?))))
 ((name . "string-append-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) string?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value string?))))
 ((name . "vector-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) vector?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))))
 ((name . "vector-of-length-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ k qualifier ... expression) vector?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (k (value integer?))))
 ((name . "sum-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) number?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value number?))))
 ((name . "product-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) number?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value number?))))
 ((name . "min-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) number?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value number?))))
 ((name . "max-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... expression) number?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (expression (value number?))))
 ((name . "any?-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... test) boolean?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))))
 ((name . "every?-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ qualifier ... test) boolean?))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))))
 ((name . "first-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ default qualifier ... expression)))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))))
 ((name . "last-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ default qualifier ... expression)))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))))
 ((name . "fold-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ x0 qualifier ... expression f2)))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (f2 (value procedure?))))
 ((name . "fold3-ec")
  (signature
   syntax-rules
   (if not and or begin nested)
   ((_ x0 qualifier ... expression f1 f2)))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (generator (value generator-macro))
   (f1 (value procedure?))
   (f2 (value procedure?))))
 ((name . ":")
  (signature
   syntax-rules
   (index)
   ((_ var arg1 arg2 ...) generator-macro)
   ((_ var1 (index var2) arg1 arg2 ...) generator-macro)))
 ((name . ":-dispatch-ref") (signature lambda () procedure?))
 ((name . ":-dispatch-set!") (signature lambda ((procedure? d)) undefined))
 ((name . "make-initial-:-dispatch") (signature lambda () procedure?))
 ((name . ":list")
  (signature
   syntax-rules
   (index)
   ((_ var arg1 arg2 ...) generator-macro)
   ((_ var1 (index var2) arg1 arg2 ...) generator-macro))
  (subsigs (arg1 (value list?)) (arg2 (value list?))))
 ((name . ":string")
  (signature
   syntax-rules
   (index)
   ((_ var arg1 arg2 ...) generator-macro)
   ((_ var1 (index var2) arg1 arg2 ...) generator-macro))
  (subsigs (arg1 (value string?)) (arg2 (value string?))))
 ((name . ":vector")
  (signature
   syntax-rules
   (index)
   ((_ var arg1 arg2 ...) generator-macro)
   ((_ var1 (index var2) arg1 arg2 ...) generator-macro))
  (subsigs (arg1 (value vector?)) (arg2 (value vector?))))
 ((name . ":integers")
  (signature
   syntax-rules
   (index)
   ((_ var) generator-macro)
   ((_ var1 (index var2)) generator-macro)))
 ((name . ":range")
  (signature
   syntax-rules
   (index)
   ((_ var stop) generator-macro)
   ((_ var1 (index var2) stop) generator-macro)
   ((_ var start stop) generator-macro)
   ((_ var1 (index var2) start stop) generator-macro)
   ((_ var start stop step) generator-macro)
   ((_ var1 (index var2) start stop step) generator-macro))
  (subsigs
   (start (value integer?))
   (stop (value integer?))
   (step (value integer?))))
 ((name . ":real-range")
  (signature
   syntax-rules
   (index)
   ((_ var stop) generator-macro)
   ((_ var1 (index var2) stop) generator-macro)
   ((_ var start stop) generator-macro)
   ((_ var1 (index var2) start stop) generator-macro)
   ((_ var start stop step) generator-macro)
   ((_ var1 (index var2) start stop step) generator-macro))
  (subsigs (start (value real?)) (stop (value real?)) (step (value real?))))
 ((name . ":char-range")
  (signature
   syntax-rules
   (index)
   ((_ var min max) generator-macro)
   ((_ var1 (index var2) min max) generator-macro))
  (subsigs (min (value char?)) (max (value char?))))
 ((name . ":port")
  (signature
   syntax-rules
   (index)
   ((_ var port) generator-macro)
   ((_ var1 (index var2) port) generator-macro)
   ((_ var port read-proc) generator-macro)
   ((_ var1 (index var2) port read-proc) generator-macro))
  (subsigs (port (value input-port?)) (read-proc (value procedure?))))
 ((name . ":dispatched")
  (signature
   syntax-rules
   (index)
   ((_ var dispatch arg1 arg2 ...) generator-macro)
   ((_ var1 (index var2) dispatch arg1 arg2 ...) generator-macro))
  (subsigs (dispatch (value procedure?))))
 ((name . ":generator-proc")
  (signature syntax-rules () ((_ generator-macro) procedure?)))
 ((name . "dispatch-union")
  (signature lambda ((procedure? d1) (procedure? d2)) procedure?))
 ((name . "do")
  (signature
   syntax-rules
   (let)
   ((_ (lb ...) ne1? (ls ...)) generator-macro)
   ((_
     (let (ob ...) oc ...)
     (lb ...)
     ne1?
     (let (ib ...) ic ...)
     ne2?
     (ls ...)))))
 ((name . ":let")
  (signature
   syntax-rules
   (index)
   ((_ var expression) generator-macro)
   ((_ var1 (index var2) expression) generator-macro)))
 ((name . ":parallel")
  (signature syntax-rules () ((_ generator ...) generator-macro))
  (subsigs (generator (value generator-macro))))
 ((name . ":while")
  (signature syntax-rules () ((_ generator expression) generator-macro))
  (subsigs (generator (value generator-macro))))
 ((name . ":until")
  (signature syntax-rules () ((_ generator expression) generator-macro))
  (subsigs (generator (value generator-macro)))))
