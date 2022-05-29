(((name . do-ec)
  (signature syntax-rules (if not and or begin nested) ((_ qualifier ... command)))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro)))
 ((name . list-ec)
  (signature syntax-rules (if not and or begin nested) ((_ qualifier ... expression) list?))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro)))
 ((name . append-ec)
  (signature syntax-rules (if not and or begin nested) ((_ qualifier ... expression) list?))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro) (expression list?)))
 ((name . string-ec)
  (signature syntax-rules (if not and or begin nested) ((_ qualifier ... expression) string?))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro) (expression char?)))
 ((name . string-append-ec)
  (signature syntax-rules (if not and or begin nested) ((_ qualifier ... expression) string?))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro) (expression string?)))
 ((name . vector-ec)
  (signature syntax-rules (if not and or begin nested) ((_ qualifier ... expression) vector?))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro)))
 ((name . vector-of-length-ec)
  (signature syntax-rules (if not and or begin nested) ((_ k qualifier ... expression) vector?))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro) (k integer?)))
 ((name . sum-ec)
  (signature syntax-rules (if not and or begin nested) ((_ qualifier ... expression) number?))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro) (expression number?)))
 ((name . product-ec)
  (signature syntax-rules (if not and or begin nested) ((_ qualifier ... expression) number?))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro) (expression number?)))
 ((name . min-ec)
  (signature syntax-rules (if not and or begin nested) ((_ qualifier ... expression) number?))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro) (expression number?)))
 ((name . max-ec)
  (signature syntax-rules (if not and or begin nested) ((_ qualifier ... expression) number?))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro) (expression number?)))
 ((name . any?-ec)
  (signature syntax-rules (if not and or begin nested) ((_ qualifier ... test) boolean?))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro)))
 ((name . every?-ec)
  (signature syntax-rules (if not and or begin nested) ((_ qualifier ... test) boolean?))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro)))
 ((name . first-ec)
  (signature syntax-rules (if not and or begin nested) ((_ default qualifier ... expression)))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro)))
 ((name . last-ec)
  (signature syntax-rules (if not and or begin nested) ((_ default qualifier ... expression)))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro)))
 ((name . fold-ec)
  (signature syntax-rules (if not and or begin nested) ((_ x0 qualifier ... expression f2)))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro) (f2 procedure?)))
 ((name . fold3-ec)
  (signature syntax-rules (if not and or begin nested) ((_ x0 qualifier ... expression f1 f2)))
  (subsigs
    (qualifier
      generator
      (if test)
      (not test)
      (and test ...)
      (or test ...)
      (begin command ... expression)
      (nested qualifier ...)))
  (syntax-param-signatures (generator generator-macro) (f1 procedure?) (f2 procedure?)))
 ((name . :)
  (signature syntax-rules (index) 
             ((_ var arg1 arg2 ...) generator-macro)
             ((_ var1 (index var2) arg1 arg2 ...) generator-macro)))
 ((name . :-dispatch-ref)
  (signature lambda () procedure?))
 ((name . :-dispatch-set!)
  (signature lambda ((procedure? d)) undefined))
 ((name . |make-initial-:-dispatch|)
  (signature lambda () procedure?))
 ((name . :list)
  (signature syntax-rules (index) 
             ((_ var arg1 arg2 ...) generator-macro)
             ((_ var1 (index var2) arg1 arg2 ...) generator-macro))
  (syntax-param-signatures
    (arg1 list?)
    (arg2 list?)))
 ((name . :string)
  (signature syntax-rules (index) 
             ((_ var arg1 arg2 ...) generator-macro)
             ((_ var1 (index var2) arg1 arg2 ...) generator-macro))
  (syntax-param-signatures
    (arg1 string?)
    (arg2 string?)))
 ((name . :vector)
  (signature syntax-rules (index) 
             ((_ var arg1 arg2 ...) generator-macro)
             ((_ var1 (index var2) arg1 arg2 ...) generator-macro))
  (syntax-param-signatures
    (arg1 vector?)
    (arg2 vector?)))
 ((name . :integers)
  (signature syntax-rules (index)
             ((_ var) generator-macro)
             ((_ var1 (index var2)) generator-macro)))
 ((name . :range)
  (signature syntax-rules (index)
             ((_ var stop) generator-macro)
             ((_ var1 (index var2) stop) generator-macro)
             ((_ var start stop) generator-macro)
             ((_ var1 (index var2) start stop) generator-macro)
             ((_ var start stop step) generator-macro)
             ((_ var1 (index var2) start stop step) generator-macro))
  (syntax-param-signatures
    (start integer?)
    (stop integer?)
    (step integer?)))
 ((name . :real-range)
  (signature syntax-rules (index)
             ((_ var stop) generator-macro)
             ((_ var1 (index var2) stop) generator-macro)
             ((_ var start stop) generator-macro)
             ((_ var1 (index var2) start stop) generator-macro)
             ((_ var start stop step) generator-macro)
             ((_ var1 (index var2) start stop step) generator-macro))
  (syntax-param-signatures
    (start real?)
    (stop real?)
    (step real?)))
 ((name . :char-range)
  (signature syntax-rules (index)
             ((_ var min max) generator-macro)
             ((_ var1 (index var2) min max) generator-macro))
  (syntax-param-signatures
    (min char?)
    (max char?)))
 ((name . :port)
  (signature syntax-rules (index)
             ((_ var port) generator-macro)
             ((_ var1 (index var2) port) generator-macro)
             ((_ var port read-proc) generator-macro)
             ((_ var1 (index var2) port read-proc) generator-macro))
  (syntax-param-signatures
    (port input-port?)
    (read-proc procedure?)))
 ((name . :dispatched)
  (signature syntax-rules (index)
             ((_ var dispatch arg1 arg2 ...) generator-macro)
             ((_ var1 (index var2) dispatch arg1 arg2 ...) generator-macro))
  (syntax-param-signatures
    (dispatch procedure?)))
 ((name . :generator-proc)
  (signature syntax-rules ()
             ((_ generator-macro) procedure?)))
 ((name . dispatch-union)
  (signature lambda ((procedure? d1) (procedure? d2)) procedure?))
 ((name . do)
  (signature syntax-rules (let)
             ((_ (lb ...) ne1? (ls ...)) generator-macro)
             ((_ (let (ob ...) oc ...) (lb ...) ne1? (let (ib ...) ic ...) ne2? (ls ...)))))
 ((name . :let)
  (signature syntax-rules (index)
             ((_ var expression) generator-macro)
             ((_ var1 (index var2) expression) generator-macro)))
 ((name . :parallel)
  (signature syntax-rules ()
             ((_ generator ...) generator-macro))
  (syntax-param-signatures
    (generator generator-macro)))
 ((name . :while)
  (signature syntax-rules ()
             ((_ generator expression) generator-macro))
  (syntax-param-signatures
    (generator generator-macro)))
 ((name . :until)
  (signature syntax-rules ()
             ((_ generator expression) generator-macro))
  (syntax-param-signatures
    (generator generator-macro))))
