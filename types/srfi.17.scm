(((name . "set!")
  (signature
   syntax-rules
   ()
   ((_ variable expression))
   ((_ (proc args ...) expression)))
  (desc . "The special form set! is extended so the first operand can be a procedure application, and not just a variable. The procedure is typically one that extracts a component from some data structure. Informally, when the procedure is called in the first operand of set!, it causes the corresponding component to be replaced by the second operand. For example, (set! (vector-ref x i) v) would be equivalent to (vector-set! x i v).
Each procedure that may be used as the first operand to set! must have a corresponding \"setter\" procedure. The builtin procedure setter takes a procedure and returns the corresponding setter procedure.

The following standard procedures have pre-defined setters:
(set! (car x) v) == (set-car! x v)
(set! (cdr x) v) == (set-cdr! x v)
(set! (caar x) v) == (set-car! (car x) v)
(set! (cadr x) v) == (set-car! (cdr x) v)
....
(set! (caXXr x) v) == (set-car! (cXXr x) v)
(set! (cdXXr x) v) == (set-cdr! (cXXr x) v)
(set! (string-ref x i) v) == (string-set! x i v)
(set! (vector-ref x i) v) == (vector-set! x i v)
        "))
 ((name . "setter") 
  (signature lambda ((procedure? proc)) procedure?)
  (desc . "Returns an associated setter procedure.  We define:
(set! (proc arg ...) value)
as:
((setter proc) arg ... value)

This SRFI specifies the special case for the setter property. This is defined such that:
(set! (setter proc) setter)
sets the setter procedure associated with proc to setter. For example, we can assume
(set! (setter car) set-car!)
has been executed by the Scheme prologue."))
 ((name . "getter-with-setter")
  (signature lambda ((procedure? getter) (procedure? setter)) procedure?)
  (desc . "This evaluates to a new anonymous procedure which when applied invokes getter, and whose setter is setter. It is an error for a program to subsequently try to modify the setter of the resulting compound.
For example, we could define:
(define car (getter-with-setter %primitive-car %primitive-set-car!))
(define set-car! %primitive-set-car!)")))
