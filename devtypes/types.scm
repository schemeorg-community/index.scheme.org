(
 ((name . test-value)
  (signature value string?)
  (tags a b c))
 ((name . test-fn)
  (signature lambda ((string? a) ((or string? integer?) b) c ...) c?))
 ((name . test-fn)
  (signature lambda ((vector? v)) c?)
  (subsigs
    (v (vector foo))))
 ((name . test-fn)
  (signature lambda ((vector? v)) c?)
  (subsigs
    (v (vector (string? foo)))))
 ((name . test-fn)
  (signature lambda ((list? l)) c?)
  (subsigs
    (l (list foo))))
 ((name . test-fn)
  (signature lambda ((list? l)) c?)
  (subsigs
    (l (list (string? foo)))))
 ((name . test-fn)
  (signature lambda ((list? alist)) c?)
  (subsigs
    (alist (alist foo bar))))
 ((name . test-fn)
  (signature lambda ((list? alist) (procedure? proc)) c?)
  (subsigs
    (alist (alist (string? foo) (string? bar)))
    (proc (lambda ((string? baz)) *))))
 ((name . test-fn)
  (signature lambda ((list? alist)) c?)
  (spec-values
    (alist 
      ("wow" "asd"))))
 ((name . test-syntax)
  (signature syntax-rules (=>)
             ((_ foo => bar) string?)
             ((_ foo => bar baz)))
  (subsigs
    (foo (syntax (name val) 
                 _)))
  (subsigs
    (foo (syntax (name val) 
                 _))
    (bar (value procedure?))
    (bar (lambda () string?))))
 )
