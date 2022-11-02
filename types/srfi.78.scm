(((name . "check")
  (signature
   syntax-rules
   (=>)
   ((_ expr (=> equal) expected))
   ((_ expr => expected)))
  (subsigs (equal (value procedure?))))
 ((name . "check-ec")
  (signature
   syntax-rules
   (=>)
   ((_ qualifier ... expr (=> equal) expected (argument ...)))
   ((_ qualifier ... expr => expected (argument ...)))
   ((_ qualifier ... expr (=> equal) expected))
   ((_ qualifier ... expr => expected)))
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
   (equal (value procedure?))
   (generator (value generator-macro))))
 ((name . "check-report") (signature lambda () undefined))
 ((name . "check-reset!") (signature lambda () undefined))
 ((name . "check-passed?")
  (signature lambda ((integer? expected-total-count)) boolean?)))
