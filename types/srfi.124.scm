(((name . ephemeron?)
  (signature lambda (object) boolean?)
  (tags pure predicate))
 ((name . make-ephemeron) (signature lambda (key datum) ephemeron?))
 ((name . ephemeron-broken?)
  (signature lambda ((ephemeron? ephemeron)) boolean?))
 ((name . ephemeron-key) (signature lambda ((ephemeron? ephemeron)) *))
 ((name . ephemeron-datum) (signature lambda ((ephemeron? ephemeron)) *))
 ((name . reference-barrier) (signature lambda (key) *)))