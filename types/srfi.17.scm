(((name . "set!")
  (signature
   syntax-rules
   ()
   ((_ variable expression))
   ((_ (proc args ...) expression))))
 ((name . "setter") (signature lambda ((procedure? proc)) procedure?)))
