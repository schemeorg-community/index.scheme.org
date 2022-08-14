(((name . sorted?)
  (signature lambda ((vector? sequence) (procedure? less?)) boolean?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))))
 ((name . sorted?)
  (signature lambda ((vector? sequence) (procedure? less?) (procedure? key)) boolean?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))
    (key (lambda (obj) *))))
 ((name . sorted?)
  (signature lambda ((list? sequence) (procedure? less?)) boolean?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))))
 ((name . sorted?)
  (signature lambda ((list? sequence) (procedure? less?) (procedure? key)) boolean?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))
    (key (lambda (obj) *))))
 ((name . sorted?)
  (signature lambda ((string? sequence) (procedure? less?)) boolean?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))))
 ((name . sorted?)
  (signature lambda ((string? sequence) (procedure? less?) (procedure? key)) boolean?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))
    (key (lambda (obj) *))))
 ((name . merge)
  (signature lambda ((list? lst1) (list? lst2) (procedure? less?)) list?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))))
 ((name . merge)
  (signature lambda ((list? lst1) (list? lst2) (procedure? less?) (procedure? key)) list?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))
    (key (lambda (obj) *))))
 ((name . merge!)
  (signature lambda ((list? lst1) (list? lst2) (procedure? less?)) list?)
  (subsigs
    (less? (lambda (x y) boolean?))))
 ((name . merge!)
  (signature lambda ((list? lst1) (list? lst2) (procedure? less?) (procedure? key)) list?)
  (subsigs
    (less? (lambda (x y) boolean?))
    (key (lambda (obj) *))))

 ((name . sort)
  (signature lambda ((vector? sequence) (procedure? less?)) vector?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))))
 ((name . sort)
  (signature lambda ((vector? sequence) (procedure? less?) (procedure? key)) vector?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))
    (key (lambda (obj) *))))
 ((name . sort)
  (signature lambda ((list? sequence) (procedure? less?)) list?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))))
 ((name . sort)
  (signature lambda ((list? sequence) (procedure? less?) (procedure? key)) list?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))
    (key (lambda (obj) *))))
 ((name . sort)
  (signature lambda ((string? sequence) (procedure? less?)) string?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))))
 ((name . sort)
  (signature lambda ((string? sequence) (procedure? less?) (procedure? key)) string?)
  (tags pure)
  (subsigs
    (less? (lambda (x y) boolean?))
    (key (lambda (obj) *))))
 ((name . sort!)
  (signature lambda ((vector? sequence) (procedure? less?)) vector?)
  (subsigs
    (less? (lambda (x y) boolean?))))
 ((name . sort!)
  (signature lambda ((vector? sequence) (procedure? less?) (procedure? key)) vector?)
  (subsigs
    (less? (lambda (x y) boolean?))
    (key (lambda (obj) *))))
 ((name . sort!)
  (signature lambda ((list? sequence) (procedure? less?)) list?)
  (subsigs
    (less? (lambda (x y) boolean?))))
 ((name . sort!)
  (signature lambda ((list? sequence) (procedure? less?) (procedure? key)) list?)
  (subsigs
    (less? (lambda (x y) boolean?))
    (key (lambda (obj) *))))
 ((name . sort!)
  (signature lambda ((string? sequence) (procedure? less?)) string?)
  (subsigs
    (less? (lambda (x y) boolean?))))
 ((name . sort!)
  (signature lambda ((string? sequence) (procedure? less?) (procedure? key)) string?)
  (subsigs
    (less? (lambda (x y) boolean?))
    (key (lambda (obj) *)))))
