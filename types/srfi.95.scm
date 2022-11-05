(((name . "sorted?")
  (signature
   case-lambda
   (((vector? sequence) (procedure? less?)) boolean?)
   (((vector? sequence) (procedure? less?) (procedure? key)) boolean?)
   (((list? sequence) (procedure? less?)) boolean?)
   (((list? sequence) (procedure? less?) (procedure? key)) boolean?)
   (((string? sequence) (procedure? less?)) boolean?)
   (((string? sequence) (procedure? less?) (procedure? key)) boolean?))
  (subsigs (less? (lambda (x y) boolean?)) (key (lambda (obj) *)))
  (tags pure))
 ((name . "merge")
  (signature
   case-lambda
   (((list? lst1) (list? lst2) (procedure? less?)) list?)
   (((list? lst1) (list? lst2) (procedure? less?) (procedure? key)) list?))
  (subsigs (less? (lambda (x y) boolean?)) (key (lambda (obj) *)))
  (tags pure))
 ((name . "merge!")
  (signature
   case-lambda
   (((list? lst1) (list? lst2) (procedure? less?)) list?)
   (((list? lst1) (list? lst2) (procedure? less?) (procedure? key)) list?))
  (subsigs (less? (lambda (x y) boolean?)) (key (lambda (obj) *))))
 ((name . "sort")
  (signature
   case-lambda
   (((vector? sequence) (procedure? less?)) vector?)
   (((vector? sequence) (procedure? less?) (procedure? key)) vector?)
   (((list? sequence) (procedure? less?)) list?)
   (((list? sequence) (procedure? less?) (procedure? key)) list?)
   (((string? sequence) (procedure? less?)) string?)
   (((string? sequence) (procedure? less?) (procedure? key)) string?))
  (subsigs (less? (lambda (x y) boolean?)) (key (lambda (obj) *)))
  (tags pure))
 ((name . "sort!")
  (signature
   case-lambda
   (((vector? sequence) (procedure? less?)) vector?)
   (((vector? sequence) (procedure? less?) (procedure? key)) vector?)
   (((list? sequence) (procedure? less?)) list?)
   (((list? sequence) (procedure? less?) (procedure? key)) list?)
   (((string? sequence) (procedure? less?)) string?)
   (((string? sequence) (procedure? less?) (procedure? key)) string?))
  (subsigs (less? (lambda (x y) boolean?)) (key (lambda (obj) *)))))
