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
  (tags pure)
  (desc . "Returns #t when the sequence argument is in non-decreasing order according to less? (that is, there is no adjacent pair ... x y ... for which (less? y x)).
Returns #f when the sequence contains at least one out-of-order pair. It is an error if the sequence is not a list or array (including vectors and strings)."))
 ((name . "merge")
  (signature
   case-lambda
   (((list? lst1) (list? lst2) (procedure? less?)) list?)
   (((list? lst1) (list? lst2) (procedure? less?) (procedure? key)) list?))
  (subsigs (less? (lambda (x y) boolean?)) (key (lambda (obj) *)))
  (tags pure)
  (desc . "Merges two sorted lists, returning a freshly allocated list as its result."))
 ((name . "merge!")
  (signature
   case-lambda
   (((list? lst1) (list? lst2) (procedure? less?)) list?)
   (((list? lst1) (list? lst2) (procedure? less?) (procedure? key)) list?))
  (subsigs (less? (lambda (x y) boolean?)) (key (lambda (obj) *)))
  (desc . "Merges two sorted lists, re-using the pairs of list1 and list2 to build the result. The result will be either list1 or list2."))
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
  (tags pure)
  (desc . "Accepts a list or array (including vectors and strings) for sequence; and returns a completely new sequence which is sorted according to less?. The returned sequence is the same type as the argument sequence."))
 ((name . "sort!")
  (signature
   case-lambda
   (((vector? sequence) (procedure? less?)) vector?)
   (((vector? sequence) (procedure? less?) (procedure? key)) vector?)
   (((list? sequence) (procedure? less?)) list?)
   (((list? sequence) (procedure? less?) (procedure? key)) list?)
   (((string? sequence) (procedure? less?)) string?)
   (((string? sequence) (procedure? less?) (procedure? key)) string?))
  (subsigs (less? (lambda (x y) boolean?)) (key (lambda (obj) *)))
  (desc . "Returns list, array, vector, or string sequence which has been mutated to order its elements according to less?.")))
