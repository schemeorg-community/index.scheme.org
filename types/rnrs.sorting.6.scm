(((name . list-sort)
  (signature lambda ((procedure? <) (list? lis)) list?)
  (tags pure)
  (subsigs (< (lambda (obj1 obj2) boolean?))))
 ((name . vector-sort)
  (signature lambda ((procedure? <) (vector? v)) boolean?)
  (tags pure)
  (subsigs (< (lambda (obj1 obj2) boolean?))))
 ((name . vector-sort!)
  (signature lambda ((procedure? <) (vector? v)) boolean?)
  (subsigs (< (lambda (obj1 obj2) boolean?)))))