(((name . "is")
  (signature
   syntax-rules
   (_)
   ((is val1 predicate-or-comparator val2 ...) (or boolean? procedure?)))
  (subsigs (val (pattern _ obj)) (predicate-or-comparator (value procedure?))))
 ((name . "isnt")
  (signature
   syntax-rules
   (_)
   ((isnt val1 predicate-or-comparator val2 ...) (or boolean? procedure?)))
  (subsigs
   (val (pattern _ obj))
   (predicate-or-comparator (value procedure?)))))
