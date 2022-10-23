(((name . is)
  (signature syntax-rules (_)
             ((is val1 predicate-or-comparator val2 ...) (or boolean? procedure?)))
  (subsigs
    (val _
         obj))
  (syntax-param-signatures
    (predicate-or-comparator procedure?)))
 ((name . isnt)
  (signature syntax-rules (_)
             ((isnt val1 predicate-or-comparator val2 ...) (or boolean? procedure?)))
  (subsigs
    (val _
         obj))
  (syntax-param-signatures
    (predicate-or-comparator procedure?))))
