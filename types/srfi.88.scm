(((name . keyword?)
  (signature lambda (obj) boolean?)
  (tags pure predicate))
 ((name . keyword->string)
  (signature lambda ((keyword? keyword)) string?)
  (tags pure predicate))
 ((name . string->keyword)
  (signature lambda ((string? string)) keyword?)
  (tags pure predicate)))