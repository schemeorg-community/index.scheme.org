(
 (char-alphabetic?
   (lambda ((char? char)) boolean?)
   (pure))

 (char-ci<=?
   (lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
   (pure))

 (char-ci<?
   (lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
   (pure))

 (char-ci=?
   (lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
   (pure))

 (char-ci>=?
   (lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
   (pure))

 (char-ci>?
   (lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
   (pure))

 (char-downcase
   (lambda ((char? char)) char?)
   (pure))

 (char-foldcase
   (lambda ((char? char)) char?)
   (pure))

 (char-lower-case?
   (lambda ((char? char)) boolean?)
   (pure))

 (char-numeric?
   (lambda ((char? char)) boolean?)
   (pure))

 (char-upcase
   (lambda ((char? char)) char?)
   (pure))

 (char-upper-case?
   (lambda ((char? char)) boolean?)
   (pure))
 (char-whitespace?
   (lambda ((char? char)) boolean?)
   (pure))

 (digit-value
   (lambda ((char? char)) (or boolean? integer?))
   (pure))

 (string-ci<=?
   (lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
   (pure))

 (string-ci<?
   (lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
   (pure))

 (string-ci=?
   (lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
   (pure))

 (string-ci>=?
   (lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
   (pure))

 (string-ci>?
   (lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
   (pure))

 (string-downcase
   (lambda ((string? string)) string?)
   (pure))

 (string-foldcase
   (lambda ((string? string)) string?)
   (pure))

 (string-upcase
   (lambda ((string? string)) string?)
   (pure))
 )
