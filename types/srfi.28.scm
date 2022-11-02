(((name . "format")
  (signature lambda ((string? format-string) obj ...) string?)
  (tags pure)
  (spec-values
   (format-string
    ("~a"
     "The corresponding value is inserted into the string as if printed with display.")
    ("~s"
     "The corresponding value is inserted into the string as if printed with write.")
    ("~%A" " A newline is inserted.")
    ("~~" "A tilde '~' is inserted.")))))
