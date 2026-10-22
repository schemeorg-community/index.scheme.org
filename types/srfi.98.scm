(((name . "getenv")
  (signature case-lambda
             (((string? name)) (or string? #f))
             (() list?))
  (tags pure)
  (desc . "Returns the value of the named environment variable as a string, or #f if the named environment variable is not found.
In the second form, returns names and values of all the environment variables as an alist.")))
