(((name . "write-with-shared-structure")
  (signature
   case-lambda
   ((obj) undefined)
   ((obj (output-port? port)) undefined)
   ((obj (output-port? port) optarg) undefined))
  (desc . "Writes a written representation of obj to the given port. Strings that appear in the written representation are enclosed in doublequotes, and within those strings backslash and doublequote characters are escaped by backslashes. Character objects are written using the #\\ notation.
Objects which denote locations rather than values (cons cells, vectors, and non-zero-length strings in R5RS scheme; also mutable objects, records, or containers if provided by the implementation), if they appear at more than one point in the data being written, must be preceded by \"#N=\" the first time they are written and replaced by \"#N#\" all subsequent times they are written, where N is a natural number used to identify that particular object. If objects which denote locations occur only once in the structure, then (write-with-shared-structure) must produce the same external representation for those objects as (write).
Write-with-shared-structure must terminate in finite time when writing finite data. Write-with-shared-structure must produce a finite representation when writing finite data.
Write-with-shared-structure returns an unspecified value. The port argument may be omitted, in which case it defaults to the value returned by (current-output-port). The optarg argument may also be omitted. If present, its effects on the output and return value are unspecified but (write-with-shared-structure) must still write a representation that can be read by (read-with-shared-structure). Some implementations may wish to use optarg to specify formatting conventions, numeric radixes, or return values. The reference implementation ignores optarg."))
 ((name . "read-with-shared-structure")
  (signature case-lambda
             (() *)
             ((input-port? port) *))
  (desc . "(read-with-shared-structure) converts the external representations of Scheme objects produced by (write-with-shared-structure) into scheme objects. That is, it is a parser for the nonterminal <datum> in the augmented external representation grammar defined above. (read-with-shared-structure) returns the next object parsable from the given input port, updating port to point to the first character past the end of the external representation of the object.
If an end-of-file is encountered in the input before any characters are found that can begin an object, then an end-of-file object is returned. The port remains open, and further attempts to read it (by (read-with-shared-structure) or (read) will also return an end-of-file object. If an end of file is encountered after the beginning of an object's external representation, but the external representation is incomplete and therefore not parsable, an error is signalled.
The port argument may be omitted, in which case it defaults to the value returned by (current-input-port). It is an error to read from a closed port.")))
