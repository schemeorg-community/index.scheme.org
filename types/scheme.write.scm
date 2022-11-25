(((name . "display")
  (signature
   case-lambda
   ((obj) undefined)
   ((obj (output-port? port)) undefined))
  (desc . "Writes a representation of obj to the given textual output port. Strings that appear in the written representation are output as if by write-string instead of by write. Symbols are not escaped. Character objects appear in the representation as if written by write-char instead of by write. The display representation of other objects is unspecified. However, display must not loop forever on self-referencing pairs, vectors, or records. Thus if the normal write representation is used, datum labels are needed to represent cycles as in write. Implementations may support extended syntax to represent record types or other types that do not have datum representations. The display procedure returns an unspecified value. Rationale: The write procedure is intended for producing machine-readable output and display for producing human-readable output."))
 ((name . "write")
  (signature
   case-lambda
   ((obj) undefined)
   ((obj (output-port? port)) undefined))
  (desc . "Writes a representation of obj to the given textual output port. Strings that appear in the written representation are enclosed in quotation marks, and within those strings backslash and quotation mark characters are escaped by backslashes. Symbols that contain non-ASCII characters are escaped with vertical lines. Character objects are written using the #\\ notation. If obj contains cycles which would cause an infinite loop using the normal written representation, then at least the objects that form part of the cycle must be represented using datum labels as described in section 2.4. Datum labels must not be used if there are no cycles. Implementations may support extended syntax to represent record types or other types that do not have datum representations."))
 ((name . "write-shared")
  (signature
   case-lambda
   ((obj) undefined)
   ((obj (output-port? port)) undefined))
  (desc . "The write-shared procedure is the same as write, except that shared structure must be represented using datum labels for all pairs and vectors that appear more than once in the output."))
 ((name . "write-simple")
  (signature
   case-lambda
   ((obj) undefined)
   ((obj (output-port? port)) undefined))
  (desc . "The write-simple procedure is the same as write, except that shared structure is never represented using datum labels. This can cause write-simple not to terminate if obj contains circular structure.")))
