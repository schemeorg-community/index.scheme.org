(((name . "eof-object") (signature lambda () eof-object?) (tags pure) (desc . "Returns the end-of-file object. "))
 ((name . "eof-object?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is the end-of-file object, #f otherwise."))
 ((group
    ((name . "call-with-input-file")
     (signature lambda ((string? string) (procedure? proc)) *)
     (subsigs (proc (lambda ((input-port? port)) *))))
    ((name . "call-with-output-file")
     (signature lambda ((string? string) (procedure? proc)) *)
     (subsigs (proc (lambda ((output-port? port)) *)))))
  (desc . "Proc should accept one argument. These procedures open the file named by filename for input or for output, with no specified file options, and call proc with the obtained port as an argument. If proc returns, the port is closed automatically and the values returned by proc are returned. If proc does not return, the port is not closed automatically, unless it is possible to prove that the port will never again be used for an I/O operation."))
 ((name . "input-port?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if the argument is an input port (or a combined input and output port), and returns #f otherwise."))
 ((name . "output-port?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if the argument is an output port (or a combined input and output port), #f otherwise."))
 ((name . "current-error-port") (signature lambda () output-port?) (desc . "This returns a default textual port for input. Normally, this default port is associated with standard input, but can be dynamically re-assigned using the with-input-from-file procedure from the (rnrs io simple (6)) library (see section 8.3). The port may or may not have an associated transcoder; if it does, the transcoder is implementation-dependent."))
 ((group
    ((name . "current-input-port") (signature lambda () input-port?))
    ((name . "current-output-port") (signature lambda () output-port?)))
  (desc . "These return default textual ports for regular output and error output. Normally, these default ports are associated with standard output, and standard error, respectively. The return value of current-output-port can be dynamically re-assigned using the with-output-to-file procedure. A port returned by one of these procedures may or may not have an associated transcoder; if it does, the transcoder is implementation-dependent."))
 ((group
    ((name . "with-input-from-file")
     (signature lambda ((string? string) (procedure? thunk)) *)
     (subsigs (thunk (lambda () *))))
    ((name . "with-output-to-file")
     (signature lambda ((string? string) (procedure? thunk)) *)
     (subsigs (thunk (lambda () *)))))
  (desc . "Thunk must be a procedure and must accept zero arguments. The file is opened for input or output using empty file options, and thunk is called with no arguments. During the dynamic extent of the call to thunk, the obtained port is made the value returned by current-input-port or current-output-port procedures; the previous default values are reinstated when the dynamic extent is exited. When thunk returns, the port is closed automatically. The values returned by thunk are returned. If an escape procedure is used to escape back into the call to thunk after thunk is returned, the behavior is unspecified."))
 ((name . "open-input-file") (signature lambda ((string? string)) input-port?) (desc . "Opens filename for input, with empty file options, and returns the obtained port."))
 ((name . "open-output-file")
  (signature lambda ((string? string)) output-port?)
  (desc . "Opens filename for output, with empty file options, and returns the obtained port. "))
 ((group
    ((name . "close-input-port")
     (signature lambda ((input-port? input-port)) undefined))
    ((name . "close-output-port")
     (signature lambda ((output-port? output-port)) undefined)))
  (desc . "Closes input-port or output-port, respectively."))
 ((name . "read-char")
  (signature
   case-lambda
   (() (or eof-object? char?))
   (((input-port? port)) (or eof-object? char?)))
  (desc . " Reads from textual-input-port, blocking as necessary until a character is available from textual-input-port, or the data that are available cannot be the prefix of any valid encoding, or an end of file is reached.
If a complete character is available before the next end of file, read-char returns that character, and updates the input port to point past that character. If an end of file is reached before any data are read, read-char returns the end-of-file object.
If textual-input-port is omitted, it defaults to the value returned by current-input-port. "))
 ((name . "peek-char")
  (signature
   case-lambda
   (() (or eof-object? char?))
   (((input-port? port)) (or eof-object? char?)))
  (desc . "This is the same as read-char, but does not consume any data from the port."))
 ((name . "read") 
  (signature case-lambda (() *) (((input-port? port)) *))
  (desc . "Reads an external representation from textual-input-port and returns the datum it represents. The read procedure operates in the same way as get-datum, see section 8.2.9.
If textual-input-port is omitted, it defaults to the value returned by current-input-port. "))
 ((name . "write-char")
  (signature
   case-lambda
   (((char? char)) undefined)
   (((char? char) (output-port? port)) undefined))
  (desc . "Writes an encoding of the character char to the textual-output-port, and returns unspecified values.
If textual-output-port is omitted, it defaults to the value returned by current-output-port. "))
 ((name . "newline")
  (signature case-lambda (() undefined) (((output-port? port)) undefined))
  (desc . "This is equivalent to using write-char to write #\\linefeed to textual-output-port.
If textual-output-port is omitted, it defaults to the value returned by current-output-port. "))
 ((name . "display")
  (signature
   case-lambda
   ((obj) undefined)
   ((obj (output-port? port)) undefined))
  (desc . "Writes a representation of obj to the given textual-output-port. Strings that appear in the written representation are not enclosed in doublequotes, and no characters are escaped within those strings. Character objects appear in the representation as if written by write-char instead of by write. The display procedure returns unspecified values. The textual-output-port argument may be omitted, in which case it defaults to the value returned by current-output-port."))
 ((name . "write")
  (signature
   case-lambda
   ((obj) undefined)
   ((obj (output-port? port)) undefined))
  (desc . "Writes the external representation of obj to textual-output-port. The write procedure operates in the same way as put-datum; see section 8.2.12.
If textual-output-port is omitted, it defaults to the value returned by current-output-port.")))
