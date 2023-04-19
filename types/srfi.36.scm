(((group
    ((name . "&i/o-error") (signature value condition-type?))
    ((name . "i/o-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "(define-condition-type &i/o-error &error
  i/o-error?)

This is a supertype for a set of more specific I/O errors."))
 ((group
    ((name . "&i/o-port-error") (signature value condition-type?))
    ((name . "i/o-port-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate))
    ((name . "i/o-error-port")
     (signature lambda ((i/o-port-error? err)) port?)
     (tags pure)))
  (desc . "(define-condition-type &i/o-port-error &i/o-error
  i/o-port-error?
  (port i/o-error-port))

This condition type specifies an I/O error that occurred during an operation on a port. Condition objects belonging to this type must specify a port in the port field."))
 ((group
    ((name . "&i/o-read-error") (signature value condition-type?))
    ((name . "i/o-read-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "(define-condition-type &i/o-read-error &i/o-port-error
  i/o-read-error?)

This condition type specifies a read error that occurred during an operation on a port."))
 ((group
    ((name . "&i/o-write-error") (signature value condition-type?))
    ((name . "i/o-write-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "(define-condition-type &i/o-write-error &i/o-port-error
  i/o-write-error?)

This condition type specifies a write error that occurred during an operation on a port."))
 ((group
    ((name . "&i/o-closed-error") (signature value condition-type?))
    ((name . "i/o-closed-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "(define-condition-type &i/o-closed-error &i/o-port-error
  i/o-closed-error?)

A condition of this type specifies that an operation tried to operate on a closed port under the assumption that it is open."))
 ((group
    ((name . "&i/o-filename-error") (signature value condition-type?))
    ((name . "i/o-filename-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate))
    ((name . "i/o-error-filename")
     (signature lambda ((i/o-filename-error? err)) string?)
     (tags pure)))
  (desc . "(define-condition-type &i/o-filename-error &i/o-error
  i/o-filename-error?
  (filename i/o-error-filename))

This condition type specifies an I/O error that occurred during an operation on a named file. Condition objects belonging to this type must specify a file name in the filename field."))
 ((group
    ((name . "&i/o-malformed-filename-error") (signature value condition-type?))
    ((name . "i/o-malformed-filename-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "(define-condition-type &i/o-malformed-filename-error &i/o-filename-error
  i/o-malformed-filename-error?)

This condition type indicates that a file name had an invalid format."))
 ((group
    ((name . "&i/o-file-protection-error") (signature value condition-type?))
    ((name . "i/o-file-protection-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "(define-condition-type &i/o-file-protection-error &i/o-filename-error
  i/o-file-protection-error?)

A condition of this type specifies that an operation tried to operate on a named file with insufficient access rights."))
 ((group
    ((name . "&i/o-file-is-read-only-error") (signature value condition-type?))
    ((name . "i/o-file-is-read-only-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "(define-condition-type &i/o-file-is-read-only-error &i/o-file-protection-error
  i/o-file-is-read-only-error?)

A condition of this type specifies that an operation tried to operate on a named read-only file under the assumption that it is writeable."))
 ((group
    ((name . "&i/o-file-already-exists-error") (signature value condition-type?))
    ((name . "i/o-file-already-exists-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "(define-condition-type &i/o-file-already-exists-error &i/o-filename-error
  i/o-file-already-exists-error?)

A condition of this type specifies that an operation tried to operate on an existing named file under the assumption that it does not exist."))
 ((group
    ((name . "&i/o-no-such-file-error") (signature value condition-type?))
    ((name . "i/o-no-such-file-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "(define-condition-type &i/o-no-such-file-error &i/o-filename-error
  i/o-no-such-file-error?)

A condition of this type specifies that an operation tried to operate on an non-existent named file under the assumption that it exists."))
 ((group
    ((name . "&read-error") (signature value condition-type?))
    ((name . "read-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate))
    ((name . "read-error-line")
     (signature lambda ((read-error? err)) (or integer? #f))
     (tags pure))
    ((name . "read-error-column")
     (signature lambda ((read-error? err)) (or integer? #f))
     (tags pure))
    ((name . "read-error-position")
     (signature lambda ((read-error? err)) (or integer? #f))
     (tags pure))
    ((name . "read-error-span")
     (signature lambda ((read-error? err)) (or integer? #f))
     (tags pure)))
  (desc . "(define-condition-type &read-error &error
  read-error?
  (line read-error-line)
  (column read-error-column)
  (position read-error-position)
  (span read-error-span))

A condition of this type specifies that a parse error happened during a read operation. The fields give more information about the nature of the error. However, a Scheme implementation is not obliged to actually provide any information in any of the fields: each field value may be #f in that case.
The field values that are not #f must contain the following kinds of information:
* line is the line number of the parse error, starting from the beginning of the character sequence accessed through the port of the read operation. The first line has the number 1.
* column is the column number of the parse error. The first column has the number 0.
* position is the character position of the beginning of the parse error, starting from the beginning of the character sequence accessed through the port of the read operation. The first position is 1.
* span is the number of characters involved in the parse error.")))
