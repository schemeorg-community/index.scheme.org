(((group
    ((name . "&i/o") (signature value record-type-descriptor?))
    ((name . "make-i/o-error") (signature lambda () i/o-error?) (tags pure))
    ((name . "i/o-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . " This condition type could be defined by
(define-condition-type &i/o &error
                       make-i/o-error i/o-error?)

This is a supertype for a set of more specific I/O errors."))
 ((group
    ((name . "&i/o-read") (signature value record-type-descriptor?))
    ((name . "make-i/o-read-error")
     (signature lambda () i/o-read-error?)
     (tags pure))
    ((name . "i/o-read-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &i/o-read &i/o
                       make-i/o-read-error i/o-read-error?)

This condition type describes read errors that occurred during an I/O operation."))
 ((group
    ((name . "&i/o-write") (signature value record-type-descriptor?))
    ((name . "make-i/o-write-error")
     (signature lambda () i/o-write-error?)
     (tags pure))
    ((name . "i/o-write-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &i/o-write &i/o
                       make-i/o-write-error i/o-write-error?)

This condition type describes write errors that occurred during an I/O operation."))
 ((group
    ((name . "&i/o-invalid-position") (signature value record-type-descriptor?))
    ((name . "make-i/o-invalid-position-error")
     (signature lambda (position) i/o-invalid-position-error?)
     (tags pure))
    ((name . "i/o-invalid-position-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)
     )
    ((name . "i/o-error-position")
     (signature lambda ((i/o-invalid-position-error? condition)) *)
     (tags pure)))
  (desc . "This condition type could be defined by
(define-condition-type &i/o-invalid-position &i/o
                       make-i/o-invalid-position-error
                       i/o-invalid-position-error?
                       (position i/o-error-position))

This condition type describes attempts to set the file position to an invalid position. Position should be the file position that the program intended to set. This condition describes a range error, but not an assertion violation."))
 ((group
    ((name . "&i/o-filename") (signature value record-type-descriptor?))
    ((name . "make-i/o-filename-error")
     (signature lambda (filename) i/o-filename-error?)
     (tags pure))
    ((name . "i/o-filename-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)
     )
    ((name . "i/o-error-filename")
     (signature lambda ((i/o-filename-error? condition)) *)
     (tags pure)))
  (desc . "This condition type could be defined by
(define-condition-type &i/o-filename &i/o
                       make-i/o-filename-error i/o-filename-error?
                       (filename i/o-error-filename))

This condition type describes an I/O error that occurred during an operation on a named file. Filename should be the name of the file."))
 ((group
    ((name . "&i/o-file-protection") (signature value record-type-descriptor?))
    ((name . "make-i/o-file-protection-error")
     (signature lambda (file) i/o-file-protection-error?)
     (tags pure))
    ((name . "i/o-file-protection-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &i/o-file-protection
                       &i/o-filename
                       make-i/o-file-protection-error
                       i/o-file-protection-error?)

A condition of this type specifies that an operation tried to operate on a named file with insufficient access rights."))
 ((group
    ((name . "&i/o-file-is-read-only") (signature value record-type-descriptor?))
    ((name . "make-i/o-file-is-read-only-error")
     (signature lambda (file) i/o-file-is-read-only-error?)
     (tags pure))
    ((name . "i/o-file-is-read-only-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &i/o-file-is-read-only
                       &i/o-file-protection
                       make-i/o-file-is-read-only-error
                       i/o-file-is-read-only-error?)

A condition of this type specifies that an operation tried to operate on a named read-only file under the assumption that it is writeable."))
 ((group
    ((name . "&i/o-file-already-exists")
     (signature value record-type-descriptor?))
    ((name . "make-i/o-file-already-exists-error")
     (signature lambda (file) i/o-file-already-exists-error?)
     (tags pure))
    ((name . "i/o-file-already-exists-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &i/o-file-already-exists
                       &i/o-filename
                       make-i/o-file-already-exists-error
                       i/o-file-already-exists-error?)

A condition of this type specifies that an operation tried to operate on an existing named file under the assumption that it did not exist."))
 ((group
    ((name . "&i/o-file-does-not-exist")
     (signature value record-type-descriptor?))
    ((name . "make-i/o-file-does-not-exist-error")
     (signature lambda (file) i/o-file-does-not-exist-error?)
     (tags pure))
    ((name . "i/o-file-does-not-exist-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &i/o-file-does-not-exist
                       &i/o-filename
                       make-i/o-file-does-not-exist-error
                       i/o-file-does-not-exist-error?)

A condition of this type specifies that an operation tried to operate on an non-existent named file under the assumption that it existed."))
 ((group
    ((name . "&i/o-port") (signature value record-type-descriptor?))
    ((name . "make-i/o-port-error")
     (signature lambda ((port? port)) i/o-port-error?)
     (tags pure))
    ((name . "i/o-port-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate))
    ((name . "i/o-error-port")
     (signature lambda ((i/o-port-error? condition)) port?)
     (tags pure)))
  (desc . "This condition type could be defined by
(define-condition-type &i/o-port &i/o
                       make-i/o-port-error i/o-port-error?
                       (port i/o-error-port))

This condition type specifies the port with which an I/O error is associated. Port should be the port. Conditions raised by procedures accepting a port as an argument should include an &i/o-port-error condition.")))
