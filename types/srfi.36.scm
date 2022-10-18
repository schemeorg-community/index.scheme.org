(((name . &i/o-error)
  (signature value condition-type?))
 ((name . i/o-error?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes error?))
 ((name . &i/o-port-error)
  (signature value condition-type?))
 ((name . i/o-port-error?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes i/o-error?))
 ((name . i/o-error-port)
  (signature lambda ((i/o-port-error? err)) port?)
  (tags pure))
 ((name . &i/o-read-error)
  (signature value condition-type?))
 ((name . i/o-read-error?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes i/o-port-error?))
 ((name . &i/o-write-error)
  (signature value condition-type?))
 ((name . i/o-write-error?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes i/o-port-error?))
 ((name . &i/o-closed-error)
  (signature value condition-type?))
 ((name . i/o-closed-error?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes i/o-port-error?))
 ((name . &i/o-filename-error)
  (signature value condition-type?))
 ((name . i/o-filename-error?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes i/o-error?))
 ((name . i/o-error-filename)
  (signature lambda ((i/o-filename-error? err)) string?)
  (tags pure))
 ((name . &i/o-malformed-filename-error)
  (signature value condition-type?))
 ((name . i/o-malformed-filename-error?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes i/o-filename-error?))
 ((name . &i/o-file-protection-error)
  (signature value condition-type?))
 ((name . i/o-file-protection-error?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes i/o-filename-error?))
 ((name . &i/o-file-is-read-only-error)
  (signature value condition-type?))
 ((name . i/o-file-is-read-only-error?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes i/o-file-protection-error?))
 ((name . &i/o-file-already-exists-error)
  (signature value condition-type?))
 ((name . i/o-file-already-exists-error?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes i/o-filename-error?))
 ((name . &i/o-no-such-file-error)
  (signature value condition-type?))
 ((name . i/o-no-such-file-error?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes i/o-filename-error?))
 ((name . &read-error)
  (signature value condition-type?))
 ((name . read-error?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes error?))
 ((name . read-error-line)
  (signature lambda ((read-error? err)) (or integer? #f))
  (tags pure))
 ((name . read-error-column)
  (signature lambda ((read-error? err)) (or integer? #f))
  (tags pure))
 ((name . read-error-position)
  (signature lambda ((read-error? err)) (or integer? #f))
  (tags pure))
 ((name . read-error-span)
  (signature lambda ((read-error? err)) (or integer? #f))
  (tags pure)))