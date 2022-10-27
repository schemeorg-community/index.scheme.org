(((name . "endianness")
  (signature
   syntax-rules
   (big little native)
   ((_ big) endianness)
   ((_ little) endianness)
   ((_ native) endianness)))
 ((name . "blob?") (signature lambda (obj) boolean?) (tags pure predicate))
 ((name . "make-blob")
  (signature lambda ((integer? octet-count)) blob?)
  (tags pure))
 ((name . "blob-length")
  (signature lambda ((blob? blob)) integer?)
  (tags pure))
 ((name . "blob-u8-ref")
  (signature lambda ((blob? blob) (integer? k)) integer?)
  (tags pure))
 ((name . "blob-s8-ref")
  (signature lambda ((blob? blob) (integer? k)) integer?)
  (tags pure))
 ((name . "blob-u8-set!")
  (signature lambda ((blob? blob) (integer? k) (integer? octet)) undefined))
 ((name . "blob-s8-set!")
  (signature lambda ((blob? blob) (integer? k) (integer? byte)) undefined))
 ((name . "blob-uint-ref")
  (signature
   lambda
   ((integer? size) (endianness endianness) (blob? blob) (integer? k))
   integer?)
  (tags pure))
 ((name . "blob-sint-ref")
  (signature
   lambda
   ((integer? size) (endianness endianness) (blob? blob) (integer? k))
   integer?)
  (tags pure))
 ((name . "blob-uint-set!")
  (signature
   lambda
   ((integer? size)
    (endianness endianness)
    (blob? blob)
    (integer? k)
    (integer? value))
   undefined))
 ((name . "blob-sint-set!")
  (signature
   lambda
   ((integer? size)
    (endianness endianness)
    (blob? blob)
    (integer? k)
    (integer? value))
   undefined))
 ((name . "blob-u32-ref")
  (signature
   lambda
   ((endianness endianness) (blob? blob) (integer? k))
   integer?)
  (tags pure))
 ((name . "blob-s32-ref")
  (signature
   lambda
   ((endianness endianness) (blob? blob) (integer? k))
   integer?)
  (tags pure))
 ((name . "blob-u32-native-ref")
  (signature lambda ((blob? blob) (integer? k)) integer?)
  (tags pure))
 ((name . "blob-s32-native-ref")
  (signature lambda ((blob? blob) (integer? k)) integer?)
  (tags pure))
 ((name . "blob-u32-set!")
  (signature
   lambda
   ((endianness endianness) (blob? blob) (integer? k) (integer? value))
   undefined))
 ((name . "blob-s32-set!")
  (signature
   lambda
   ((endianness endianness) (blob? blob) (integer? k) (integer? value))
   undefined))
 ((name . "blob-u32-native-set!")
  (signature lambda ((blob? blob) (integer? k) (integer? value)) undefined))
 ((name . "blob-s32-native-set!")
  (signature lambda ((blob? blob) (integer? k) (integer? value)) undefined))
 ((name . "blob-u64-ref")
  (signature
   lambda
   ((endianness endianness) (blob? blob) (integer? k))
   integer?)
  (tags pure))
 ((name . "blob-s64-ref")
  (signature
   lambda
   ((endianness endianness) (blob? blob) (integer? k))
   integer?)
  (tags pure))
 ((name . "blob-u64-native-ref")
  (signature lambda ((blob? blob) (integer? k)) integer?)
  (tags pure))
 ((name . "blob-s64-native-ref")
  (signature lambda ((blob? blob) (integer? k)) integer?)
  (tags pure))
 ((name . "blob-u64-set!")
  (signature
   lambda
   ((endianness endianness) (blob? blob) (integer? k) (integer? value))
   undefined))
 ((name . "blob-s64-set!")
  (signature
   lambda
   ((endianness endianness) (blob? blob) (integer? k) (integer? value))
   undefined))
 ((name . "blob-u64-native-set!")
  (signature lambda ((blob? blob) (integer? k) (integer? value)) undefined))
 ((name . "blob-s64-native-set!")
  (signature lambda ((blob? blob) (integer? k) (integer? value)) undefined))
 ((name . "blob=?")
  (signature lambda ((blob? blob1) (blob? blob2)) boolean?)
  (tags pure))
 ((name . "blob-copy!")
  (signature
   lambda
   ((blob? source)
    (integer? source-start)
    (blob? target)
    (integer? target-start)
    (integer? n))
   boolean?)
  (tags pure))
 ((name . "blob-copy") (signature lambda ((blob? blob)) blob?) (tags pure))
 ((name . "blob->u8-list") (signature lambda ((blob? blob)) list?) (tags pure))
 ((name . "u8-list->blob")
  (signature lambda ((list? octets)) blob?)
  (tags pure))
 ((name . "blob->uint-list")
  (signature
   lambda
   ((integer? size) (endianness endianness) (blob? blob))
   list?)
  (tags pure))
 ((name . "blob->sint-list")
  (signature
   lambda
   ((integer? size) (endianness endianness) (blob? blob))
   list?)
  (tags pure))
 ((name . "uint-list->blob")
  (signature
   lambda
   ((integer? size) (endianness endianness) (list? list))
   blob?)
  (tags pure))
 ((name . "sint-list->blob")
  (signature
   lambda
   ((integer? size) (endianness endianness) (list? list))
   blob?)
  (tags pure)))
