(((name . "endianness") (signature syntax-rules () ((_ endianness-symbol))))
 ((name . "native-endianness") (signature lambda () symbol?))
 ((name . "bytevector?")
  (signature lambda (obj) boolean?)
  (tags pure predicate))
 ((name . "make-bytevector")
  (signature
   case-lambda
   (((integer? k)) bytevector?)
   (((integer? k) (integer? byte)) bytevector?))
  (tags pure))
 ((name . "bytevector-length")
  (signature lambda ((bytevector? bytevector)) integer?)
  (tags pure))
 ((name . "bytevector=?")
  (signature
   lambda
   ((bytevector? bytevector1) (bytevector? bytevector2))
   boolean?)
  (tags pure))
 ((name . "bytevector-fill!")
  (signature lambda ((bytevector? bytevector) (integer? k)) undefined))
 ((name . "bytevector-copy!")
  (signature
   lambda
   ((bytevector? source)
    (integer? source-start)
    (bytevector? target)
    (integer? target-start)
    (integer? k))
   undefined))
 ((name . "bytevector-copy")
  (signature lambda ((bytevector? bytevector)) bytevector?)
  (tags pure))
 ((name . "bytevector-u8-ref")
  (signature lambda ((bytevector? bytevector) (integer? k)) integer?)
  (tags pure))
 ((name . "bytevector-s8-ref")
  (signature lambda ((bytevector? bytevector) (integer? k)) integer?)
  (tags pure))
 ((name . "bytevector-u8-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? octet))
   undefined))
 ((name . "bytevector-s8-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? byte))
   undefined))
 ((name . "bytevector->u8-list")
  (signature lambda ((bytevector? bytevector)) list?)
  (tags pure))
 ((name . "u8-list->bytevector")
  (signature lambda ((list? list)) bytevector?)
  (tags pure))
 ((name . "bytevector-uint-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-sint-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-uint-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (integer? n)
    (symbol? endianness)
    (integer? size))
   undefined))
 ((name . "bytevector-sint-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (integer? n)
    (symbol? endianness)
    (integer? size))
   undefined))
 ((name . "bytevector->uint-list")
  (signature
   lambda
   ((bytevector? bytevector) (symbol? endianness) (integer? size))
   list?)
  (tags pure))
 ((name . "bytevector->sint-list")
  (signature
   lambda
   ((bytevector? bytevector) (symbol? endianness) (integer? size))
   list?)
  (tags pure))
 ((name . "uint-list->bytevector")
  (signature
   lambda
   ((list? list) (symbol? endianness) (integer? size))
   bytevector?)
  (tags pure))
 ((name . "sint-list->bytevector")
  (signature
   lambda
   ((list? list) (symbol? endianness) (integer? size))
   bytevector?)
  (tags pure))
 ((name . "bytevector-u16-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-s16-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-u16-native-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-s16-native-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-u16-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (integer? n)
    (symbol? endianness)
    (integer? size))
   undefined))
 ((name . "bytevector-s16-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (integer? n)
    (symbol? endianness)
    (integer? size))
   undefined))
 ((name . "bytevector-u16-native-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? n) (integer? size))
   undefined))
 ((name . "bytevector-s16-native-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? n) (integer? size))
   undefined))
 ((name . "bytevector-u32-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-s32-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-u32-native-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-s32-native-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-u32-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (integer? n)
    (symbol? endianness)
    (integer? size))
   undefined))
 ((name . "bytevector-s32-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (integer? n)
    (symbol? endianness)
    (integer? size))
   undefined))
 ((name . "bytevector-u32-native-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? n) (integer? size))
   undefined))
 ((name . "bytevector-s32-native-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? n) (integer? size))
   undefined))
 ((name . "bytevector-u64-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-s64-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-u64-native-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-s64-native-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? size))
   integer?)
  (tags pure))
 ((name . "bytevector-u64-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (integer? n)
    (symbol? endianness)
    (integer? size))
   undefined))
 ((name . "bytevector-s64-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (integer? n)
    (symbol? endianness)
    (integer? size))
   undefined))
 ((name . "bytevector-u64-native-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? n) (integer? size))
   undefined))
 ((name . "bytevector-s64-native-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? n) (integer? size))
   undefined))
 ((name . "bytevector-ieee-single-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   real?)
  (tags pure))
 ((name . "bytevector-ieee-double-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   real?)
  (tags pure))
 ((name . "bytevector-ieee-single-native-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? size))
   real?)
  (tags pure))
 ((name . "bytevector-ieee-double-native-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? size))
   real?)
  (tags pure))
 ((name . "bytevector-ieee-single-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (real? x)
    (symbol? endianness)
    (integer? size))
   undefined))
 ((name . "bytevector-ieee-double-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (real? x)
    (symbol? endianness)
    (integer? size))
   undefined))
 ((name . "bytevector-ieee-single-native-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (real? x) (integer? size))
   undefined))
 ((name . "bytevector-ieee-double-native-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (real? x) (integer? size))
   undefined))
 ((name . "string->utf8")
  (signature lambda ((string? string)) bytevector?)
  (tags pure))
 ((name . "string->utf16")
  (signature
   case-lambda
   (((string? string)) bytevector?)
   (((string? string) (symbol? endianness)) bytevector?))
  (tags pure))
 ((name . "string->utf32")
  (signature
   case-lambda
   (((string? string)) bytevector?)
   (((string? string) (symbol? endianness)) bytevector?))
  (tags pure))
 ((name . "utf8->string")
  (signature lambda ((bytevector? bytevector)) string?)
  (tags pure))
 ((name . "utf16->string")
  (signature
   case-lambda
   (((bytevector? bytevector) (symbol? endianness)) string?)
   (((bytevector? bytevector)
     (symbol? endianness)
     (boolean? endianness-mandatory?))
    string?))
  (tags pure))
 ((name . "utf32->string")
  (signature
   case-lambda
   (((bytevector? bytevector) (symbol? endianness)) string?)
   (((bytevector? bytevector)
     (symbol? endianness)
     (boolean? endianness-mandatory?))
    string?))
  (tags pure)))
