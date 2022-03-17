(

 (endianness
   (syntax-rules ()
     ((_ endianness-symbol))))

 (native-endianness
   (lambda () symbol?))

 (bytevector?
   (lambda (obj) boolean?)
   (pure predicate))

 (make-bytevector
   (lambda ((integer? k)) bytevector?)
   ())

 (make-bytevector
   (lambda ((integer? k) (integer? byte)) bytevector?)
   (pure))

 (bytevector-length
   (lambda ((bytevector? bytevector)) integer?)
   (pure))

 (bytevector=?
   (lambda ((bytevector? bytevector1) (bytevector? bytevector2)) boolean?)
   (pure))

 (bytevector-fill!
   (lambda ((bytevector? bytevector) (integer? k)) undefined)
   ())

 (bytevector-copy!
   (lambda ((bytevector? source) (integer? source-start) (bytevector? target) (integer? target-start) (integer? k)) undefined)
   ())

 (bytevector-copy
   (lambda ((bytevector? bytevector)) bytevector?)
   (pure))

 (bytevector-u8-ref
   (lambda ((bytevector? bytevector) (integer? k)) integer?)
   (pure)) 

 (bytevector-s8-ref
   (lambda ((bytevector? bytevector) (integer? k)) integer?)
   (pure)) 

 (bytevector-u8-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? octet)) undefined))

 (bytevector-s8-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? byte)) undefined))

 (bytevector->u8-list
   (lambda ((bytevector? bytevector)) list?)
   (pure))

 (u8-list->bytevector
   (lambda ((list? list)) bytevector?)
   (pure))

 (bytevector-uint-ref
   (lambda ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size)) integer?)
   (pure))

 (bytevector-sint-ref
   (lambda ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size)) integer?)
   (pure))

 (bytevector-uint-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (symbol? endianness) (integer? size)) undefined)
   ())

 (bytevector-sint-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (symbol? endianness) (integer? size)) undefined)
   ())

 (bytevector->uint-list
   (lambda ((bytevector? bytevector) (symbol? endianness) (integer? size)) list?)
   (pure))

 (bytevector->sint-list
   (lambda ((bytevector? bytevector) (symbol? endianness) (integer? size)) list?)
   (pure))

 (uint-list->bytevector
   (lambda ((list? list) (symbol? endianness) (integer? size)) bytevector?)
   (pure))

 (sint-list->bytevector
   (lambda ((list? list) (symbol? endianness) (integer? size)) bytevector?)
   (pure))

 (bytevector-u16-ref
   (lambda ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size)) integer?)
   (pure))

 (bytevector-s16-ref
   (lambda ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size)) integer?)
   (pure))

 (bytevector-u16-native-ref
   (lambda ((bytevector? bytevector) (integer? k) (integer? size)) integer?)
   (pure))

 (bytevector-s16-native-ref
   (lambda ((bytevector? bytevector) (integer? k) (integer? size)) integer?)
   (pure))

 (bytevector-u16-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (symbol? endianness) (integer? size)) undefined)
   ())

 (bytevector-s16-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (symbol? endianness) (integer? size)) undefined)
   ())

 (bytevector-u16-native-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (integer? size)) undefined)
   ())

 (bytevector-s16-native-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (integer? size)) undefined)
   ())

 (bytevector-u32-ref
   (lambda ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size)) integer?)
   (pure))

 (bytevector-s32-ref
   (lambda ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size)) integer?)
   (pure))

 (bytevector-u32-native-ref
   (lambda ((bytevector? bytevector) (integer? k) (integer? size)) integer?)
   (pure))

 (bytevector-s32-native-ref
   (lambda ((bytevector? bytevector) (integer? k) (integer? size)) integer?)
   (pure))

 (bytevector-u32-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (symbol? endianness) (integer? size)) undefined)
   ())

 (bytevector-s32-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (symbol? endianness) (integer? size)) undefined)
   ())

 (bytevector-u32-native-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (integer? size)) undefined)
   ())

 (bytevector-s32-native-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (integer? size)) undefined)
   ())

 (bytevector-u64-ref
   (lambda ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size)) integer?)
   (pure))

 (bytevector-s64-ref
   (lambda ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size)) integer?)
   (pure))

 (bytevector-u64-native-ref
   (lambda ((bytevector? bytevector) (integer? k) (integer? size)) integer?)
   (pure))

 (bytevector-s64-native-ref
   (lambda ((bytevector? bytevector) (integer? k) (integer? size)) integer?)
   (pure))

 (bytevector-u64-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (symbol? endianness) (integer? size)) undefined)
   ())

 (bytevector-s64-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (symbol? endianness) (integer? size)) undefined)
   ())

 (bytevector-u64-native-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (integer? size)) undefined)
   ())

 (bytevector-s64-native-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? n) (integer? size)) undefined)
   ())

 (bytevector-ieee-single-ref
   (lambda ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size)) integer?)
   (pure))

 (bytevector-ieee-double-ref
   (lambda ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size)) integer?)
   (pure))

 (bytevector-ieee-single-native-ref
   (lambda ((bytevector? bytevector) (integer? k) (integer? size)) integer?)
   (pure))

 (bytevector-ieee-double-native-ref
   (lambda ((bytevector? bytevector) (integer? k) (integer? size)) integer?)
   (pure))

 (bytevector-ieee-single-set!
   (lambda ((bytevector? bytevector) (integer? k) (real? x) (symbol? endianness) (integer? size)) undefined)
   ())

 (bytevector-ieee-double-set!
   (lambda ((bytevector? bytevector) (integer? k) (real? x) (symbol? endianness) (integer? size)) undefined)
   ())

 (bytevector-ieee-single-native-set!
   (lambda ((bytevector? bytevector) (integer? k) (real? x) (integer? size)) undefined)
   ())

 (bytevector-ieee-double-native-set!
   (lambda ((bytevector? bytevector) (integer? k) (real? x) (integer? size)) undefined)
   ())

 (string->utf8
   (lambda ((string? string)) bytevector?)
   (pure))

 (string->utf16
   (lambda ((string? string)) bytevector?)
   (pure))

 (string->utf16
   (lambda ((string? string) (symbol? endianness)) bytevector?)
   (pure))

 (string->utf32
   (lambda ((string? string)) bytevector?)
   (pure))

 (string->utf32
   (lambda ((string? string) (symbol? endianness)) bytevector?)
   (pure))

 (utf8->string
   (lambda ((bytevector? bytevector)) string?)
   (pure))

 (utf16->string
   (lambda ((bytevector? bytevector) (symbol? endianness)) string?)
   (pure))

 (utf16->string
   (lambda ((bytevector? bytevector) (symbol? endianness) (boolean? endianness-mandatory?)) string?)
   (pure))

 (utf32->string
   (lambda ((bytevector? bytevector) (symbol? endianness)) string?)
   (pure))

 (utf32->string
   (lambda ((bytevector? bytevector) (symbol? endianness) (boolean? endianness-mandatory?)) string?)
   (pure))

 )
