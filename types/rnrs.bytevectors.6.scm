(((name . "endianness")
  (signature syntax-rules () ((_ endianness-symbol)))
  (desc . "The name of <endianness symbol> must be a symbol describing an endianness. An implementation must support at least the symbols big and little, but may support other endianness symbols. (endianness <endianness symbol>) evaluates to the symbol named <endianness symbol>. Whenever one of the procedures operating on bytevectors accepts an endianness as an argument, that argument must be one of these symbols. It is a syntax violation for <endianness symbol> to be anything other than an endianness symbol supported by the implementation.
Note: Implementors should use widely accepted designations for endianness symbols other than big and little. 
Note: Only the name of <endianness symbol> is significant."))
 ((name . "native-endianness")
  (signature lambda () symbol?)
  (desc . "Returns the endianness symbol associated implementation's preferred endianness (usually that of the underlying machine architecture). This may be any <endianness symbol>, including a symbol other than big and little."))
 ((name . "bytevector?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a bytevector, otherwise returns #f."))
 ((name . "make-bytevector")
  (signature
   case-lambda
   (((integer? k)) bytevector?)
   (((integer? k) (integer? byte)) bytevector?))
  (tags pure)
  (desc . "Returns a newly allocated bytevector of k bytes. If the fill argument is missing, the initial contents of the returned bytevector are unspecified. If the fill argument is present, it must be an exact integer object in the interval {−128, ... 255} that specifies the initial value for the bytes of the bytevector: If fill is positive, it is interpreted as an octet; if it is negative, it is interpreted as a byte."))
 ((name . "bytevector-length")
  (signature lambda ((bytevector? bytevector)) integer?)
  (tags pure)
  (desc . "Returns, as an exact integer object, the number of bytes in bytevector."))
 ((name . "bytevector=?")
  (signature
   lambda
   ((bytevector? bytevector1) (bytevector? bytevector2))
   boolean?)
  (tags pure)
  (desc . "Returns #t if bytevector1 and bytevector2 are equal - that is, if they have the same length and equal bytes at all valid indices. It returns #f otherwise."))
 ((name . "bytevector-fill!")
  (signature lambda ((bytevector? bytevector) (integer? k)) undefined)
  (desc . "The fill argument is as in the description of the make-bytevector procedure. The bytevector-fill! procedure stores fill in every element of bytevector and returns unspecified values. Analogous to vector-fill!."))
 ((name . "bytevector-copy!")
  (signature
   lambda
   ((bytevector? source)
    (integer? source-start)
    (bytevector? target)
    (integer? target-start)
    (integer? k))
   undefined)
  (desc . "Source and target must be bytevectors. Source-start, target-start, and k must be non-negative exact integer objects that satisfy
0 <= source-start <= source-start + k <= lsource
0 <= target-start <= target-start + k <= ltarget
where lsource is the length of source and ltarget is the length of target. The bytevector-copy! procedure copies the bytes from source at indices source-start, ..., source-start + k - 1 to consecutive indices in target starting at target-index. This must work even if the memory regions for the source and the target overlap, i.e., the bytes at the target location after the copy must be equal to the bytes at the source location before the copy. This returns unspecified values."))
 ((name . "bytevector-copy")
  (signature lambda ((bytevector? bytevector)) bytevector?)
  (tags pure)
  (desc . "Returns a newly allocated copy of bytevector."))
 ((name . "bytevector-u8-ref")
  (signature lambda ((bytevector? bytevector) (integer? k)) integer?)
  (tags pure)
  (desc . "K must be a valid index of bytevector. The bytevector-u8-ref procedure returns the byte at index k of bytevector, as an octet."))
 ((name . "bytevector-s8-ref")
  (signature lambda ((bytevector? bytevector) (integer? k)) integer?)
  (tags pure)
  (desc . "K must be a valid index of bytevector. The bytevector-s8-ref procedure returns the byte at index k of bytevector, as a (signed) byte."))
 ((name . "bytevector-u8-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? octet))
   undefined)
  (desc . "K must be a valid index of bytevector. The bytevector-u8-set! procedure stores octet in element k of bytevector."))
 ((name . "bytevector-s8-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? byte))
   undefined)
  (desc . "K must be a valid index of bytevector. The bytevector-s8-set! procedure stores the two's-complement representation of byte in element k of bytevector."))
 ((name . "bytevector->u8-list")
  (signature lambda ((bytevector? bytevector)) list?)
  (tags pure)
  (desc . "The bytevector->u8-list procedure returns a newly allocated list of the octets of bytevector in the same order."))
 ((name . "u8-list->bytevector")
  (signature lambda ((list? list)) bytevector?)
  (subsigs
    (list (list (integer? octet))))
  (tags pure)
  (desc . "List must be a list of octets. The u8-list->bytevector procedure returns a newly allocated bytevector whose elements are the elements of list list, in the same order. It is analogous to list->vector."))
 ((name . "bytevector-uint-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   integer?)
  (tags pure)
  (desc . "The bytevector-uint-ref procedure retrieves the exact integer object corresponding to the unsigned representation of size size and specified by endianness at indices k, ..., k + size − 1."))
 ((name . "bytevector-sint-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   integer?)
  (tags pure)
  (desc . "The bytevector-sint-ref procedure retrieves the exact integer object corresponding to the two's-complement representation of size size and specified by endianness at indices k, ..., k + size − 1."))
 ((name . "bytevector-uint-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (integer? n)
    (symbol? endianness)
    (integer? size))
   undefined)
  (desc . "The bytevector-uint-set! procedure stores the unsigned representation of size size and specified by endianness into bytevector at indices k, ..., k + size − 1."))
 ((name . "bytevector-sint-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (integer? n)
    (symbol? endianness)
    (integer? size))
   undefined)
  (desc . "bytevector-sint-set! stores the two's-complement representation of size size and specified by endianness into bytevector at indices k, ..., k + size − 1."))
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
  (tags pure)
  (desc . "K, ..., k + 3 must be valid indices of bytevector. The procedure returns the inexact real number object that best represents the IEEE-754 single-precision number represented by the four bytes beginning at index k. "))
 ((name . "bytevector-ieee-double-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (symbol? endianness) (integer? size))
   real?)
  (tags pure)
  (desc . "K, ..., k + 7 must be valid indices of bytevector. The procedure returns the inexact real number object that best represents the IEEE-754 double-precision number represented by the eight bytes beginning at index k."))
 ((name . "bytevector-ieee-single-native-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? size))
   real?)
  (tags pure)
  (desc . "K, ..., k + 3 must be valid indices of bytevector. K must be a multiple of 4. The procedure returns the inexact real number object that best represents the IEEE-754 single-precision number represented by the four bytes beginning at index k."))
 ((name . "bytevector-ieee-double-native-ref")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (integer? size))
   real?)
  (tags pure)
  (desc . "K, ..., k + 7 must be valid indices of bytevector. K must be a multiple of 8. The procedure returns the inexact real number object that best represents the IEEE-754 double-precision number represented by the eight bytes beginning at index k."))
 ((name . "bytevector-ieee-single-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (real? x)
    (symbol? endianness)
    (integer? size))
   undefined)
  (desc . "K, ..., k + 3 must be valid indices of bytevector. The procedure stores an IEEE-754 single-precision representation of x into elements k through k + 3 of bytevector, and returns unspecified values."))
 ((name . "bytevector-ieee-double-set!")
  (signature
   lambda
   ((bytevector? bytevector)
    (integer? k)
    (real? x)
    (symbol? endianness)
    (integer? size))
   undefined)
  (desc . "K, ..., k + 7 must be valid indices of bytevector. The procedure stores an IEEE-754 double-precision representation of x into elements k through k + 7 of bytevector, and returns unspecified values."))
 ((name . "bytevector-ieee-single-native-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (real? x) (integer? size))
   undefined)
  (desc . "K, ..., k + 3 must be valid indices of bytevector. K must be a multiple of 4. The procedure stores an IEEE-754 single-precision representation of x into elements k through k + 3 of bytevector, and returns unspecified values."))
 ((name . "bytevector-ieee-double-native-set!")
  (signature
   lambda
   ((bytevector? bytevector) (integer? k) (real? x) (integer? size))
   undefined)
  (desc . "K, ..., k + 7 must be valid indices of bytevector. K must be a multiple of 8. The procedure stores an IEEE-754 double-precision representation of x into elements k through k + 7 of bytevector, and returns unspecified values."))
 ((name . "string->utf8")
  (signature lambda ((string? string)) bytevector?)
  (tags pure)
  (desc . "Returns a newly allocated (unless empty) bytevector that contains the UTF-8 encoding of the given string."))
 ((name . "string->utf16")
  (signature
   case-lambda
   (((string? string)) bytevector?)
   (((string? string) (symbol? endianness)) bytevector?))
  (tags pure)
  (desc . "If endianness is specified, it must be the symbol big or the symbol little. The string->utf16 procedure returns a newly allocated (unless empty) bytevector that contains the UTF-16BE or UTF-16LE encoding of the given string (with no byte-order mark). If endianness is not specified or is big, then UTF-16BE is used. If endianness is little, then UTF-16LE is used."))
 ((name . "string->utf32")
  (signature
   case-lambda
   (((string? string)) bytevector?)
   (((string? string) (symbol? endianness)) bytevector?))
  (tags pure)
  (desc . "If endianness is specified, it must be the symbol big or the symbol little. The string->utf32 procedure returns a newly allocated (unless empty) bytevector that contains the UTF-32BE or UTF-32LE encoding of the given string (with no byte mark). If endianness is not specified or is big, then UTF-32BE is used. If endianness is little, then UTF-32LE is used."))
 ((name . "utf8->string")
  (signature lambda ((bytevector? bytevector)) string?)
  (tags pure)
  (desc . "Returns a newly allocated (unless empty) string whose character sequence is encoded by the given bytevector."))
 ((name . "utf16->string")
  (signature
   case-lambda
   (((bytevector? bytevector) (symbol? endianness)) string?)
   (((bytevector? bytevector)
     (symbol? endianness)
     (boolean? endianness-mandatory?))
    string?))
  (tags pure)
  (desc . "Endianness must be the symbol big or the symbol little. The utf16->string procedure returns a newly allocated (unless empty) string whose character sequence is encoded by the given bytevector. Bytevector is decoded according to UTF-16BE or UTF-16LE: If endianness-mandatory? is absent or #f, utf16->string determines the endianness according to a UTF-16 BOM at the beginning of bytevector if a BOM is present; in this case, the BOM is not decoded as a character. Also in this case, if no UTF-16 BOM is present, endianness specifies the endianness of the encoding. If endianness-mandatory? is a true value, endianness specifies the endianness of the encoding, and any UTF-16 BOM in the encoding is decoded as a regular character.
Note: A UTF-16 BOM is either a sequence of bytes #xFE, #xFF specifying big and UTF-16BE, or #xFF, #xFE specifying little and UTF-16LE."))
 ((name . "utf32->string")
  (signature
   case-lambda
   (((bytevector? bytevector) (symbol? endianness)) string?)
   (((bytevector? bytevector)
     (symbol? endianness)
     (boolean? endianness-mandatory?))
    string?))
  (tags pure)
  (desc . " Endianness must be the symbol big or the symbol little. The utf32->string procedure returns a newly allocated (unless empty) string whose character sequence is encoded by the given bytevector. Bytevector is decoded according to UTF-32BE or UTF-32LE: If endianness-mandatory? is absent or #f, utf32->string determines the endianness according to a UTF-32 BOM at the beginning of bytevector if a BOM is present; in this case, the BOM is not decoded as a character. Also in this case, if no UTF-32 BOM is present, endianness specifies the endianness of the encoding. If endianness-mandatory? is a true value, endianness specifies the endianness of the encoding, and any UTF-32 BOM in the encoding is decoded as a regular character.
Note: A UTF-32 BOM is either a sequence of bytes #x00, #x00, #xFE, #xFF specifying big and UTF-32BE, or #xFF, #xFE, #x00, #x00, specifying little and UTF-32LE.")))
