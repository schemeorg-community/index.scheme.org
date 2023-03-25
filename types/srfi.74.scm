(((name . "endianness")
  (signature
   syntax-rules
   (big little native)
   ((_ big) endianness)
   ((_ little) endianness)
   ((_ native) endianness))
  (desc . "(endianness big) and (endianness little) evaluate to two distinct and unique objects representing an endianness. The native endianness evaluates to the endianness of the underlying machine architecture, and must be eq? to either (endianness big) or (endianness little). "))
 ((name . "blob?") (signature lambda (obj) boolean?) (tags pure predicate) (desc . "Returns #t if obj is a blob, otherwise returns #f."))
 ((name . "make-blob")
  (signature lambda ((integer? octet-count)) blob?)
  (tags pure)
  (desc . "Returns a newly allocated blob of k octets, all of them 0."))
 ((name . "blob-length")
  (signature lambda ((blob? blob)) integer?)
  (tags pure)
  (desc . "Returns the number of octets in blob as an exact integer."))
 ((name . "blob-u8-ref")
  (signature lambda ((blob? blob) (integer? k)) integer?)
  (tags pure)
  (desc . "Blob-u8-ref returns the octet at index k of blob."))
 ((name . "blob-s8-ref")
  (signature lambda ((blob? blob) (integer? k)) integer?)
  (tags pure)
  (desc . "Blob-s8-ref returns the exact integer corresponding to the two's complement representation at index k of blob. "))
 ((name . "blob-u8-set!")
  (signature lambda ((blob? blob) (integer? k) (integer? octet)) undefined)
  (desc . "Blob-u8-set! stores octet in element k of blob."))
 ((name . "blob-s8-set!")
  (signature lambda ((blob? blob) (integer? k) (integer? byte)) undefined)
  (desc . "Blob-u8-set! stores the two's complement representation of byte in element k of blob."))
 ((group
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
       undefined)))
  (desc . "Size must be a positive exact integer. K must be a valid index of blob; so must the indices {k, ..., k + size - 1}. Endianness must be an endianness object.
Blob-uint-ref retrieves the exact integer corresponding to the unsigned representation of size size and specified by endianness at indices {k, ..., k + size - 1}.
Blob-sint-ref retrieves the exact integer corresponding to the two's complement representation of size size and specified by endianness at indices {k, ..., k + size - 1}.
For blob-uint-set!, n must be an exact integer in the interval [0, (256^size)-1]. Blob-uint-set! stores the unsigned representation of size size and specified by endianness into the blob at indices {k, ..., k + size - 1}.
For blob-uint-set!, n must be an exact integer in the interval [-256^(size-1), (256^(size-1))-1]. Blob-sint-set! stores the two's complement representation of size size and specified by endianness into the blob at indices {k, ..., k + size - 1}."))
((group
    ((name . "blob-u16-ref")
     (signature
       lambda
       ((endianness endianness) (blob? blob) (integer? k))
       integer?)
     (tags pure))
    ((name . "blob-s16-ref")
     (signature
       lambda
       ((endianness endianness) (blob? blob) (integer? k))
       integer?)
     (tags pure))
    ((name . "blob-u16-native-ref")
     (signature lambda ((blob? blob) (integer? k)) integer?)
     (tags pure))
    ((name . "blob-s16-native-ref")
     (signature lambda ((blob? blob) (integer? k)) integer?)
     (tags pure))
    ((name . "blob-u16-set!")
     (signature
       lambda
       ((endianness endianness) (blob? blob) (integer? k) (integer? value))
       undefined))
    ((name . "blob-s16-set!")
     (signature
       lambda
       ((endianness endianness) (blob? blob) (integer? k) (integer? value))
       undefined))
    ((name . "blob-u16-native-set!")
     (signature lambda ((blob? blob) (integer? k) (integer? value)) undefined))
    ((name . "blob-s16-native-set!")
     (signature lambda ((blob? blob) (integer? k) (integer? value)) undefined)))
  (desc . "K must be a valid index of blob; so must the index k+ 1. Endianness must be an endianness object.
These retrieve and set two-octet representations of numbers at indices k and k+1, according to the endianness specified by endianness. The procedures with u16 in their names deal with the unsigned representation, those with s16 with the two's complement representation.
The procedures with native in their names employ the native endianness, and only work at aligned indices: k must be a multiple of 2. It is an error to use them at non-aligned indices."))
 ((group
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
     (signature lambda ((blob? blob) (integer? k) (integer? value)) undefined)))
  (desc . "K must be a valid index of blob; so must the indices {k, ..., k+ 3}. Endianness must be an endianness object.
These retrieve and set four-octet representations of numbers at indices {k, ..., k+ 3}, according to the endianness specified by endianness. The procedures with u32 in their names deal with the unsigned representation, those with s32 with the two's complement representation.
The procedures with native in their names employ the native endianness, and only work at aligned indices: k must be a multiple of 4. It is an error to use them at non-aligned indices."))
 ((group
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
     (signature lambda ((blob? blob) (integer? k) (integer? value)) undefined)))
  (desc . "K must be a valid index of blob; so must the indices {k, ..., k+ 7}. Endianness must be an endianness object.
These retrieve and set eight-octet representations of numbers at indices {k, ..., k+ 7}, according to the endianness specified by endianness. The procedures with u64 in their names deal with the unsigned representation, those with s64 with the two's complement representation.
The procedures with native in their names employ the native endianness, and only work at aligned indices: k must be a multiple of 8. It is an error to use them at non-aligned indices."))
 ((name . "blob=?")
  (signature lambda ((blob? blob1) (blob? blob2)) boolean?)
  (tags pure)
  (desc . "Returns #t if blob-1 and blob-2 are equal---that is, if they have the same length and equal octets at all valid indices."))
 ((name . "blob-copy!")
  (signature
   lambda
   ((blob? source)
    (integer? source-start)
    (blob? target)
    (integer? target-start)
    (integer? n))
   boolean?)
  (tags pure)
  (desc . "Copies data from blob source to blob target. Source-start, target-start, and n must be non-negative exact integers that satisfy
0 <= source-start <= source-start + n <= (blob-length source)
0 <= target-start <= target-start + n <= (blob-length target)

This copies the octets from source at indices [source-start, source-start + n) to consecutive indices in target starting at target-index.
This must work even if the memory regions for the source and the target overlap, i.e., the octets at the target location after the copy must be equal to the octets at the source location before the copy.
The return values are unspecified."))
 ((name . "blob-copy") (signature lambda ((blob? blob)) blob?) (tags pure) (desc . "Returns a newly allocated copy of blob blob."))
 ((name . "blob->u8-list") (signature lambda ((blob? blob)) list?) (tags pure) (desc . "blob->u8-list returns a newly allocated list of the octets of blob in the same order."))
 ((name . "u8-list->blob")
  (signature lambda ((list? octets)) blob?)
  (tags pure)
  (desc . "u8-list->blob returns a newly allocated blob whose elements are the elements of list octets, which must all be octets, in the same order. Analogous to list->vector."))
 ((group
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
  (desc . "Size must be a positive exact integer. Endianness must be an endianness object.
These convert between lists of integers and their consecutive representations according to size and endianness in blobs in the same way as blob->u8-list, blob->s8-list, u8-list->blob, and s8-list->blob do for one-octet representations.")))
