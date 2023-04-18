(((group
    ((name . "floor/")
     (signature
       lambda
       ((integer? numerator) (integer? denominator))
       (values integer? integer?))
     (tags pure))
    ((name . "floor-quotient")
     (signature lambda ((integer? numerator) (integer? denominator)) integer?)
     (tags pure))
    ((name . "floor-remainder")
     (signature lambda ((integer? numerator) (integer? denominator)) integer?)
     (tags pure)))
  (desc . "q = floor(n/d)
Thus a nonzero r is negative iff d is negative."))
 ((group
    ((name . "ceiling/")
     (signature
       lambda
       ((integer? numerator) (integer? denominator))
       (values integer? integer?))
     (tags pure))
    ((name . "ceiling-quotient")
     (signature lambda ((integer? numerator) (integer? denominator)) integer?)
     (tags pure))
    ((name . "ceiling-remainder")
     (signature lambda ((integer? numerator) (integer? denominator)) integer?)
     (tags pure)))
  (desc . "q = ceiling(n/d)
Thus a nonzero r is negative iff d is non-negative.
If denominator is the number of units in a block, and numerator; is some number of units, then (ceiling-quotient numerator denominator) gives the number of blocks needed to cover numerator units. For example, denominator might be the number of bytes in a disk sector, and numerator the number of bytes in a file; then the quotient is the number of disk sectors needed to store the contents of the file. For another example, denominator might be the number of octets in the output of a cryptographic hash function, and numerator the number of octets desired in a key for a symmetric cipher, to be derived using the cryptographic hash function; then the quotient is the number of hash values needed to concatenate to make a key."))
 ((group
    ((name . "truncate/")
     (signature
       lambda
       ((integer? numerator) (integer? denominator))
       (values integer? integer?))
     (tags pure))
    ((name . "truncate-quotient")
     (signature lambda ((integer? numerator) (integer? denominator)) integer?)
     (tags pure))
    ((name . "truncate-remainder")
     (signature lambda ((integer? numerator) (integer? denominator)) integer?)
     (tags pure)))
  (desc . "q = truncate(n/d)
Thus a nonzero r is negative iff n is negative. With the truncate operator pair, the quotient of +1, 0, or -1 by any non-unit denominator is 0; that is, three contiguous numerators divided by a common denominator share a common quotient. Of the other division operator pairs, only the round pair exhibits this property."))
 ((group
    ((name . "round/")
     (signature
       lambda
       ((integer? numerator) (integer? denominator))
       (values integer? integer?))
     (tags pure))
    ((name . "round-quotient")
     (signature lambda ((integer? numerator) (integer? denominator)) integer?)
     (tags pure))
    ((name . "round-remainder")
     (signature lambda ((integer? numerator) (integer? denominator)) integer?)
     (tags pure)))
  (desc . "q = round(n/d)
The round function rounds to the nearest integer, breaking ties by choosing the nearest even integer. Nothing general can be said about the sign of r. Like the truncate operator pair, the quotient of +1, 0, or -1 by any non-unit denominator is 0, so that three contiguous numerators by a common denominator share a common quotient."))
 ((group
    ((name . "euclidean/")
     (signature
       lambda
       ((integer? numerator) (integer? denominator))
       (values integer? integer?))
     (tags pure))
    ((name . "euclidean-quotient")
     (signature lambda ((integer? numerator) (integer? denominator)) integer?)
     (tags pure))
    ((name . "euclidean-remainder")
     (signature lambda ((integer? numerator) (integer? denominator)) integer?)
     (tags pure)))
  (desc . "If d > 0, q = floor(n/d); if d < 0, q = ceiling(n/d).
This division operator pair satisfies the stronger property
0 <= r < |d|,
used often in mathematics. Thus, for example, (euclidean-remainder numerator denominator) is always a valid index into a vector whose length is at least the absolute value of denominator. This division operator pair is so named because it is the subject of the Euclidean division algorithm."))
 ((group
    ((name . "balanced/")
     (signature
       lambda
       ((integer? numerator) (integer? denominator))
       (values integer? integer?))
     (tags pure))
    ((name . "balanced-quotient")
     (signature lambda ((integer? numerator) (integer? denominator)) integer?)
     (tags pure))
    ((name . "balanced-remainder")
     (signature lambda ((integer? numerator) (integer? denominator)) integer?)
     (tags pure)))
  (desc . "This division operator pair satisfies the property
-|d/2| <= r < |d/2|.
When d is a power of 2, say 2k for some k, this reduces to
-2(k - 1) <= r < 2(k - 1).
Computer scientists will immediately recognize this as the interval of integers representable in two's-complement with k bits.")))
