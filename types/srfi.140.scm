(((name . "string?")
  (signature lambda (obj) boolean?)
  (tags pure)
  (desc . "Is obj a string? Must return true if istring? returns true. Must execute in O(1) time."))
 ((name . "istring?")
  (signature lambda (obj) boolean?)
  (tags pure)
  (desc . "Is obj an immutable string, with guaranteed O(1) performance for string-ref and string-length? Must execute in O(1) time."))
 ((name . "string-null?")
  (signature lambda ((string? string)) boolean?)
  (tags pure)
  (desc . "Is string the empty string? Same result as (= (string-length string) 0) but must execute in O(1) time."))
 ((group
    ((name . "string-every")
     (signature case-lambda 
                (((procedure? pred) (string? string)) *)
                (((procedure? pred) (string? string) (integer? start)) *)
                (((procedure? pred) (string? string) (integer? start) (integer? end)) *))
     (subsigs 
       (pred (lambda ((char? c)) boolean?)))
     (tags pure))
    ((name . "string-any")
     (signature case-lambda 
                (((procedure? pred) (string? string)) *)
                (((procedure? pred) (string? string) (integer? start)) *)
                (((procedure? pred) (string? string) (integer? start) (integer? end)) *))
     (subsigs 
       (pred (lambda ((char? c)) boolean?)))
     (tags pure)))
  (desc . "Checks to see if every/any character in string satisfies pred, proceeding from left (index start) to right (index end). These procedures are short-circuiting: if pred returns false, string-every does not call pred on subsequent characters; if pred returns true, string-any does not call pred on subsequent characters. Both procedures are \"witness-generating\":

* If string-every is given an empty interval (with start = end), it returns #t.
* If string-every returns true for a non-empty interval (with start < end), the returned true value is the one returned by the final call to the predicate on (string-ref string (- end 1)).
* If string-any returns true, the returned true value is the one returned by the predicate.

Note: The names of these procedures do not end with a question mark. This indicates a general value is returned instead of a simple boolean (#t or #f)."))
 ((group
    ((name . "string->vector")
     (signature case-lambda
                (((string? string)) vector?)
                (((string? string) (integer? start)) vector?)
                (((string? string) (integer? start) (integer? end)) vector?))
     (subsigs
       (return (vector (char? char))))
     (tags pure))
    ((name . "string->list")
     (signature case-lambda
                (((string? string)) list?)
                (((string? string) (integer? start)) list?)
                (((string? string) (integer? start) (integer? end)) list?))
     (subsigs
       (return (list (char? char))))
     (tags pure)))
  (desc . "string->vector, and string->list return a newly allocated (unless empty) vector, or list of the characters that make up the given substring."))
 ((group
    ((name . "vector->string")
     (signature case-lambda
                (((vector? char-vector)) istring?)
                (((vector? char-vector) (integer? start)) istring?)
                (((vector? char-vector) (integer? start) (integer? end)) istring?))
     (subsigs
       (char-vector (vector (char? char))))
     (tags pure))
    ((name . "list->string")
     (signature case-lambda
                (((list? char-list)) istring?)
                (((list? char-list) (integer? start)) istring?)
                (((list? char-list) (integer? start) (integer? end)) istring?))
     (subsigs
       (char-list (list (char? char))))
     (tags pure)))
  (desc . "These procedures return an istring containing the characters of the given subvector or sublist. The behavior of the result will not be affected by subsequent mutation of the vector or list."))
 ((name . "reverse-list->string")
  (signature lambda ((list? char-list)) istring?)
  (subsigs
    (char-list (list (char? char))))
  (tags pure)
  (desc . "An efficient implementation of (compose list->string reverse):

(reverse-list->string '(#\\a #\\B #\\c)) → \"cBa\"

This is a common idiom in the epilogue of string-processing loops that accumulate their result using a list in reverse order. (See also string-concatenate-reverse for the \"chunked\" variant.)"))
 ((group
    ((name . "string->utf8")
     (signature case-lambda 
                (((string? string)) bytevector?)
                (((string? string) (integer? start)) bytevector?)
                (((string? string) (integer? start) (integer? end)) bytevector?))
     (tags pure))
    ((name . "string->utf16")
     (signature case-lambda 
                (((string? string)) bytevector?)
                (((string? string) (integer? start)) bytevector?)
                (((string? string) (integer? start) (integer? end)) bytevector?))
     (tags pure))
    ((name . "string->utf16be")
     (signature case-lambda 
                (((string? string)) bytevector?)
                (((string? string) (integer? start)) bytevector?)
                (((string? string) (integer? start) (integer? end)) bytevector?))
     (tags pure))
    ((name . "string->utf16le")
     (signature case-lambda 
                (((string? string)) bytevector?)
                (((string? string) (integer? start)) bytevector?)
                (((string? string) (integer? start) (integer? end)) bytevector?))
     (tags pure)))
  (desc . "These procedures return a newly allocated (unless empty) bytevector containing a UTF-8 or UTF-16 encoding of the given substring.
The bytevectors returned by string->utf8, string->utf16be, and string->utf16le do not contain a byte-order mark (BOM). string->utf16be returns a big-endian encoding, while string->utf16le returns a little-endian encoding.
The bytevectors returned by string->utf16 begin with a BOM that declares an implementation-dependent endianness. The latter should match the big-endian or little-endian identifier returned by the R7RS features procedure. The bytevector elements following that BOM encode the given substring using that endianness.
Rationale: These procedures are consistent with the Unicode standard. Unicode suggests UTF-16 should default to big-endian, but Microsoft prefers little-endian."))
 ((group
    ((name . "utf8->string")
     (signature case-lambda
                (((bytevector? bytevector)) istring?)
                (((bytevector? bytevector) (integer? start)) istring?)
                (((bytevector? bytevector) (integer? start) (integer? end)) istring?))
     (tags pure))
    ((name . "utf16->string")
     (signature case-lambda
                (((bytevector? bytevector)) istring?)
                (((bytevector? bytevector) (integer? start)) istring?)
                (((bytevector? bytevector) (integer? start) (integer? end)) istring?))
     (tags pure))
    ((name . "utf16be->string")
     (signature case-lambda
                (((bytevector? bytevector)) istring?)
                (((bytevector? bytevector) (integer? start)) istring?)
                (((bytevector? bytevector) (integer? start) (integer? end)) istring?))
     (tags pure))
    ((name . "utf16le->string")
     (signature case-lambda
                (((bytevector? bytevector)) istring?)
                (((bytevector? bytevector) (integer? start)) istring?)
                (((bytevector? bytevector) (integer? start) (integer? end)) istring?))
     (tags pure)))
  (desc . "These procedures interpret their bytevector argument as a UTF-8 or UTF-16 encoding of a sequence of characters, and return a string containing that sequence.
The bytevector subrange given to utf16->string may begin with a byte order mark (BOM); if so, that BOM determines whether the rest of the subrange is to be interpreted as big-endian or little-endian; in either case, the BOM will not become a character in the returned string. If the subrange does not begin with a BOM, it is decoded using the same implementation-dependent endianness used by string->utf16.
The utf16be->string and utf16le->string procedures interpret their inputs as big-endian or little-endian, respectively. If a BOM is present, it is treated as a normal character and will become part of the result.
It is an error if the bytevector subrange given to utf8->string contains invalid UTF-8 byte sequences. For the other three procedures, it is an error if (- end start) is odd, or if the bytevector subrange contains invalid UTF-16 byte sequences."))
 ((name . "string")
  (signature lambda ((char? char) ...) istring?)
  (tags pure)
  (desc . "Returns a string consisting of the given characters."))
 ((name . "string-tabulate")
  (signature lambda ((procedure? proc) (integer? len)) istring?)
  (subsigs
    (proc (lambda ((integer? index)) char?)))
  (tags pure)
  (desc . "Proc is a procedure that accepts an exact integer as its argument and returns a character. Constructs a string of size len by calling proc on each value from 0 (inclusive) to len (exclusive) to produce the corresponding element of the string. The order in which proc is called on those indexes is not specified.
Rationale: Although string-unfold is more general, string-tabulate is likely to run faster for the common special case it implements."))
 ((group
    ((name . "string-unfold")
     (signature case-lambda 
                (((procedure? stop?) (procedure? mapper) (procedure? successor) seed) istring?)
                (((procedure? stop?) (procedure? mapper) (procedure? successor) seed ((or char? string?) base)) istring?)
                (((procedure? stop?) (procedure? mapper) (procedure? successor) seed ((or char? string?) base) (procedure? make-final)) istring?))
     (subsigs
       (stop? (lambda (seed) boolean?))
       (mapper (lambda (seed) (or char? string?)))
       (successor (lambda (seed) *))
       (make-final (lambda (seed) (or char? string?))))
     (tags pure))
    ((name . "string-unfold-right")
     (signature case-lambda 
                (((procedure? stop?) (procedure? mapper) (procedure? successor) seed) istring?)
                (((procedure? stop?) (procedure? mapper) (procedure? successor) seed ((or char? string?) base)) istring?)
                (((procedure? stop?) (procedure? mapper) (procedure? successor) seed ((or char? string?) base) (procedure? make-final)) istring?))
     (subsigs
       (stop? (lambda (seed) boolean?))
       (mapper (lambda (seed) (or char? string?)))
       (successor (lambda (seed) *))
       (make-final (lambda (seed) (or char? string?))))
     (tags pure)))
  (desc . "This is a fundamental constructor for strings.
* successor is used to generate a series of \"seed\" values from the initial seed:
* seed, (successor seed), (successor2 seed), (successor3 seed), ...
* stop? tells us when to stop — when it returns true when applied to one of these seed values.
* mapper maps each seed value to the corresponding character(s) in the result string, which are assembled into that string in left-to-right order. It is an error for mapper to return anything other than a character or string.
* base is the optional initial/leftmost portion of the constructed string, which defaults to the empty string \"\". It is an error if base is anything other than a character or string.
* make-final is applied to the terminal seed value (on which stop? returns true) to produce the final/rightmost portion of the constructed string. It defaults to (lambda (x) (string)). It is an error for make-final to return anything other than a character or string.

string-unfold-right is the same as string-unfold except the results of mapper are assembled into the string in right-to-left order, base is the optional rightmost portion of the constructed string, and make-final produces the leftmost portion of the constructed string. If mapper returns a string, the string is prepended to the constructed string (without reversal)."))
 ((name . "string-length")
  (signature lambda ((string? string)) integer?)
  (tags pure)
  (desc . "Returns the number of characters in the given string. If the string is an istring, must execute in constant time."))
 ((name . "string-ref")
  (signature lambda ((string? string) (integer? k)) char?)
  (tags pure)
  (desc . "It is an error if k is not a valid index of string. The string-ref procedure returns character k of string using zero-origin indexing. If the string is an istring, must execute in constant time."))
 ((name . "substring")
  (signature lambda ((string? string) (integer? start) (integer? end)) istring?)
  (tags pure)
  (desc . "This procedure returns a istring containing the characters of string starting with index start (inclusive) and ending with index end (exclusive).
If string is a mutable string, then that string does not share any storage with the result, so subsequent mutation of that string will not affect the result returned by substring. When the first argument is an istring, implementations are encouraged to return a result that shares storage with that istring, to whatever extent sharing is possible while maintaining some small fixed bound on the ratio of storage used by the shared representation divided by the storage that would be used by an unshared representation. In particular, these procedures should just return their first argument when that argument is an istring, start is 0, and end is the length of that string.
For the functionality of substring with guaranteed no sharing use xsubstring for an immutable result, or string-copy for a mutable result."))
 ((group
    ((name . "string-take")
     (signature lambda ((string? string) (integer? nchars)) istring?)
     (tags pure))
    ((name . "string-drop")
     (signature lambda ((string? string) (integer? nchars)) istring?)
     (tags pure))
    ((name . "string-take-right")
     (signature lambda ((string? string) (integer? nchars)) istring?)
     (tags pure))
    ((name . "string-drop-right")
     (signature lambda ((string? string) (integer? nchars)) istring?)
     (tags pure)))
  (desc . "string-take returns an immutable string containing the first nchars of string; string-drop returns a string containing all but the first nchars of string. string-take-right returns a string containing the last nchars of string; string-drop-right returns a string containing all but the last nchars of string.
Subsequent mutation of the argument string will not affect the istring returned by these procedures. If string is an istring, implementations are encouraged to return a result that shares storage with that string (which is easily accomplished by using substring to create the result)."))
 ((group
    ((name . "string-pad")
     (signature case-lambda 
                (((string? string) (integer? len)) istring?)
                (((string? string) (integer? len) (char? char)) istring?)
                (((string? string) (integer? len) (char? char) (integer? start)) istring?)
                (((string? string) (integer? len) (char? char) (integer? start) (integer? end)) istring?))
     (tags pure))
    ((name . "string-pad-right")
     (signature case-lambda 
                (((string? string) (integer? len)) istring?)
                (((string? string) (integer? len) (char? char)) istring?)
                (((string? string) (integer? len) (char? char) (integer? start)) istring?)
                (((string? string) (integer? len) (char? char) (integer? start) (integer? end)) istring?))
     (tags pure)))
  (desc . "Returns an istring of length len comprised of the characters drawn from the given subrange of string. The result is padded on the left (right) by as many occurrences of the character char (which defaults to #\\space) as needed. If string has more than len chars, it is truncated on the left (right) to length len."))
 ((group
    ((name . "string-trim")
     (signature case-lambda 
                (((string? string)) istring?)
                (((string? string) (procedure? pred)) istring?)
                (((string? string) (procedure? pred) (integer? start)) istring?)
                (((string? string) (procedure? pred) (integer? start) (integer? end)) istring?))
     (subsigs
       (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "string-trim-right")
     (signature case-lambda 
                (((string? string)) istring?)
                (((string? string) (procedure? pred)) istring?)
                (((string? string) (procedure? pred) (integer? start)) istring?)
                (((string? string) (procedure? pred) (integer? start) (integer? end)) istring?))
     (subsigs
       (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "string-trim-both")
     (signature case-lambda 
                (((string? string)) istring?)
                (((string? string) (procedure? pred)) istring?)
                (((string? string) (procedure? pred) (integer? start)) istring?)
                (((string? string) (procedure? pred) (integer? start) (integer? end)) istring?))
     (subsigs
       (pred (lambda ((char? char)) boolean?)))
     (tags pure)))
  (desc . "Returns a string obtained from the given subrange of string by skipping over all characters on the left / on the right / on both sides that satisfy the second argument pred: pred defaults to char-whitespace?."))
 ((name . "string-replace")
  (signature case-lambda
             (((string? string1) (string? string2) (integer? start1) (integer? end1)) istring?)
             (((string? string1) (string? string2) (integer? start1) (integer? end1) (integer? start2)) istring?)
             (((string? string1) (string? string2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) istring?))
  (tags pure)
  (desc . "Returns
(string-append (substring string1 0 start1)
               (substring string2 start2 end2)
               (substring string1 end1 (string-length string1)))

That is, the segment of characters in string1 from start1 to end1 is replaced by the segment of characters in string2 from start2 to end2. If start1=end1, this simply splices the characters drawn from string2 into string1 at that position."))
 ((group
    ((name . "string=?")
     (signature lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
     (tags pure))
    ((name . "string<?")
     (signature lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
     (tags pure))
    ((name . "string>?")
     (signature lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
     (tags pure))
    ((name . "string<=?")
     (signature lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
     (tags pure))
    ((name . "string>=?")
     (signature lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
     (tags pure))
    ((name . "string-ci=?")
     (signature lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
     (tags pure))
    ((name . "string-ci<?")
     (signature lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
     (tags pure))
    ((name . "string-ci>?")
     (signature lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
     (tags pure))
    ((name . "string-ci<=?")
     (signature lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
     (tags pure))
    ((name . "string-ci>=?")
     (signature lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
     (tags pure)))
  (desc . "As in R7RS"))
 ((group
    ((name . "string-prefix-length")
     (signature case-lambda 
                (((string? string1) (string? string2)) integer?)
                (((string? string1) (string? string2) (integer? start1)) integer?)
                (((string? string1) (string? string2) (integer? start1) (integer? end1)) integer?)
                (((string? string1) (string? string2) (integer? start1) (integer? end1) (integer? start2)) integer?)
                (((string? string1) (string? string2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) integer?))
     (tags pure))
    ((name . "string-suffix-length")
     (signature case-lambda 
                (((string? string1) (string? string2)) integer?)
                (((string? string1) (string? string2) (integer? start1)) integer?)
                (((string? string1) (string? string2) (integer? start1) (integer? end1)) integer?)
                (((string? string1) (string? string2) (integer? start1) (integer? end1) (integer? start2)) integer?)
                (((string? string1) (string? string2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) integer?)
                )
     (tags pure)))
  (desc . "Return the length of the longest common prefix/suffix of string1 and string2. For prefixes, this is equivalent to their \"mismatch index\" (relative to the start indexes).
The optional start/end indexes restrict the comparison to the indicated substrings of string1 and string2."))
 ((group
    ((name . "string-prefix?")
     (signature case-lambda 
                (((string? string1) (string? string2)) boolean?)
                (((string? string1) (string? string2) (integer? start1)) boolean?)
                (((string? string1) (string? string2) (integer? start1) (integer? end1)) boolean?)
                (((string? string1) (string? string2) (integer? start1) (integer? end1) (integer? start2)) boolean?)
                (((string? string1) (string? string2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) boolean?))
     (tags pure))
    ((name . "string-suffix?")
     (signature case-lambda 
                (((string? string1) (string? string2)) boolean?)
                (((string? string1) (string? string2) (integer? start1)) boolean?)
                (((string? string1) (string? string2) (integer? start1) (integer? end1)) boolean?)
                (((string? string1) (string? string2) (integer? start1) (integer? end1) (integer? start2)) boolean?)
                (((string? string1) (string? string2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) boolean?))
     (tags pure)))
  (desc . "Is string1 a prefix/suffix of string2?
The optional start/end indexes restrict the comparison to the indicated substrings of string1 and string2."))
 ((group
    ((name . "string-index")
     (signature case-lambda
                (((string? string) (procedure? pred)) (or integer? #f))
                (((string? string) (procedure? pred) (integer? start)) (or integer? #f))
                (((string? string) (procedure? pred) (integer? start) (integer? end)) (or integer? #f)))
     (subsigs
       (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "string-index-right")
     (signature case-lambda
                (((string? string) (procedure? pred)) (or integer? #f))
                (((string? string) (procedure? pred) (integer? start)) (or integer? #f))
                (((string? string) (procedure? pred) (integer? start) (integer? end)) (or integer? #f)))
     (subsigs
       (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "string-skip")
     (signature case-lambda
                (((string? string) (procedure? pred)) (or integer? #f))
                (((string? string) (procedure? pred) (integer? start)) (or integer? #f))
                (((string? string) (procedure? pred) (integer? start) (integer? end)) (or integer? #f)))
     (subsigs
       (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "string-skip-right")
     (signature case-lambda
                (((string? string) (procedure? pred)) (or integer? #f))
                (((string? string) (procedure? pred) (integer? start)) (or integer? #f))
                (((string? string) (procedure? pred) (integer? start) (integer? end)) (or integer? #f)))
     (subsigs
       (pred (lambda ((char? char)) boolean?)))
     (tags pure)))
  (desc . "string-index searches through the given substring from the left, returning the index of the leftmost character satisfying the predicate pred. string-index-right searches from the right, returning the index of the rightmost character satisfying the predicate pred. If no match is found, these procedures return #f.
Rationale: The SRFI 130 analogues of these procedures return cursors, even when no match is found, and SRFI 130's string-index-right returns the successor of the cursor for the first character that satisfies the predicate. As there are no cursors in this SRFI, it seems best to follow the more intuitive and long-standing precedent set by SRFI 13.
The start and end arguments specify the beginning and end of the search; the valid indexes relevant to the search include start but exclude end. Beware of \"fencepost\" errors: when searching right-to-left, the first index considered is (- end 1), whereas when searching left-to-right, the first index considered is start. That is, the start/end indexes describe the same half-open interval [start,end) in these procedures that they do in all other procedures specified by this SRFI.
The skip functions are similar, but use the complement of the criterion: they search for the first char that doesn't satisfy pred. To skip over initial whitespace, for example, say
(substring string
            (or (string-skip string char-whitespace?)
                (string-length string))
            (string-length string))

These functions can be trivially composed with string-take and string-drop to produce take-while, drop-while, span, and break procedures without loss of efficiency."))
 ((group
    ((name . "string-contains")
     (signature case-lambda
                (((string? string1) (string? string2)) (or integer? #f))
                (((string? string1) (string? string2) (integer? start1)) (or integer? #f))
                (((string? string1) (string? string2) (integer? start1) (integer? end1)) (or integer? #f))
                (((string? string1) (string? string2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) (or integer? #f)))
     (tags pure))
    ((name . "string-contains-right")
     (signature case-lambda
                (((string? string1) (string? string2)) (or integer? #f))
                (((string? string1) (string? string2) (integer? start1)) (or integer? #f))
                (((string? string1) (string? string2) (integer? start1) (integer? end1)) (or integer? #f))
                (((string? string1) (string? string2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) (or integer? #f)))
     (tags pure)))
  (desc . "Does the substring of string1 specified by start1 and end1 contain the sequence of characters given by the substring of string2 specified by start2 and end2?
Returns #f if there is no match. If start2 = end2, string-contains returns start1 but string-contains-right returns end1. Otherwise returns the index in string1 for the first character of the first/last match; that index lies within the half-open interval [start1,end1), and the match lies entirely within the [start1,end1) range of string1."))
 ((group
    ((name . "string-upcase")
     (signature lambda ((string? string)) istring?)
     (tags pure))
    ((name . "string-downcase")
     (signature lambda ((string? string)) istring?)
     (tags pure))
    ((name . "string-foldcase")
     (signature lambda ((string? string)) istring?)
     (tags pure))
    ((name . "string-titlecase")
     (signature lambda ((string? string)) istring?)
     (tags pure)))
  (desc . "These procedures return the string obtained by applying Unicode's full uppercasing, lowercasing, case-folding, or title-casing algorithms to their argument. In some cases, the length of the result may be different from the length of the argument. If the result is equal to the argument in the sense of string=?, and the argument is immutable, then that argument may be returned. Note that language-sensitive mappings and foldings are not used.
The results are the same as the R7RS procedures, but as immutable strings."))
 ((name . "string-append")
  (signature lambda ((string? string) ...) istring?)
  (tags pure)
  (desc . "Returns a string whose sequence of characters is the concatenation of the sequences of characters in the given arguments."))
 ((name . "string-concatenate")
  (signature lambda ((list? string-list)) istring?)
  (subsigs
    (string-list (list (string? string))))
  (tags pure)
  (desc . "Concatenates the elements of string-list together into a single string.
If any elements of string-list are mutable strings, then those strings do not share any storage with the result, so subsequent mutation of those string will not affect the string returned by this procedure. Implementations are encouraged to return a result that shares storage with some of the strings in the list if that sharing would be space-efficient.
Rationale: Some implementations of Scheme limit the number of arguments that may be passed to an n-ary procedure, so the (apply string-append string-list) idiom, which is otherwise equivalent to using this procedure, is not as portable."))
 ((name . "string-concatenate-reverse")
  (signature case-lambda
             (((list? string-list)) istring?)
             (((list? string-list) (string? final-string)) istring?)
             (((list? string-list) (string? final-string) (integer? end)) istring?))
  (subsigs
    (string-list (list (string? string))))
  (tags pure)
  (desc . "With no optional arguments, calling this procedure is equivalent to
(string-concatenate (reverse string-list))

If the optional argument final-string is specified, it is effectively consed onto the beginning of string-list before performing the list-reverse and string-concatenate operations.
If the optional argument end is given, only the characters up to but not including end in final-string are added to the result, thus producing
(string-concatenate 
  (reverse (cons (substring final-string 0 end)
                 string-list)))"))
 ((name . "string-join")
  (signature case-lambda
             (((list? string-list)) istring?)
             (((list? string-list) (string? delimiter)) istring?)
             (((list? string-list) (string? delimiter) (symbol? grammar)) istring?))
  (tags pure)
  (desc . "This procedure is a simple unparser; it pastes strings together using the delimiter string.
The string-list is a list of strings. The delimiter is the string used to delimit elements; it defaults to a single space \" \". The grammar argument is a symbol that determines how the delimiter is used, and defaults to 'infix. It is an error for grammar to be any symbol other than these four:
* 'infix means an infix or separator grammar: insert the delimiter between list elements. An empty list will produce an empty string.
* 'strict-infix means the same as 'infix if the string-list is non-empty, but will signal an error if given an empty list. (This avoids an ambiguity shown in the examples below.)
* 'suffix means a suffix or terminator grammar: insert the delimiter after every list element.
* 'prefix means a prefix grammar: insert the delimiter before every list element."))
 ((group
    ((name . "string-fold")
     (signature case-lambda
                (((procedure? kons) knil (string? string)) *)
                (((procedure? kons) knil (string? string) (integer? start)) *)
                (((procedure? kons) knil (string? string) (integer? start) (integer? end)) *))
     (subsigs
       (kons (lambda ((char? char) value) *)))
     (tags pure))
    ((name . "string-fold-right")
     (signature case-lambda
                (((procedure? kons) knil (string? string)) *)
                (((procedure? kons) knil (string? string) (integer? start)) *)
                (((procedure? kons) knil (string? string) (integer? start) (integer? end)) *))
     (subsigs
       (kons (lambda ((char? char) value) *)))
     (tags pure)))
  (desc . "These are the fundamental iterators for strings.
The string-fold procedure maps the kons procedure across the given string from left to right:
(... (kons string[2] (kons string[1] (kons string[0] knil))))
The string-fold-right procedure maps kons across the given string or string from right to left:
(kons string[0]
      (... (kons string[end-3]
                 (kons string[end-2]
                       (kons string[end-1]
                             knil)))))"))
 ((name . "string-map")
  (signature lambda ((procedure? proc) (string? string1) (string? string2) ...) istring?)
  (subsigs
    (proc (lambda ((char? char1) ...) (or char? string?))))
  (tags pure)
  (desc . "As in R7RS, except the result is an immutable string. As an extension, the result from proc may be a string (not just a character)."))
 ((name . "string-for-each")
  (signature lambda ((procedure? proc) (string? string1) (string? string2) ...) undefined)
  (subsigs
    (proc (lambda ((char? char1) ...) undefined)))
  (tags pure)
  (desc . "As in R7RS."))
 ((name . "string-map-index")
  (signature case-lambda
             (((procedure? proc) (string? string)) istring?)
             (((procedure? proc) (string? string) (integer? start)) istring?)
             (((procedure? proc) (string? string) (integer? start) (integer? end)) istring?))
  (subsigs
    (proc (lambda ((integer? index)) (or string? char?))))
  (tags pure)
  (desc . "Calls proc on each valid index of the specified substring, converts the results of those calls into strings, and returns the concatenation of those strings. It is an error for proc to return anything other than a character or string. The dynamic order in which proc is called on the indexes is unspecified, as is the dynamic order in which the coercions are performed. If any strings returned by proc are mutated after they have been returned and before the call to string-map-index has returned, then string-map-index returns a string with unspecified contents; the string-map-index procedure itself does not mutate those strings."))
 ((name . "string-for-each-index")
  (signature case-lambda
             (((procedure? proc) (string? string)) undefined)
             (((procedure? proc) (string? string) (integer? start)) undefined)
             (((procedure? proc) (string? string) (integer? start) (integer? end)) undefined))
  (subsigs
    (proc (lambda ((integer? index)) undefined)))
  (desc . "Calls proc on each valid index of the specified substring, in increasing order, discarding the results of those calls. This is simply a safe and correct way to loop over a substring."))
 ((name . "string-count")
  (signature case-lambda
             (((string? string) (procedure? pred)) integer?)
             (((string? string) (procedure? pred) (integer? start)) integer?)
             (((string? string) (procedure? pred) (integer? start) (integer? end)) integer?))
  (subsigs
    (pred (lambda ((char? char)) boolean?)))
  (tags pure)
  (desc . "Returns a count of the number of characters in the specified substring of string that satisfy the given predicate."))
 ((group
    ((name . "string-filter")
     (signature case-lambda
                (((procedure? pred) (string? string)) istring?)
                (((procedure? pred) (string? string) (integer? start)) istring?)
                (((procedure? pred) (string? string) (integer? start) (integer? end)) istring?))
     (subsigs
       (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "string-remove")
     (signature case-lambda
                (((procedure? pred) (string? string)) istring?)
                (((procedure? pred) (string? string) (integer? start)) istring?)
                (((procedure? pred) (string? string) (integer? start) (integer? end)) istring?))
     (subsigs
       (pred (lambda ((char? char)) boolean?)))
     (tags pure)))
  (desc . "Filter the given substring of string, retaining only those characters that satisfy / do not satisfy pred.
If string is a mutable string, then that string does not share any storage with the result, so subsequent mutation of that string will not affect the string returned by these procedures. If string is an immutable string, implementations are encouraged to return a result that shares storage with that string whenever sharing would be space-efficient."))
 ((name . "string-repeat")
  (signature lambda (((or string? char?) string-or-char) (integer? len)) istring?)
  (tags pure)
  (desc . "Create a string by repeating the first argument len times. If the first argument is a character, it is as if it were wrapped with the string constructor."))
 ((name . "xsubstring")
  (signature case-lambda
             (((string? string)) istring?)
             (((string? string) (integer? from)) istring?)
             (((string? string) (integer? from) (integer? to)) istring?)
             (((string? string) (integer? from) (integer? to) (integer? start)) istring?)
             (((string? string) (integer? from) (integer? to) (integer? start) (integer? end)) istring?))
  (tags pure)
  (desc . "This is an \"extended substring\" procedure that implements replicated copying of a substring.
string is a string; start and end are optional arguments that specify a substring of string, defaulting to 0 and the length of string. This substring is conceptually replicated both up and down the index space, in both the positive and negative directions.
xsubstring returns the substring of this string beginning at index from, and ending at to. It is an error if from is greater than to.
If from and to are missing they default to 0 and from+(end-start), respectively. This variant is a generalization of using substring, but unlike substring never shares substructures that would retain characters or sequences of characters that are substructures of its first argument or previously allocated objects. (Hence it is equivalent to SRFI-135's string-copy.)
Note that
* The from/to arguments give a half-open range containing the characters from index from up to, but not including, index to.
* The from/to indexes are not expressed in the index space of string. They refer instead to the replicated index space of the substring defined by string, start, and end.

It is an error if start=end, unless from=to, which is allowed as a special case."))
 ((name . "string-split")
  (signature case-lambda
             (((string? string) (string? delimiter)) list?)
             (((string? string) (string? delimiter) (symbol? grammar)) list?)
             (((string? string) (string? delimiter) (symbol? grammar) (integer? limit)) list?)
             (((string? string) (string? delimiter) (symbol? grammar) (integer? limit) (integer? start)) list?)
             (((string? string) (string? delimiter) (symbol? grammar) (integer? limit) (integer? start) (integer? end)) list?))
  (subsigs
    (return (list (istring? str))))
  (tags pure)
  (desc . "Returns a list of strings representing the words contained in the substring of string from start (inclusive) to end (exclusive). The delimiter is a string to be used as the word separator. This will often be a single character, but multiple characters are allowed for use cases such as splitting on \"\\r\\n\". The returned list will have one more item than the number of non-overlapping occurrences of the delimiter in the string. If delimiter is an empty string, then the returned list contains a list of strings, each of which contains a single character.
The grammar is a symbol with the same meaning as in the string-join procedure. If it is infix, which is the default, processing is done as described above, except an empty string produces the empty list; if grammar is strict-infix, then an empty string signals an error. The values prefix and suffix cause a leading/trailing empty string in the result to be suppressed.
If limit is a non-negative exact integer, at most that many splits occur, and the remainder of string is returned as the final element of the list (so the result will have at most limit+1 elements). If limit is not specified or is #f, then as many splits as possible are made. It is an error if limit is any other value."))
 ((name . "make-string")
  (signature case-lambda
             (() string?)
             (((integer? k)) string?)
             (((integer? k) (char? char)) string?))
  (tags pure)
  (desc . "Return a new allocated mutable string of length k, where k defaults to 0. If char is given, then all the characters of the string are initialized to char, otherwise the contents of the string are unspecified. The 1-argument version is deprecated as poor style, except when k is 0.
To return an immutable string that repeats k times a character char use string-repeat.
This is as R7RS, except the result is variable-size and we allow leaving out k when it is zero."))
 ((name . "string-copy")
  (signature
   case-lambda
   (((string? string)) string?)
   (((string? string) (integer? start)) string?)
   (((string? string) (integer? start) (integer? end)) string?))
  (tags pure)
  (desc . "Returns a newly allocated copy of the part of the given string between start and end."))
 ((name . "string-set!")
  (signature lambda ((string? string) (integer? k) (char? char)) undefined)
  (desc . "It is an error if k is not a valid index of string. The string-set! procedure stores char in element k of string. There is no requirement for this procedure to execute in constant time."))
 ((name . "string-fill!")
  (signature
   case-lambda
   (((string? string) (char? fill)) undefined)
   (((string? string) (char? fill) (integer? start)) undefined)
   (((string? string) (char? fill) (integer? start) (integer? end))
    undefined))
  (desc . "It is an error if fill is not a character. The string-fill! procedure stores fill in the elements of string between start and end."))
 ((name . "string-copy!")
  (signature
   case-lambda
   (((string? to) (integer? at) (string? from)) undefined)
   (((string? to) (integer? at) (string? from) (integer? start)) undefined)
   (((string? to) (integer? at) (string? from) (integer? start) (integer? end))
    undefined))
  (desc . "It is an error if at is less than zero or greater than the length of to. It is also an error if (- (string-length to) at) is less than (- end start).
Copies the characters of string from between start and end to string to, starting at at. The order in which characters are copied is unspecified, except that if the source and destination overlap, copying takes place as if the source is first copied into a temporary string and then into the destination. This can be achieved without allocating storage by making sure to copy in the correct direction in such circumstances."))
 ((name . "string-append!")
  (signature lambda ((string? string) ((or char? string?) value) ...) undefined)
  (desc . "
The string must be a variable-size mutable string. The string-append! procedure extends string by appending each value (in order) to the end of string. A value can be a character or a string.
Using a string port in this situation is probably preferable: It is more portable, and you can expect decent performance in most implementations. Using string-append! may be slighly more efficient on some implementations, due to lower overhead, but that depends on the strategy used by string-append! when the allocated buffer is too small. The string-append! function is most useful when using (reading) a string is interleaved with growing it, or when also using string-replace!."))
 ((name . "string-replace!")
  (signature case-lambda 
             (((string? dst) (integer? dst-start) (integer? dst-end) (string? src)) undefined)
             (((string? dst) (integer? dst-start) (integer? dst-end) (string? src) (integer? src-start)) undefined)
             (((string? dst) (integer? dst-start) (integer? dst-end) (string? src) (integer? src-start) (integer? src-end)) undefined))
  (desc . "Replaces the characters of the variable-size string dst (between dst-start and dst-end) with the characters of the string src (between src-start and src-end). The number of characters from src may be different than the number replaced in dst, so the string may grow or contract. The special case where dst-start is equal to dst-end corresponds to insertion; the case where src-start is equal to src-end corresponds to deletion. The order in which characters are copied is unspecified, except that if the source and destination overlap, copying takes place as if the source is first copied into a temporary string and then into the destination. (This can be achieved without allocating storage by making sure to copy in the correct direction in such circumstances.)
When value is a string then (string-append! dst value) is equivalent to (string-replace! dst (string-length dst) (string-length dst) value).")))
