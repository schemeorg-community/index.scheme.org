(((name . "text?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Is obj an immutable text? In particular, (text? obj) returns false if (string? obj) returns true, which implies string? returns false if text? returns true. Must execute in O(1) time."))
 ((name . "textual?") (signature lambda (obj) boolean?) (tags pure predicate) (desc . "Returns true if and only if obj is an immutable text or a string. Must execute in O(1) time."))
 ((name . "textual-null?")
  (signature lambda ((textual? textual)) boolean?)
  (tags pure)
  (desc . "Is textual the empty text or the empty string? Must execute in O(1) time."))
 ((group
    ((name . "textual-every")
     (signature
       case-lambda
       (((procedure? pred) (textual? textual)) *)
       (((procedure? pred) (textual? textual) (integer? start)) *)
       (((procedure? pred) (textual? textual) (integer? start) (integer? end)) *))
     (subsigs (pred (lambda ((char? char)) *)))
     (tags pure))
    ((name . "textual-any")
     (signature
       case-lambda
       (((procedure? pred) (textual? textual)) *)
       (((procedure? pred) (textual? textual) (integer? start)) *)
       (((procedure? pred) (textual? textual) (integer? start) (integer? end)) *))
     (subsigs (pred (lambda ((char? char)) *)))
     (tags pure)))
  (desc . "Checks to see if every/any character in textual satisfies pred, proceeding from left (index start) to right (index end). textual-every These procedures are short-circuiting: if pred returns false, textual-every does not call pred on subsequent characters; if pred returns true, textual-any does not call pred on subsequent characters; Both procedures are \"witness-generating\":
* If textual-every is given an empty interval (with start = end), it returns #t.
* If textual-every returns true for a non-empty interval (with start < end), the returned true value is the one returned by the final call to the predicate on (text-ref (textual-copy text) (- end 1)).
* If textual-any returns true, the returned true value is the one returned by the predicate.

Note: The names of these procedures do not end with a question mark. This indicates a general value is returned instead of a simple boolean (#t or #f)."))
 ((name . "make-text")
  (signature lambda ((integer? len) (char? char)) text?)
  (tags pure)
  (desc . "Returns a text of the given length filled with the given character."))
 ((name . "text") (signature lambda ((char? char) ...) text?) (tags pure) (desc . "Returns a text consisting of the given characters."))
 ((name . "text-tabulate")
  (signature lambda ((procedure? proc) (integer? len)) text?)
  (subsigs (proc (lambda ((integer? k)) char?)))
  (tags pure)
  (desc . "Proc is a procedure that accepts an exact integer as its argument and returns a character. Constructs a text of size len by calling proc on each value from 0 (inclusive) to len (exclusive) to produce the corresponding element of the text. The order in which proc is called on those indexes is not specified."))
 ((name . "text-unfold")
  (signature
   case-lambda
   (((procedure? stop?) (procedure? mapper) (procedure? successor) seed) text?)
   (((procedure? stop?)
     (procedure? mapper)
     (procedure? successor)
     seed
     (textual? base))
    text?)
   (((procedure? stop?)
     (procedure? mapper)
     (procedure? successor)
     seed
     (textual? base)
     (procedure? make-final))
    text?))
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string? text?)))
   (success (lambda (seed) *))
   (make-final (lambda (seed) (or char? string? text?))))
  (tags pure)
  (desc . "This is a fundamental constructor for texts.
* successor is used to generate a series of \"seed\" values from the initial seed: seed, (successor seed), (successor2 seed), (successor3 seed), ...
* stop? tells us when to stop — when it returns true when applied to one of these seed values.
* mapper maps each seed value to the corresponding character(s) in the result text, which are assembled into that text in left-to-right order. It is an error for mapper to return anything other than a character, string, or text.
* base is the optional initial/leftmost portion of the constructed text, which defaults to the empty text (text). It is an error if base is anything other than a character, string, or text.
* make-final is applied to the terminal seed value (on which stop? returns true) to produce the final/rightmost portion of the constructed text. It defaults to (lambda (x) (text)). It is an error for make-final to return anything other than a character, string, or text.

text-unfold is a fairly powerful text constructor. You can use it to convert a list to a text, read a port into a text, reverse a text, copy a text, and so forth."))
 ((name . "text-unfold-right")
  (signature
   case-lambda
   (((procedure? stop?) (procedure? mapper) (procedure? successor) seed) text?)
   (((procedure? stop?)
     (procedure? mapper)
     (procedure? successor)
     seed
     (textual? base))
    text?)
   (((procedure? stop?)
     (procedure? mapper)
     (procedure? successor)
     seed
     (textual? base)
     (procedure? make-final))
    text?))
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string? text?)))
   (success (lambda (seed) *))
   (make-final (lambda (seed) (or char? string? text?))))
  (tags pure)
  (desc . "This is a fundamental constructor for texts. It is the same as text-unfold except the results of mapper are assembled into the text in right-to-left order, base is the optional rightmost portion of the constructed text, and make-final produces the leftmost portion of the constructed text."))
 ((name . "textual->text")
  (signature lambda ((textual? textual)) text?)
  (tags pure)
  (desc . "When given a text, textual->text just returns that text. When given a string, textual->text returns the result of calling string->text on that string. Signals an error when its argument is neither string nor text."))
 ((group
    ((name . "textual->string")
     (signature
       case-lambda
       (((textual? textual)) string?)
       (((textual? textual) (integer? start)) string?)
       (((textual? textual) (integer? start) (integer? end)) string?))
     (tags pure))
    ((name . "textual->vector")
     (signature
       case-lambda
       (((textual? textual)) vector?)
       (((textual? textual) (integer? start)) vector?)
       (((textual? textual) (integer? start) (integer? end)) vector?))
     (subsigs
       (return (vector (char? c))))
     (tags pure))
    ((name . "textual->list")
     (signature
       case-lambda
       (((textual? textual)) list?)
       (((textual? textual) (integer? start)) list?)
       (((textual? textual) (integer? start) (integer? end)) list?))
     (subsigs
       (return (list (char? c))))
     (tags pure)))
  (desc . "textual->string, textual->vector, and textual->list return a newly allocated (unless empty) mutable string, vector, or list of the characters that make up the given subtext or substring."))
 ((group
    ((name . "string->text")
     (signature
       case-lambda
       (((string? string)) text?)
       (((string? string) (integer? start)) text?)
       (((string? string) (integer? start) (integer? end)) text?))
     (tags pure))
    ((name . "vector->text")
     (signature
       case-lambda
       (((vector? vector)) text?)
       (((vector? vector) (integer? start)) text?)
       (((vector? vector) (integer? start) (integer? end)) text?))
     (subsigs
       (vector (vector (char? c))))
     (tags pure))
    ((name . "list->text")
     (signature
       case-lambda
       (((list? list)) text?)
       (((list? list) (integer? start)) text?)
       (((list? list) (integer? start) (integer? end)) text?))
     (subsigs
       (list (list (char? c))))
     (tags pure)))
  (desc . "These procedures return a text containing the characters of the given substring, subvector, or sublist. The behavior of the text will not be affected by subsequent mutation of the given string, vector, or list."))
 ((name . "reverse-list->text")
  (signature lambda ((list? char-list)) text?)
  (subsigs
    (char-list (list (char? c))))
  (tags pure)
  (desc . "An efficient implementation of (compose list->text reverse):
(reverse-list->text '(#\\a #\\B #\\c)) → \"cBa\"

This is a common idiom in the epilogue of text-processing loops that accumulate their result using a list in reverse order. (See also textual-concatenate-reverse for the \"chunked\" variant.)"))
 ((group
    ((name . "textual->utf8")
     (signature
       case-lambda
       (((textual? textual)) bytevector?)
       (((textual? textual) (integer? start)) bytevector?)
       (((textual? textual) (integer? start) (integer? end)) bytevector?))
     (tags pure))
    ((name . "textual->utf16")
     (signature
       case-lambda
       (((textual? textual)) bytevector?)
       (((textual? textual) (integer? start)) bytevector?)
       (((textual? textual) (integer? start) (integer? end)) bytevector?))
     (tags pure))
    ((name . "textual->utf16be")
     (signature
       case-lambda
       (((textual? textual)) bytevector?)
       (((textual? textual) (integer? start)) bytevector?)
       (((textual? textual) (integer? start) (integer? end)) bytevector?))
     (tags pure))
    ((name . "textual->utf16le")
     (signature
       case-lambda
       (((textual? textual)) bytevector?)
       (((textual? textual) (integer? start)) bytevector?)
       (((textual? textual) (integer? start) (integer? end)) bytevector?))
     (tags pure)))
  (desc . "These procedures return a newly allocated (unless empty) bytevector containing a UTF-8 or UTF-16 encoding of the given subtext or substring.
The bytevectors returned by textual->utf8, textual->utf16be, and textual->utf16le do not contain a byte-order mark (BOM). textual->utf16be returns a big-endian encoding, while textual->utf16le returns a little-endian encoding.
The bytevectors returned by textual->utf16 begin with a BOM that declares an implementation-dependent endianness, and the bytevector elements following that BOM encode the given subtext or substring using that endianness."))
 ((group
    ((name . "utf8->text")
     (signature
       case-lambda
       (((bytevector? bytevector)) text?)
       (((bytevector? bytevector) (integer? start)) text?)
       (((bytevector? bytevector) (integer? start) (integer? end)) text?))
     (tags pure))
    ((name . "utf16->text")
     (signature
       case-lambda
       (((bytevector? bytevector)) text?)
       (((bytevector? bytevector) (integer? start)) text?)
       (((bytevector? bytevector) (integer? start) (integer? end)) text?))
     (tags pure))
    ((name . "utf16be->text")
     (signature
       case-lambda
       (((bytevector? bytevector)) text?)
       (((bytevector? bytevector) (integer? start)) text?)
       (((bytevector? bytevector) (integer? start) (integer? end)) text?))
     (tags pure))
    ((name . "utf16le->text")
     (signature
       case-lambda
       (((bytevector? bytevector)) text?)
       (((bytevector? bytevector) (integer? start)) text?)
       (((bytevector? bytevector) (integer? start) (integer? end)) text?))
     (tags pure)))
  (desc . "These procedures interpret their bytevector argument as a UTF-8 or UTF-16 encoding of a sequence of characters, and return a text containing that sequence.
The bytevector subrange given to utf16->text may begin with a byte order mark (BOM); if so, that BOM determines whether the rest of the subrange is to be interpreted as big-endian or little-endian; in either case, the BOM will not become a character in the returned text. If the subrange does not begin with a BOM, it is decoded using the same implementation-dependent endianness used by textual->utf16.
The utf16be->text and utf16le->text procedures interpret their inputs as big-endian or little-endian, respectively. If a BOM is present, it is treated as a normal character and will become part of the result.
It is an error if the bytevector subrange given to utf8->text contains invalid UTF-8 byte sequences. For the other three procedures, it is an error if start or end are odd, or if the bytevector subrange contains invalid UTF-16 byte sequences."))
 ((name . "text-length")
  (signature lambda ((text? text)) integer?)
  (tags pure)
  (desc . "Returns the number of characters within the given text. Must execute in O(1) time."))
 ((name . "text-ref")
  (signature lambda ((text? text) (integer? idx)) char?)
  (tags pure)
  (desc . "Returns character text[idx], using 0-origin indexing. Must execute in O(1) time."))
 ((name . "textual-length")
  (signature lambda ((textual? textual)) integer?)
  (tags pure)
  (desc . "Returns the number of characters within the given textual. Generalization of text-length to accept strings as well as texts."))
 ((name . "textual-ref")
  (signature lambda ((textual? text) (integer? idx)) char?)
  (tags pure)
  (desc . "Returns character text[idx], using 0-origin indexing. Generalization of text-ref to accept strings as well as texts."))
 ((group
    ((name . "subtext")
     (signature lambda ((text? text) (integer? start) (integer? end)) text?)
     (tags pure))
    ((name . "subtextual")
     (signature lambda ((textual? text) (integer? start) (integer? end)) text?)
     (tags pure)))
  (desc . "These procedures return a text containing the characters of text or textual beginning with index start (inclusive) and ending with index end (exclusive).
If textual is a string, then that string does not share any storage with the result, so subsequent mutation of that string will not affect the text returned by subtextual. When the first argument is a text, as is required by subtext, implementations are encouraged to return a result that shares storage with that text, to whatever extent sharing is possible while maintaining some small fixed bound on the ratio of storage used by the shared representation divided by the storage that would be used by an unshared representation. In particular, these procedures should just return their first argument when that argument is a text, start is 0, and end is the length of that text."))
 ((name . "textual-copy")
  (signature
   case-lambda
   (((textual? textual)) text?)
   (((textual? textual) (integer? start)) text?)
   (((textual? textual) (integer? start) (integer? end)) text?))
  (tags pure)
  (desc . "Returns a text containing the characters of textual beginning with index start (inclusive) and ending with index end (exclusive).
Unlike subtext and subtextual, the result of textual-copy never shares substructures that would retain characters or sequences of characters that are substructures of its first argument or previously allocated objects.
If textual-copy returns an empty text, that empty text may be eq? or eqv? to the text returned by (text). If the text returned by textual-copy is non-empty, then it is not eqv? to any previously extant object."))
 ((group
    ((name . "textual-take")
     (signature lambda ((textual? textual) (integer? nchars)) text?)
     (tags pure))
    ((name . "textual-drop")
     (signature lambda ((textual? textual) (integer? nchars)) text?)
     (tags pure))
    ((name . "textual-take-right")
     (signature lambda ((textual? textual) (integer? nchars)) text?)
     (tags pure))
    ((name . "textual-drop-right")
     (signature lambda ((textual? textual) (integer? nchars)) text?)
     (tags pure)))
  (desc . "textual-take returns a text containing the first nchars of textual; textual-drop returns a text containing all but the first nchars of textual. textual-take-right returns a text containing the last nchars of textual; textual-drop-right returns a text containing all but the last nchars of textual.
If textual is a string, then that string does not share any storage with the result, so subsequent mutation of that string will not affect the text returned by these procedures. If textual is a text, implementations are encouraged to return a result that shares storage with that text (which is easily accomplished by using subtext to create the result)."))
 ((group
    ((name . "textual-pad")
     (signature
       case-lambda
       (((textual? textual) (integer? len)) text?)
       (((textual? textual) (integer? len) (char? char)) text?)
       (((textual? textual) (integer? len) (char? char) (integer? start)) text?)
       (((textual? textual)
         (integer? len)
         (char? char)
         (integer? start)
         (integer? end))
        text?))
     (tags pure))
    ((name . "textual-pad-right")
     (signature
       case-lambda
       (((textual? textual) (integer? len)) text?)
       (((textual? textual) (integer? len) (char? char)) text?)
       (((textual? textual) (integer? len) (char? char) (integer? start)) text?)
       (((textual? textual)
         (integer? len)
         (char? char)
         (integer? start)
         (integer? end))
        text?))
     (tags pure)))
  (desc . "Returns a text of length len comprised of the characters drawn from the given subrange of textual, padded on the left (right) by as many occurrences of the character char as needed. If textual has more than len chars, it is truncated on the left (right) to length len. char defaults to #\\space.
If textual is a string, then that string does not share any storage with the result, so subsequent mutation of that string will not affect the text returned by these procedures. If textual is a text, implementations are encouraged to return a result that shares storage with that text whenever sharing would be space-efficient."))
 ((group
    ((name . "textual-trim")
     (signature
       case-lambda
       (((textual? textual)) text?)
       (((textual? textual) (procedure? pred)) text?)
       (((textual? textual) (procedure? pred) (integer? start)) text?)
       (((textual? textual) (procedure? pred) (integer? start) (integer? end))
        text?))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "textual-trim-right")
     (signature
       case-lambda
       (((textual? textual)) text?)
       (((textual? textual) (procedure? pred)) text?)
       (((textual? textual) (procedure? pred) (integer? start)) text?)
       (((textual? textual) (procedure? pred) (integer? start) (integer? end))
        text?))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "textual-trim-both")
     (signature
       case-lambda
       (((textual? textual)) text?)
       (((textual? textual) (procedure? pred)) text?)
       (((textual? textual) (procedure? pred) (integer? start)) text?)
       (((textual? textual) (procedure? pred) (integer? start) (integer? end))
        text?))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure)))
  (desc . "Returns a text obtained from the given subrange of textual by skipping over all characters on the left / on the right / on both sides that satisfy the second argument pred: pred defaults to char-whitespace?.
If textual is a string, then that string does not share any storage with the result, so subsequent mutation of that string will not affect the text returned by these procedures. If textual is a text, implementations are encouraged to return a result that shares storage with that text whenever sharing would be space-efficient."))
 ((name . "textual-replace")
  (signature
   case-lambda
   (((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
    text?)
   (((textual? textual1)
     (textual? textual2)
     (integer? start1)
     (integer? end1)
     (integer? start2))
    text?)
   (((textual? textual1)
     (textual? textual2)
     (integer? start1)
     (integer? end1)
     (integer? start2)
     (integer? end2))
    text?))
  (tags pure)
  (desc . "Returns
(textual-append (subtextual textual1 0 start1)
                (subtextual textual2 start2 end2)
                (subtextual textual1 end1 (textual-length textual1)))

That is, the segment of characters in textual1 from start1 to end1 is replaced by the segment of characters in textual2 from start2 to end2. If start1=end1, this simply splices the characters drawn from textual2 into textual1 at that position."))
 ((name . "textual=?")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (textual? textual3) ...)
   boolean?)
  (tags pure)
  (desc . "Returns #t if all the texts have the same length and contain exactly the same characters in the same positions; otherwise returns #f."))
 ((group
    ((name . "textual<?")
     (signature
       lambda
       ((textual? textual1) (textual? textual2) (textual? textual3) ...)
       boolean?)
     (tags pure))
    ((name . "textual>?")
     (signature
       lambda
       ((textual? textual1) (textual? textual2) (textual? textual3) ...)
       boolean?)
     (tags pure))
    ((name . "textual<=?")
     (signature
       lambda
       ((textual? textual1) (textual? textual2) (textual? textual3) ...)
       boolean?)
     (tags pure))
    ((name . "textual>=?")
     (signature
       lambda
       ((textual? textual1) (textual? textual2) (textual? textual3) ...)
       boolean?)
     (tags pure)))
  (desc . "These procedures return #t if their arguments are (respectively): monotonically increasing, monotonically decreasing, monotonically non-decreasing, or monotonically non-increasing.
These comparison predicates are required to be transitive.
These procedures compare texts in an implementation-defined way. One approach is to make them the lexicographic extensions to texts of the corresponding orderings on characters. In that case, text<? would be the lexicographic ordering on texts induced by the ordering char<? on characters, and if two texts differ in length but are the same up to the length of the shorter text, the shorter text would be considered to be lexicographically less than the longer string. However, implementations are also allowed to use more sophisticated locale-specific orderings.
In all cases, a pair of texts must satisfy exactly one of textual<?, textual=?, and textual>?, must satisfy textual<=? if and only if they do not satisfy textual>?, and must satisfy textual>=? if and only if they do not satisfy textual<?."))
 ((name . "textual-ci=?")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (textual? textual3) ...)
   boolean?)
  (tags pure)
  (desc . "Returns #t if, after calling textual-foldcase on each of the arguments, all of the case-folded texts would have the same length and contain the same characters in the same positions; otherwise returns #f."))
 ((group
    ((name . "textual-ci<?")
     (signature
       lambda
       ((textual? textual1) (textual? textual2) (textual? textual3) ...)
       boolean?)
     (tags pure))
    ((name . "textual-ci>?")
     (signature
       lambda
       ((textual? textual1) (textual? textual2) (textual? textual3) ...)
       boolean?)
     (tags pure))
    ((name . "textual-ci<=?")
     (signature
       lambda
       ((textual? textual1) (textual? textual2) (textual? textual3) ...)
       boolean?)
     (tags pure))
    ((name . "textual-ci>=?")
     (signature
       lambda
       ((textual? textual1) (textual? textual2) (textual? textual3) ...)
       boolean?)
     (tags pure)))
  (desc . "These procedures behave as though they had called textual-foldcase on their arguments before applying the corresponding procedures without \"-ci\"."))
 ((group
    ((name . "textual-prefix-length")
     (signature
       case-lambda
       (((textual? textual1) (textual? textual2)) integer?)
       (((textual? textual1) (textual? textual2) (integer? start1)) integer?)
       (((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
        integer?)
       (((textual? textual1)
         (textual? textual2)
         (integer? start1)
         (integer? end1)
         (integer? start2))
        integer?)
       (((textual? textual1)
         (textual? textual2)
         (integer? start1)
         (integer? end1)
         (integer? start2)
         (integer? end2))
        integer?))
     (tags pure))
    ((name . "textual-suffix-length")
     (signature
       case-lambda
       (((textual? textual1) (textual? textual2)) integer?)
       (((textual? textual1) (textual? textual2) (integer? start1)) integer?)
       (((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
        integer?)
       (((textual? textual1)
         (textual? textual2)
         (integer? start1)
         (integer? end1)
         (integer? start2))
        integer?)
       (((textual? textual1)
         (textual? textual2)
         (integer? start1)
         (integer? end1)
         (integer? start2)
         (integer? end2))
        integer?))
     (tags pure)))
  (desc . "Return the length of the longest common prefix/suffix of textual1 and textual2. For prefixes, this is equivalent to their \"mismatch index\" (relative to the start indexes).
The optional start/end indexes restrict the comparison to the indicated subtexts of textual1 and textual2."))
 ((group
    ((name . "textual-prefix?")
     (signature
       case-lambda
       (((textual? textual1) (textual? textual2)) boolean?)
       (((textual? textual1) (textual? textual2) (integer? start1)) boolean?)
       (((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
        boolean?)
       (((textual? textual1)
         (textual? textual2)
         (integer? start1)
         (integer? end1)
         (integer? start2))
        boolean?)
       (((textual? textual1)
         (textual? textual2)
         (integer? start1)
         (integer? end1)
         (integer? start2)
         (integer? end2))
        boolean?))
     (tags pure))
    ((name . "textual-suffix?")
     (signature
       case-lambda
       (((textual? textual1) (textual? textual2)) boolean?)
       (((textual? textual1) (textual? textual2) (integer? start1)) boolean?)
       (((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
        boolean?)
       (((textual? textual1)
         (textual? textual2)
         (integer? start1)
         (integer? end1)
         (integer? start2))
        boolean?)
       (((textual? textual1)
         (textual? textual2)
         (integer? start1)
         (integer? end1)
         (integer? start2)
         (integer? end2))
        boolean?))
     (tags pure)))
  (desc . "Is textual1 a prefix/suffix of textual2?
The optional start/end indexes restrict the comparison to the indicated subtexts of textual1 and textual2. "))
 ((group
    ((name . "textual-index")
     (signature
       case-lambda
       (((textual? textual) (procedure? pred)) (or #f integer?))
       (((textual? textual) (procedure? pred) (integer? start)) (or #f integer?))
       (((textual? textual) (procedure? pred) (integer? start) (integer? end))
        (or #f integer?)))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "textual-index-right")
     (signature
       case-lambda
       (((textual? textual) (procedure? pred)) (or #f integer?))
       (((textual? textual) (procedure? pred) (integer? start)) (or #f integer?))
       (((textual? textual) (procedure? pred) (integer? start) (integer? end))
        (or #f integer?)))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "textual-skip")
     (signature
       case-lambda
       (((textual? textual) (procedure? pred)) (or #f integer?))
       (((textual? textual) (procedure? pred) (integer? start)) (or #f integer?))
       (((textual? textual) (procedure? pred) (integer? start) (integer? end))
        (or #f integer?)))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "textual-skip-right")
     (signature
       case-lambda
       (((textual? textual) (procedure? pred)) (or #f integer?))
       (((textual? textual) (procedure? pred) (integer? start)) (or #f integer?))
       (((textual? textual) (procedure? pred) (integer? start) (integer? end))
        (or #f integer?)))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure)))
  (desc . "textual-index searches through the given subtext or substring from the left, returning the index of the leftmost character satisfying the predicate pred. textual-index-right searches from the right, returning the index of the rightmost character satisfying the predicate pred. If no match is found, these procedures return #f.
Rationale: The SRFI 130 analogues of these procedures return cursors, even when no match is found, and SRFI 130's string-index-right returns the successor of the cursor for the first character that satisfies the predicate. As there are no cursors in this SRFI, it seems best to follow the more intuitive and long-standing precedent set by SRFI 13.
The start and end arguments specify the beginning and end of the search; the valid indexes relevant to the search include start but exclude end. Beware of \"fencepost\" errors: when searching right-to-left, the first index considered is (- end 1), whereas when searching left-to-right, the first index considered is start. That is, the start/end indexes describe the same half-open interval [start,end) in these procedures that they do in all other procedures specified by this SRFI.
The skip functions are similar, but use the complement of the criterion: they search for the first char that doesn't satisfy pred."))
 ((group
    ((name . "textual-contains")
     (signature
       case-lambda
       (((textual? textual1) (textual? textual2)) (or #f integer?))
       (((textual? textual1) (textual? textual2) (integer? start1))
        (or #f integer?))
       (((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
        (or #f integer?))
       (((textual? textual1)
         (textual? textual2)
         (integer? start1)
         (integer? end1)
         (integer? start2))
        (or #f integer?))
       (((textual? textual1)
         (textual? textual2)
         (integer? start1)
         (integer? end1)
         (integer? start2)
         (integer? end2))
        (or #f integer?)))
     (tags pure))
    ((name . "textual-contains-right")
     (signature
       case-lambda
       (((textual? textual1) (textual? textual2)) (or #f integer?))
       (((textual? textual1) (textual? textual2) (integer? start1))
        (or #f integer?))
       (((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
        (or #f integer?))
       (((textual? textual1)
         (textual? textual2)
         (integer? start1)
         (integer? end1)
         (integer? start2))
        (or #f integer?))
       (((textual? textual1)
         (textual? textual2)
         (integer? start1)
         (integer? end1)
         (integer? start2)
         (integer? end2))
        (or #f integer?)))
     (tags pure)))
  (desc . "Does the subtext of textual1 specified by start1 and end1 contain the sequence of characters given by the subtext of textual2 specified by start2 and end2?
Returns #f if there is no match. If start2 = end2, textual-contains returns start1 but textual-contains-right returns end1. Otherwise returns the index in textual1 for the first character of the first/last match; that index lies within the half-open interval [start1,end1), and the match lies entirely within the [start1,end1) range of textual1."))
 ((group
    ((name . "textual-upcase")
     (signature lambda ((textual? textual)) text?)
     (tags pure))
    ((name . "textual-downcase")
     (signature lambda ((textual? textual)) text?)
     (tags pure))
    ((name . "textual-foldcase")
     (signature lambda ((textual? textual)) text?)
     (tags pure))
    ((name . "textual-titlecase")
     (signature lambda ((textual? textual)) text?)
     (tags pure)))
  (desc . "These procedures return the text obtained by applying Unicode's full uppercasing, lowercasing, case-folding, or title-casing algorithms to their argument. In some cases, the length of the result may be different from the length of the argument. Note that language-sensitive mappings and foldings are not used."))
 ((name . "textual-append")
  (signature lambda ((textual? textual) ...) text?)
  (tags pure)
  (desc . "Returns a text whose sequence of characters is the concatenation of the sequences of characters in the given arguments."))
 ((name . "textual-concatenate")
  (signature lambda ((list? textual-list)) text?)
  (tags pure)
  (desc . "Concatenates the elements of textual-list together into a single text.
If any elements of textual-list are strings, then those strings do not share any storage with the result, so subsequent mutation of those string will not affect the text returned by this procedure. Implementations are encouraged to return a result that shares storage with some of the texts in the list if that sharing would be space-efficient.
Rationale: Some implementations of Scheme limit the number of arguments that may be passed to an n-ary procedure, so the (apply textual-append textual-list) idiom, which is otherwise equivalent to using this procedure, is not as portable."))
 ((name . "textual-concatenate-reverse")
  (signature
   case-lambda
   (((list? textual-list)) text?)
   (((list? textual-list) (textual? final-textual)) text?)
   (((list? textual-list) (textual? final-textual) (integer? end)) text?))
  (tags pure)
  (desc . "With no optional arguments, calling this procedure is equivalent to
(textual-concatenate (reverse textual-list))

If the optional argument final-textual is specified, it is effectively consed onto the beginning of textual-list before performing the list-reverse and textual-concatenate operations.
If the optional argument end is given, only the characters up to but not including end in final-textual are added to the result, thus producing
(textual-concatenate 
  (reverse (cons (subtext final-textual 0 end)
                 textual-list)))"))
 ((name . "textual-join")
  (signature
   case-lambda
   (((list? textual-list)) text?)
   (((list? textual-list) (textual? delimiter)) text?)
   (((list? textual-list) (textual? delimiter) (symbol? grammar)) text?))
  (tags pure)
  (desc . "This procedure is a simple unparser; it pastes texts together using the delimiter text.
textual-list is a list of texts and/or strings. delimiter is a text or a string. The grammar argument is a symbol that determines how the delimiter is used, and defaults to 'infix. It is an error for grammar to be any symbol other than these four:
 'infix means an infix or separator grammar: insert the delimiter between list elements. An empty list will produce an empty text.
 'strict-infix means the same as 'infix if the textual-list is non-empty, but will signal an error if given an empty list. (This avoids an ambiguity shown in the examples below.)
 'suffix means a suffix or terminator grammar: insert the delimiter after every list element.
 'prefix means a prefix grammar: insert the delimiter before every list element.

The delimiter is the text used to delimit elements; it defaults to a single space \" \"."))
 ((group
    ((name . "textual-fold")
     (signature
       case-lambda
       (((procedure? kons) knil (textual? textual)) *)
       (((procedure? kons) knil (textual? textual) (integer? start)) *)
       (((procedure? kons) knil (textual? textual) (integer? start) (integer? end))
        *))
     (subsigs (kons (lambda ((char? char) state) *)))
     (tags pure))
    ((name . "textual-fold-right")
     (signature
       case-lambda
       (((procedure? kons) knil (textual? textual)) *)
       (((procedure? kons) knil (textual? textual) (integer? start)) *)
       (((procedure? kons) knil (textual? textual) (integer? start) (integer? end))
        *))
     (subsigs (kons (lambda ((char? char) state) *)))
     (tags pure)))
  (desc . "These are the fundamental iterators for texts.
The textual-fold procedure maps the kons procedure across the given text or string from left to right:
(... (kons textual[2] (kons textual[1] (kons textual[0] knil))))

The textual-fold-right procedure maps kons across the given text or string from right to left:
(kons textual[0]
      (... (kons textual[end-3]
                 (kons textual[end-2]
                       (kons textual[end-1]
                             knil)))))"))
 ((name . "textual-map")
  (signature
   lambda
   ((procedure? proc) (textual? textual1) (textual? textual2) ...)
   text?)
  (subsigs
   (proc (lambda ((char? char1) (char? char2) ...) (or textual? char?))))
  (tags pure)
  (desc . "It is an error if proc does not accept as many arguments as the number of textual arguments passed to textual-map, does not accept characters as arguments, or returns a value that is not a character, string, or text.
The textual-map procedure applies proc element-wise to the characters of the textual arguments, converts each value returned by proc to a text, and returns the concatenation of those texts. If more than one textual argument is given and not all have the same length, then textual-map terminates when the shortest textual argument runs out. The dynamic order in which proc is called on the characters of the textual arguments is unspecified, as is the dynamic order in which the coercions are performed. If any strings returned by proc are mutated after they have been returned and before the call to textual-map has returned, then textual-map returns a text with unspecified contents; the textual-map procedure itself does not mutate those strings."))
 ((name . "textual-for-each")
  (signature
   lambda
   ((procedure? proc) (textual? textual1) (textual? textual2) ...)
   undefined)
  (subsigs (proc (lambda ((char? char1) (char? char2) ...) undefined)))
  (desc . "It is an error if proc does not accept as many arguments as the number of textual arguments passed to textual-map or does not accept characters as arguments.
The textual-for-each procedure applies proc element-wise to the characters of the textual arguments, going from left to right. If more than one textual argument is given and not all have the same length, then textual-for-each terminates when the shortest textual argument runs out."))
 ((name . "textual-map-index")
  (signature
   case-lambda
   (((procedure? proc) (textual? textual)) text?)
   (((procedure? proc) (textual? textual) (integer? start)) text?)
   (((procedure? proc) (textual? textual) (integer? start) (integer? end))
    text?))
  (subsigs (proc (lambda ((char? char)) (or textual? char?))))
  (tags pure)
  (desc . "Calls proc on each valid index of the specified subtext or substring, converts the results of those calls into texts, and returns the concatenation of those texts. It is an error for proc to return anything other than a character, string, or text. The dynamic order in which proc is called on the indexes is unspecified, as is the dynamic order in which the coercions are performed. If any strings returned by proc are mutated after they have been returned and before the call to textual-map-index has returned, then textual-map-index returns a text with unspecified contents; the textual-map-index procedure itself does not mutate those strings."))
 ((name . "textual-for-each-index")
  (signature
   case-lambda
   (((procedure? proc) (textual? textual)) undefined)
   (((procedure? proc) (textual? textual) (integer? start)) undefined)
   (((procedure? proc) (textual? textual) (integer? start) (integer? end))
    undefined))
  (subsigs (proc (lambda ((char? char)) undefined)))
  (desc . "Calls proc on each valid index of the specified subtext or substring, in increasing order, discarding the results of those calls. This is simply a safe and correct way to loop over a subtext or substring."))
 ((name . "textual-count")
  (signature
   case-lambda
   (((textual? textual) (procedure? pred)) integer?)
   (((textual? textual) (procedure? pred) (integer? start)) integer?)
   (((textual? textual) (procedure? pred) (integer? start) (integer? end))
    integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure)
  (desc . "Returns a count of the number of characters in the specified subtext of textual that satisfy the given predicate."))
 ((group
    ((name . "textual-filter")
     (signature
       case-lambda
       (((procedure? pred) (textual? textual)) text?)
       (((procedure? pred) (textual? textual) (integer? start)) text?)
       (((procedure? pred) (textual? textual) (integer? start) (integer? end))
        text?))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "textual-remove")
     (signature
       case-lambda
       (((procedure? pred) (textual? textual)) text?)
       (((procedure? pred) (textual? textual) (integer? start)) text?)
       (((procedure? pred) (textual? textual) (integer? start) (integer? end))
        text?))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure)))
  (desc . "Filter the given subtext of textual, retaining only those characters that satisfy / do not satisfy pred.
If textual is a string, then that string does not share any storage with the result, so subsequent mutation of that string will not affect the text returned by these procedures. If textual is a text, implementations are encouraged to return a result that shares storage with that text whenever sharing would be space-efficient."))
 ((name . "textual-replicate")
  (signature
   case-lambda
   (((textual? textual) (integer? from) (integer? to)) text?)
   (((textual? textual) (integer? from) (integer? to) (integer? start)) text?)
   (((textual? textual)
     (integer? from)
     (integer? to)
     (integer? start)
     (integer? end))
    text?))
  (tags pure)
  (desc . "This is an \"extended subtext\" procedure that implements replicated copying of a subtext or substring.
textual is a text or string; start and end are optional arguments that specify a subtext of textual, defaulting to 0 and the length of textual. This subtext is conceptually replicated both up and down the index space, in both the positive and negative directions. textual-replicate returns the subtext of this text beginning at index from, and ending at to. It is an error if from is greater than to.
Note that
* The from/to arguments give a half-open range containing the characters from index from up to, but not including, index to.
* The from/to indexes are not expressed in the index space of textual. They refer instead to the replicated index space of the subtext defined by textual, start, and end.

It is an error if start=end, unless from=to, which is allowed as a special case."))
 ((name . "textual-split")
  (signature
   case-lambda
   (((textual? textual) (textual? delimiter)) list?)
   (((textual? textual) (textual? delimiter) (symbol? grammar)) list?)
   (((textual? textual)
     (textual? delimiter)
     (symbol? grammar)
     ((or #f integer?) limit))
    list?)
   (((textual? textual)
     (textual? delimiter)
     (symbol? grammar)
     ((or #f integer?) limit)
     (integer? start))
    list?)
   (((textual? textual)
     (textual? delimiter)
     (symbol? grammar)
     ((or #f integer?) limit)
     (integer? start)
     (integer? end))
    list?))
  (tags pure)
  (desc . "Returns a list of texts representing the words contained in the subtext of textual from start (inclusive) to end (exclusive). The delimiter is a text or string to be used as the word separator. This will often be a single character, but multiple characters are allowed for use cases such as splitting on \"\\r\\n\". The returned list will have one more item than the number of non-overlapping occurrences of the delimiter in the text. If delimiter is an empty text, then the returned list contains a list of texts, each of which contains a single character.
The grammar is a symbol with the same meaning as in the textual-join procedure. If it is infix, which is the default, processing is done as described above, except an empty textual produces the empty list; if grammar is strict-infix, then an empty textual signals an error. The values prefix and suffix cause a leading/trailing empty text in the result to be suppressed.
If limit is a non-negative exact integer, at most that many splits occur, and the remainder of textual is returned as the final element of the list (so the result will have at most limit+1 elements). If limit is not specified or is #f, then as many splits as possible are made. It is an error if limit is any other value.")))
