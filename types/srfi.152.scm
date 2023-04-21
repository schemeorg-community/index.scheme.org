(((name . "string?")
  (signature case-lambda ((obj) boolean?) ((obj) boolean?))
  (tags pure predicate)
  (desc . "Is obj a string?"))
 ((name . "string-null?")
  (signature lambda ((string? string)) boolean?)
  (tags pure)
  (desc . "Is string the empty string?"))
 ((group
    ((name . "string-every")
     (signature
       case-lambda
       (((procedure? pred) (string? string)) *)
       (((procedure? pred) (string? string) (integer? start)) *)
       (((procedure? pred) (string? string) (integer? start) (integer? end)) *))
     (subsigs (pred (lambda ((char? char)) *)))
     (tags pure))
    ((name . "string-any")
     (signature
       case-lambda
       (((procedure? pred) (string? string)) *)
       (((procedure? pred) (string? string) (integer? start)) *)
       (((procedure? pred) (string? string) (integer? start) (integer? end)) *))
     (subsigs (pred (lambda ((char? char)) *)))
     (tags pure)))
  (desc . "Checks to see if every/any character in string satisfies pred, proceeding from left (index start) to right (index end). These procedures are short-circuiting: if pred returns false, string-every does not call pred on subsequent characters; if pred returns true, string-any does not call pred on subsequent characters; Both procedures are \"witness-generating\":
* If string-every is given an empty interval (with start = end), it returns #t.
* If string-every returns true for a non-empty interval (with start < end), the returned true value is the one returned by the final call to the predicate on (string-ref (string-copy string) (- end 1)).
* If string-any returns true, the returned true value is the one returned by the predicate."))
 ((name . "make-string")
  (signature lambda ((integer? len) (char? char)) string?)
  (tags pure)
  (desc . "Returns a string of the given length filled with the given character."))
 ((name . "string") 
  (signature lambda ((char? char) ...) string?)
  (tags pure)
  (desc . "Returns a string consisting of the given characters."))
 ((name . "string-tabulate")
  (signature lambda ((procedure? proc) (integer? len)) string?)
  (subsigs (proc (lambda ((integer? k)) char?)))
  (tags pure)
  (desc . "Proc is a procedure that accepts an exact integer as its argument and returns a character. Constructs a string of size len by calling proc on each value from 0 (inclusive) to len (exclusive) to produce the corresponding element of the string. The order in which proc is called on those indexes is not specified."))
 ((name . "string-unfold")
  (signature
   case-lambda
   (((procedure? stop?) (procedure? mapper) (procedure? successor) seed)
    string?)
   (((procedure? stop?)
     (procedure? mapper)
     (procedure? successor)
     seed
     (string? base))
    string?)
   (((procedure? stop?)
     (procedure? mapper)
     (procedure? successor)
     seed
     (string? base)
     (procedure? make-final))
    string?))
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string?)))
   (success (lambda (seed) *))
   (make-final (lambda (seed) (or char? string?))))
  (tags pure)
  (desc . "This is a fundamental constructor for strings.
* successor is used to generate a series of \"seed\" values from the initial seed:
* seed, (successor seed), (successor2 seed), (successor3 seed), ...
* stop? tells us when to stop — when it returns true when applied to one of these seed values.
* mapper maps each seed value to the corresponding character(s) in the result string, which are assembled into that string in left-to-right order. It is an error for mapper to return anything other than a character or string.
* base is the optional initial/leftmost portion of the constructed string, which defaults to the empty string \"\". It is an error if base is anything other than a character or string.
* make-final is applied to the terminal seed value (on which stop? returns true) to produce the final/rightmost portion of the constructed string. It defaults to (lambda (x) \"\"). It is an error for make-final to return anything other than a character or string."))
 ((name . "string-unfold-right")
  (signature
   case-lambda
   (((procedure? stop?) (procedure? mapper) (procedure? successor) seed)
    string?)
   (((procedure? stop?)
     (procedure? mapper)
     (procedure? successor)
     seed
     (string? base))
    string?)
   (((procedure? stop?)
     (procedure? mapper)
     (procedure? successor)
     seed
     (string? base)
     (procedure? make-final))
    string?))
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string?)))
   (success (lambda (seed) *))
   (make-final (lambda (seed) (or char? string?))))
  (tags pure)
  (desc . "This is a fundamental constructor for strings. It is the same as string-unfold except the results of mapper are assembled into the string in right-to-left order, base is the optional rightmost portion of the constructed string, and make-final produces the leftmost portion of the constructed string. If mapper returns a string, the string is prepended to the constructed string (without reversal)."))
 ((group
    ((name . "string->vector")
     (signature
       case-lambda
       (((string? string)) vector?)
       (((string? string) (integer? start)) vector?)
       (((string? string) (integer? start) (integer? end)) vector?))
     (subsigs
       (return (vector char?)))
     (tags pure))
    ((name . "string->list")
     (signature
       case-lambda
       (((string? string)) list?)
       (((string? string) (integer? start)) list?)
       (((string? string) (integer? start) (integer? end)) list?))
     (subsigs
       (return (list char?)))
     (tags pure)))
  (desc . "These procedures return a newly allocated (unless empty) vector or list of the characters that make up the given substring."))
 ((group
    ((name . "vector->string")
     (signature
       case-lambda
       (((vector? vector)) string?)
       (((vector? vector) (integer? start)) string?)
       (((vector? vector) (integer? start) (integer? end)) string?))
     (subsigs
       (vector (vector char?)))
     (tags pure))
    ((name . "list->string")
     (signature lambda ((list? list)) string?)
     (subsigs
       (list (list char?)))
     (tags pure)))
  (desc . "These procedures return a string containing the characters of the given (sub)vector or list. The behavior of the string will not be affected by subsequent mutation of the given vector or list."))
 ((name . "reverse-list->string")
  (signature lambda ((list? char-list)) string?)
  (subsigs (char-list (list char?)))
  (tags pure)
  (desc . "Semantically equivalent to (compose list->string reverse):
(reverse-list->string '(#\\a #\\B #\\c)) => \"cBa\"

This is a common idiom in the epilogue of string-processing loops that accumulate their result using a list in reverse order. (See also string-concatenate-reverse for the \"chunked\" variant.)"))
 ((name . "string-length")
  (signature lambda ((string? string)) integer?)
  (tags pure)
  (desc . "Returns the number of characters within the given string."))
 ((name . "string-ref")
  (signature lambda ((string? string) (integer? idx)) char?)
  (tags pure)
  (desc . "Returns character string[idx], using 0-origin indexing."))
 ((group
    ((name . "substring")
     (signature lambda ((string? string) (integer? start) (integer? end)) string?)
     (tags pure))
    ((name . "string-copy")
     (signature
       case-lambda
       (((string? string)) string?)
       (((string? string) (integer? start)) string?)
       (((string? string) (integer? start) (integer? end)) string?))
     (tags pure)))
  (desc . "These procedures return a string containing the characters of string beginning with index start (inclusive) and ending with index end (exclusive). The only difference is that substring requires all three arguments, whereas string-copy requires only one."))
 ((group
    ((name . "string-take")
     (signature lambda ((string? string) (integer? nchars)) string?)
     (tags pure))
    ((name . "string-drop")
     (signature lambda ((string? string) (integer? nchars)) string?)
     (tags pure))
    ((name . "string-take-right")
     (signature lambda ((string? string) (integer? nchars)) string?)
     (tags pure))
    ((name . "string-drop-right")
     (signature lambda ((string? string) (integer? nchars)) string?)
     (tags pure)))
  (desc . "string-take returns a string containing the first nchars of string; string-drop returns a string containing all but the first nchars of string. string-take-right returns a string containing the last nchars of string; string-drop-right returns a string containing all but the last nchars of string."))
 ((group
    ((name . "string-pad")
     (signature
       case-lambda
       (((string? string) (integer? len)) string?)
       (((string? string) (integer? len) (char? char)) string?)
       (((string? string) (integer? len) (char? char) (integer? start)) string?)
       (((string? string)
         (integer? len)
         (char? char)
         (integer? start)
         (integer? end))
        string?))
     (tags pure))
    ((name . "string-pad-right")
     (signature
       case-lambda
       (((string? string) (integer? len)) string?)
       (((string? string) (integer? len) (char? char)) string?)
       (((string? string) (integer? len) (char? char) (integer? start)) string?)
       (((string? string)
         (integer? len)
         (char? char)
         (integer? start)
         (integer? end))
        string?))
     (tags pure)))
  (desc . "Returns a string of length len comprised of the characters drawn from the given subrange of string, padded on the left (right) by as many occurrences of the character char as needed. If string has more than len chars, it is truncated on the left (right) to length len. char defaults to #\\space."))
 ((group
    ((name . "string-trim")
     (signature
       case-lambda
       (((string? string)) string?)
       (((string? string) (procedure? pred)) string?)
       (((string? string) (procedure? pred) (integer? start)) string?)
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        string?))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "string-trim-right")
     (signature
       case-lambda
       (((string? string)) string?)
       (((string? string) (procedure? pred)) string?)
       (((string? string) (procedure? pred) (integer? start)) string?)
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        string?))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "string-trim-both")
     (signature
       case-lambda
       (((string? string)) string?)
       (((string? string) (procedure? pred)) string?)
       (((string? string) (procedure? pred) (integer? start)) string?)
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        string?))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure)))
  (desc . "Returns a string obtained from the given subrange of string by skipping over all characters on the left side / on the right side / on both sides that satisfy the second argument pred: pred defaults to char-whitespace?."))
 ((name . "string-replace")
  (signature
   case-lambda
   (((string? string1) (string? string2) (integer? start1) (integer? end1))
    string?)
   (((string? string1)
     (string? string2)
     (integer? start1)
     (integer? end1)
     (integer? start2))
    string?)
   (((string? string1)
     (string? string2)
     (integer? start1)
     (integer? end1)
     (integer? start2)
     (integer? end2))
    string?))
  (tags pure)
  (desc . "Returns
(string-append (substring string1 0 start1)
                (substring string2 start2 end2)
                (substring string1 end1 (string-length string1)))

That is, the segment of characters in string1 from start1 to end1 is replaced by the segment of characters in string2 from start2 to end2. If start1=end1, this simply splices the characters drawn from string2 into string1 at that position."))
 ((name . "string=?")
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure)
  (desc . "Returns #t if all the strings have the same length and contain exactly the same characters in the same positions; otherwise returns #f."))
 ((group
    ((name . "string<?")
     (signature
       lambda
       ((string? string1) (string? string2) (string? string3) ...)
       boolean?)
     (tags pure))
    ((name . "string>?")
     (signature
       lambda
       ((string? string1) (string? string2) (string? string3) ...)
       boolean?)
     (tags pure))
    ((name . "string<=?")
     (signature
       lambda
       ((string? string1) (string? string2) (string? string3) ...)
       boolean?)
     (tags pure))
    ((name . "string>=?")
     (signature
       lambda
       ((string? string1) (string? string2) (string? string3) ...)
       boolean?)
     (tags pure)))
  (desc . "These procedures return #t if their arguments are (respectively): monotonically increasing, monotonically decreasing, monotonically non-decreasing, or monotonically non-increasing.
These comparison predicates are required to be transitive.
These procedures compare strings in an implementation-defined way. One approach is to make them the lexicographic extensions to strings of the corresponding orderings on characters. In that case, string<? would be the lexicographic ordering on strings induced by the ordering char<? on characters, and if two strings differ in length but are the same up to the length of the shorter string, the shorter string would be considered to be lexicographically less than the longer string. However, implementations are also allowed to use more sophisticated locale-specific orderings.
In all cases, a pair of strings must satisfy exactly one of string<?, string=?, and string>?, must satisfy string<=? if and only if they do not satisfy string>?, and must satisfy string>=? if and only if they do not satisfy string<?."))
 ((name . "string-ci=?")
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure)
  (desc . "Returns #t if, after calling string-foldcase on each of the arguments, all of the case-folded strings would have the same length and contain the same characters in the same positions; otherwise returns #f."))
 ((group
    ((name . "string-ci<?")
     (signature
       lambda
       ((string? string1) (string? string2) (string? string3) ...)
       boolean?)
     (tags pure))
    ((name . "string-ci>?")
     (signature
       lambda
       ((string? string1) (string? string2) (string? string3) ...)
       boolean?)
     (tags pure))
    ((name . "string-ci<=?")
     (signature
       lambda
       ((string? string1) (string? string2) (string? string3) ...)
       boolean?)
     (tags pure))
    ((name . "string-ci>=?")
     (signature
       lambda
       ((string? string1) (string? string2) (string? string3) ...)
       boolean?)
     (tags pure)))
  (desc . "These procedures behave as though they had called string-foldcase on their arguments before applying the corresponding procedures without \"-ci\"."))
 ((group
    ((name . "string-prefix-length")
     (signature
       case-lambda
       (((string? string1) (string? string2)) integer?)
       (((string? string1) (string? string2) (integer? start1)) integer?)
       (((string? string1) (string? string2) (integer? start1) (integer? end1))
        integer?)
       (((string? string1)
         (string? string2)
         (integer? start1)
         (integer? end1)
         (integer? start2))
        integer?)
       (((string? string1)
         (string? string2)
         (integer? start1)
         (integer? end1)
         (integer? start2)
         (integer? end2))
        integer?))
     (tags pure))
    ((name . "string-suffix-length")
     (signature
       case-lambda
       (((string? string1) (string? string2)) integer?)
       (((string? string1) (string? string2) (integer? start1)) integer?)
       (((string? string1) (string? string2) (integer? start1) (integer? end1))
        integer?)
       (((string? string1)
         (string? string2)
         (integer? start1)
         (integer? end1)
         (integer? start2))
        integer?)
       (((string? string1)
         (string? string2)
         (integer? start1)
         (integer? end1)
         (integer? start2)
         (integer? end2))
        integer?))
     (tags pure)))
  (desc . "Return the length of the longest common prefix/suffix of string1 and string2. For prefixes, this is equivalent to their \"mismatch index\" (relative to the start indexes).
The optional start/end indexes restrict the comparison to the indicated substrings of string1 and string2."))
 ((group
    ((name . "string-prefix?")
     (signature
       case-lambda
       (((string? string1) (string? string2)) boolean?)
       (((string? string1) (string? string2) (integer? start1)) boolean?)
       (((string? string1) (string? string2) (integer? start1) (integer? end1))
        boolean?)
       (((string? string1)
         (string? string2)
         (integer? start1)
         (integer? end1)
         (integer? start2))
        boolean?)
       (((string? string1)
         (string? string2)
         (integer? start1)
         (integer? end1)
         (integer? start2)
         (integer? end2))
        boolean?))
     (tags pure))
    ((name . "string-suffix?")
     (signature
       case-lambda
       (((string? string1) (string? string2)) boolean?)
       (((string? string1) (string? string2) (integer? start1)) boolean?)
       (((string? string1) (string? string2) (integer? start1) (integer? end1))
        boolean?)
       (((string? string1)
         (string? string2)
         (integer? start1)
         (integer? end1)
         (integer? start2))
        boolean?)
       (((string? string1)
         (string? string2)
         (integer? start1)
         (integer? end1)
         (integer? start2)
         (integer? end2))
        boolean?))
     (tags pure)))
  (desc . "Is string1 a prefix/suffix of string2?
The optional start/end indexes restrict the comparison to the indicated substrings of string1 and string2."))
 ((group
    ((name . "string-index")
     (signature
       case-lambda
       (((string? string) (procedure? pred)) (or #f integer?))
       (((string? string) (procedure? pred) (integer? start)) (or #f integer?))
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        (or #f integer?)))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "string-index-right")
     (signature
       case-lambda
       (((string? string) (procedure? pred)) (or #f integer?))
       (((string? string) (procedure? pred) (integer? start)) (or #f integer?))
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        (or #f integer?)))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "string-skip")
     (signature
       case-lambda
       (((string? string) (procedure? pred)) (or #f integer?))
       (((string? string) (procedure? pred) (integer? start)) (or #f integer?))
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        (or #f integer?)))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "string-skip-right")
     (signature
       case-lambda
       (((string? string) (procedure? pred)) (or #f integer?))
       (((string? string) (procedure? pred) (integer? start)) (or #f integer?))
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        (or #f integer?)))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure)))
  (desc . "string-index searches through the given substring from the left, returning the index of the leftmost character satisfying the predicate pred. string-index-right searches from the right, returning the index of the rightmost character satisfying the predicate pred. If no match is found, these procedures return #f.
The start and end arguments specify the beginning and end of the search; the valid indexes relevant to the search include start but exclude end. Beware of \"fencepost\" errors: when searching right-to-left, the first index considered is (- end 1), whereas when searching left-to-right, the first index considered is start. That is, the start/end indexes describe the same half-open interval [start,end) in these procedures that they do in all other procedures specified by this SRFI.
The skip functions are similar, but use the complement of the criterion: they search for the first char that doesn't satisfy pred."))
 ((group
    ((name . "string-contains")
     (signature
       case-lambda
       (((string? string1) (string? string2)) (or #f integer?))
       (((string? string1) (string? string2) (integer? start1)) (or #f integer?))
       (((string? string1) (string? string2) (integer? start1) (integer? end1))
        (or #f integer?))
       (((string? string1)
         (string? string2)
         (integer? start1)
         (integer? end1)
         (integer? start2))
        (or #f integer?))
       (((string? string1)
         (string? string2)
         (integer? start1)
         (integer? end1)
         (integer? start2)
         (integer? end2))
        (or #f integer?)))
     (tags pure))
    ((name . "string-contains-right")
     (signature
       case-lambda
       (((string? string1) (string? string2)) (or #f integer?))
       (((string? string1) (string? string2) (integer? start1)) (or #f integer?))
       (((string? string1) (string? string2) (integer? start1) (integer? end1))
        (or #f integer?))
       (((string? string1)
         (string? string2)
         (integer? start1)
         (integer? end1)
         (integer? start2))
        (or #f integer?))
       (((string? string1)
         (string? string2)
         (integer? start1)
         (integer? end1)
         (integer? start2)
         (integer? end2))
        (or #f integer?)))
     (tags pure)))
  (desc . "Does the substring of string1 specified by start1 and end1 contain the sequence of characters given by the substring of string2 specified by start2 and end2?
Returns #f if there is no match. If start2 = end2, string-contains returns start1 but string-contains-right returns end1. Otherwise returns the index in string1 for the first character of the first/last match; that index lies within the half-open interval [start1,end1), and the match lies entirely within the [start1,end1) range of string1."))
 ((group
    ((name . "string-take-while")
     (signature
       case-lambda
       (((string? string) (procedure? pred)) string?)
       (((string? string) (procedure? pred) (integer? start)) string?)
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        string?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure))
    ((name . "string-take-while-right")
     (signature
       case-lambda
       (((string? string) (procedure? pred)) string?)
       (((string? string) (procedure? pred) (integer? start)) string?)
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        string?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure)))
  (desc . "Returns the longest initial prefix/suffix of the substring of string specified by start and end whose elements all satisfy the predicate pred. (Not SRFI 13 procedures.)"))
 ((group
    ((name . "string-drop-while")
     (signature
       case-lambda
       (((string? string) (procedure? pred)) string?)
       (((string? string) (procedure? pred) (integer? start)) string?)
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        string?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure))
    ((name . "string-drop-while-right")
     (signature
       case-lambda
       (((string? string) (procedure? pred)) string?)
       (((string? string) (procedure? pred) (integer? start)) string?)
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        string?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure)))
  (desc . "Drops the longest initial prefix/suffix of the substring of string specified by start and end whose elements all satisfy the predicate pred, and returns the rest of the string.
These are the same as string-trim and string-trim-right, but with a different order of arguments. (Not SRFI 13 procedures.)"))
 ((group
    ((name . "string-span")
     (signature
       case-lambda
       (((string? string) (procedure? pred)) (values string? string))
       (((string? string) (procedure? pred) (integer? start))
        (values string? string))
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        (values string? string)))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure))
    ((name . "string-break")
     (signature
       case-lambda
       (((string? string) (procedure? pred)) (values string? string))
       (((string? string) (procedure? pred) (integer? start))
        (values string? string))
       (((string? string) (procedure? pred) (integer? start) (integer? end))
        (values string? string)))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure)))
  (desc . "String-span splits the substring of string specified by start and end into the longest initial prefix whose elements all satisfy pred, and the remaining tail. String-break inverts the sense of the predicate: the tail commences with the first element of the input string that satisfies the predicate. (Not SRFI 13 procedures.)
In other words: span finds the initial span of elements satisfying pred, and break breaks the string at the first element satisfying pred."))
 ((name . "string-append")
  (signature lambda ((string? string) ...) string?)
  (tags pure)
  (desc . "Returns a string whose sequence of characters is the concatenation of the sequences of characters in the given arguments."))
 ((name . "string-concatenate")
  (signature lambda ((list? string-list)) string?)
  (tags pure)
  (desc . "Concatenates the elements of string-list together into a single string."))
 ((name . "string-concatenate-reverse")
  (signature
   case-lambda
   (((list? string-list)) string?)
   (((list? string-list) (string? final-string)) string?)
   (((list? string-list) (string? final-string) (integer? end)) string?))
  (tags pure)
  (desc . "With no optional arguments, calling this procedure is equivalent to
(string-concatenate (reverse string-list))

If the optional argument final-string is specified, it is effectively consed onto the beginning of string-list before performing the list-reverse and string-concatenate operations.
If the optional argument end is given, only the characters up to but not including end in final-string are added to the result, thus producing

(string-concatenate 
  (reverse (cons (substring final-string 0 end)
                 string-list)))"))
 ((name . "string-join")
  (signature
   case-lambda
   (((list? string-list)) string?)
   (((list? string-list) (string? delimiter)) string?)
   (((list? string-list) (string? delimiter) (symbol? grammar)) string?))
  (tags pure)
  (desc . "This procedure is a simple unparser; it pastes strings together using the delimiter string.
string-list is a list of strings. delimiter is a string. The grammar argument is a symbol that determines how the delimiter is used, and defaults to 'infix. It is an error for grammar to be any symbol other than these four:
    'infix means an infix or separator grammar: insert the delimiter between list elements. An empty list will produce an empty string.
    'strict-infix means the same as 'infix if the string-list is non-empty, but will signal an error if given an empty list. (This avoids an ambiguity shown in the examples below.)
    'suffix means a suffix or terminator grammar: insert the delimiter after every list element.
    'prefix means a prefix grammar: insert the delimiter before every list element.

The delimiter is the string used to delimit elements; it defaults to a single space \" \"."))
 ((group
    ((name . "string-fold")
     (signature
       case-lambda
       (((procedure? kons) knil (string? string)) *)
       (((procedure? kons) knil (string? string) (integer? start)) *)
       (((procedure? kons) knil (string? string) (integer? start) (integer? end))
        *))
     (subsigs (kons (lambda ((char? char) state) *)))
     (tags pure))
    ((name . "string-fold-right")
     (signature
       case-lambda
       (((procedure? kons) knil (string? string)) *)
       (((procedure? kons) knil (string? string) (integer? start)) *)
       (((procedure? kons) knil (string? string) (integer? start) (integer? end))
        *))
     (subsigs (kons (lambda ((char? char) state) *)))
     (tags pure)))
  (desc . "These are the fundamental iterators for strings.
The string-fold procedure maps the kons procedure across the given string from left to right:
(... (kons string[2] (kons string[1] (kons string[0] knil))))

In other words, string-fold obeys the (tail) recursion
  (string-fold kons knil string start end)
= (string-fold kons (kons string[start] knil) start+1 end)

The string-fold-right procedure maps kons across the given string from right to left:
(kons string[0]
      (... (kons string[end-3]
                 (kons string[end-2]
                       (kons string[end-1]
                             knil)))))

obeying the (tail) recursion

  (string-fold-right kons knil string start end)
= (string-fold-right kons (kons string[end-1] knil) start end-1)"))
 ((name . "string-map")
  (signature
   lambda
   ((procedure? proc) (string? string1) (string? string2) ...)
   string?)
  (subsigs
   (proc (lambda ((char? char1) (char? char2) ...) (or string? char?))))
  (tags pure)
  (desc . "It is an error if proc does not accept as many arguments as the number of string arguments passed to string-map, does not accept characters as arguments, or returns a value that is not a character or string.
The string-map procedure applies proc element-wise to the characters of the string arguments, converts each value returned by proc to a string, and returns the concatenation of those strings. If more than one string argument is given and not all have the same length, then string-map terminates when the shortest string argument runs out. The dynamic order in which proc is called on the characters of the string arguments is unspecified, as is the dynamic order in which the coercions are performed. If any strings returned by proc are mutated after they have been returned and before the call to string-map has returned, then string-map returns a string with unspecified contents; the string-map procedure itself does not mutate those strings."))
 ((name . "string-for-each")
  (signature
   lambda
   ((procedure? proc) (string? string1) (string? string2) ...)
   undefined)
  (subsigs (proc (lambda ((char? char1) (char? char2) ...) undefined)))
  (desc . "It is an error if proc does not accept as many arguments as the number of string arguments passed to string-map or does not accept characters as arguments.
The string-for-each procedure applies proc element-wise to the characters of the string arguments, going from left to right. If more than one string argument is given and not all have the same length, then string-for-each terminates when the shortest string argument runs out. "))
 ((name . "string-count")
  (signature
   case-lambda
   (((string? string) (procedure? pred)) integer?)
   (((string? string) (procedure? pred) (integer? start)) integer?)
   (((string? string) (procedure? pred) (integer? start) (integer? end))
    integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure)
  (desc . "Returns a count of the number of characters in the specified substring of string that satisfy the given predicate."))
 ((group
    ((name . "string-filter")
     (signature
       case-lambda
       (((procedure? pred) (string? string)) string?)
       (((procedure? pred) (string? string) (integer? start)) string?)
       (((procedure? pred) (string? string) (integer? start) (integer? end))
        string?))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure))
    ((name . "string-remove")
     (signature
       case-lambda
       (((procedure? pred) (string? string)) string?)
       (((procedure? pred) (string? string) (integer? start)) string?)
       (((procedure? pred) (string? string) (integer? start) (integer? end))
        string?))
     (subsigs (pred (lambda ((char? char)) boolean?)))
     (tags pure)))
  (desc . "Filter the given substring of string, retaining only those characters that satisfy / do not satisfy pred.
Compatibility note: In SRFI 13, string-remove is called string-delete. This is inconsistent with SRFI 1 and other SRFIs."))
 ((name . "string-replicate")
  (signature
   case-lambda
   (((string? string) (integer? from) (integer? to)) string?)
   (((string? string) (integer? from) (integer? to) (integer? start)) string?)
   (((string? string)
     (integer? from)
     (integer? to)
     (integer? start)
     (integer? end))
    string?))
  (tags pure)
  (desc . "This is an \"extended substring\" procedure that implements replicated copying of a substring.
string is a string; start and end are optional arguments that specify a substring of string, defaulting to 0 and the length of string. This substring is conceptually replicated both up and down the index space, in both the positive and negative directions. For example, if string is \"abcdefg\", start is 3, and end is 6, then we have the conceptual bidirectionally-infinite string
    ...  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f  d  e  f  d ...
        -9 -8 -7 -6 -5 -4 -3 -2 -1  0 +1 +2 +3 +4 +5 +6 +7 +8 +9

string-replicate returns the substring of this string beginning at index from, and ending at to. It is an error if from is greater than to.
It is an error if start=end, unless from=to, which is allowed as a special case."))
 ((name . "string-segment")
  (signature lambda ((string? string) (integer? k)) list?)
  (tags pure)
  (desc . "Returns a list of strings representing the consecutive substrings of length k. The last string may be shorter than k. (Not a SRFI 13 procedure.)"))
 ((name . "string-split")
  (signature
   case-lambda
   (((string? string) (string? delimiter)) list?)
   (((string? string) (string? delimiter) (symbol? grammar)) list?)
   (((string? string)
     (string? delimiter)
     (symbol? grammar)
     ((or #f integer?) limit))
    list?)
   (((string? string)
     (string? delimiter)
     (symbol? grammar)
     ((or #f integer?) limit)
     (integer? start))
    list?)
   (((string? string)
     (string? delimiter)
     (symbol? grammar)
     ((or #f integer?) limit)
     (integer? start)
     (integer? end))
    list?))
  (tags pure)
  (desc . "Returns a list of strings representing the words contained in the substring of string from start (inclusive) to end (exclusive). The delimiter is a string to be used as the word separator. This will often be a single character, but multiple characters are allowed for use cases such as splitting on \"\r\n\". The returned list will have one more item than the number of non-overlapping occurrences of the delimiter in the string. If delimiter is an empty string, then the returned list contains a list of strings, each of which contains a single character. (Not a SRFI 13 procedure; replaces string-tokenize).
The grammar is a symbol with the same meaning as in the string-join procedure. If it is infix, which is the default, processing is done as described above, except an empty string produces the empty list; if grammar is strict-infix, then an empty string signals an error. The values prefix and suffix cause a leading/trailing empty string in the result to be suppressed.
If limit is a non-negative exact integer, at most that many splits occur, and the remainder of string is returned as the final element of the list (so the result will have at most limit+1 elements). If limit is not specified or is #f, then as many splits as possible are made. It is an error if limit is any other value.
To split on a regular expression, use SRFI 115's regexp-split procedure."))
 ((name . "read-string")
  (signature
   case-lambda
   (((integer? k)) (or eof-object? string?))
   (((integer? k) (input-port? port)) (or eof-object? string?)))
  (desc . "Reads the next k characters, or as many as are available before the end of file, from the textual input port into a newly allocated string in left-to-right order and returns the string. If no characters are available before the end of file, an end-of-file object is returned. The default port is the value of (current-input-port)."))
 ((name . "write-string")
  (signature
   case-lambda
   (((string? string)) undefined)
   (((string? string) (output-port? port)) undefined)
   (((string? string) (output-port? port) (integer? start)) undefined)
   (((string? string) (output-port? port) (integer? start) (integer? end))
    undefined))
  (desc . "Writes the characters of string from index start to index end onto textual output port port. The default port is the value of (current-output-port)."))
 ((name . "string-set!")
  (signature lambda ((string? string) (integer? k) (char? char)) undefined)
  (desc . "The string-set! procedure stores char in element k of string."))
 ((name . "string-fill!")
  (signature
   case-lambda
   (((string? string) (char? fill)) undefined)
   (((string? string) (char? fill) (integer? start)) undefined)
   (((string? string) (char? fill) (integer? start) (integer? end))
    undefined))
  (desc . "The string-fill! procedure stores fill in elements start through end of string."))
 ((name . "string-copy!")
  (signature
   case-lambda
   (((string? to) (integer? at) (string? from)) undefined)
   (((string? to) (integer? at) (string? from) (integer? start)) undefined)
   (((string? to) (integer? at) (string? from) (integer? start) (integer? end))
    undefined))
  (desc . "Copies the characters of string from between start and end to string to, starting at at. The order in which characters are copied is unspecified, except that if the source and destination overlap, copying takes place as if the source is first copied into a temporary string and then into the destination. This can be achieved without allocating storage by making sure to copy in the correct direction in such circumstances.")))
