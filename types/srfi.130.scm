(((name . "string-cursor?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj can be a string cursor, and #f otherwise. In implementations where cursors and indexes are the same thing, #t is returned on any cursor or index; where they are disjoint, #t is returned on cursors, #f on indexes. If obj is neither a cursor nor an index, string-cursor? will always return #f."))
 ((group
    ((name . "string-cursor-start")
     (signature lambda ((string? s)) string-cursor?)
     (tags pure))
    ((name . "string-cursor-end")
     (signature lambda ((string? s)) string-cursor?)
     (tags pure)))
  (desc . "Returns the start/post-end cursor of s respectively."))
 ((group
    ((name . "string-cursor-next")
     (signature
       lambda
       ((string? s) ((or integer? string-cursor?) cursor))
       string-cursor?)
     (tags pure))
    ((name . "string-cursor-prev")
     (signature
       lambda
       ((string? s) ((or integer? string-cursor?) cursor))
       string-cursor?)
     (tags pure)))
  (desc . "Returns the cursor into s following/preceding cursor. If cursor is an index, returns one more/less than cursor. It is an error if cursor is the post-end/start cursor of s."))
 ((group
    ((name . "string-cursor-forward")
     (signature
       lambda
       ((string? s) ((or integer? string-cursor?) cursor) (integer? nchars))
       string-cursor?)
     (tags pure))
    ((name . "string-cursor-back")
     (signature
       lambda
       ((string? s) ((or integer? string-cursor?) cursor) (integer? nchars))
       string-cursor?)
     (tags pure)))
  (desc . "Returns the cursor into s which follows/precedes cursor by nchars characters. If cursor is an index, returns nchars more/less than cursor. It is an error if the result would be an invalid cursor or index."))
 ((group
    ((name . "string-cursor=?")
     (signature
       lambda
       (((or integer? string-cursor?) cursor1)
        ((or integer? string-cursor?) cursor2))
       boolean?)
     (tags pure))
    ((name . "string-cursor<?")
     (signature
       lambda
       (((or integer? string-cursor?) cursor1)
        ((or integer? string-cursor?) cursor2))
       boolean?)
     (tags pure))
    ((name . "string-cursor>?")
     (signature
       lambda
       (((or integer? string-cursor?) cursor1)
        ((or integer? string-cursor?) cursor2))
       boolean?)
     (tags pure))
    ((name . "string-cursor<=?")
     (signature
       lambda
       (((or integer? string-cursor?) cursor1)
        ((or integer? string-cursor?) cursor2))
       boolean?)
     (tags pure))
    ((name . "string-cursor>=?")
     (signature
       lambda
       (((or integer? string-cursor?) cursor1)
        ((or integer? string-cursor?) cursor2))
       boolean?)
     (tags pure)))
  (desc . "Compares two cursors or two indexes pointing into the same string."))
 ((name . "string-cursor-diff")
  (signature
   lambda
   ((string? s)
    ((or integer? string-cursor?) start)
    ((or integer? string-cursor?) end))
   integer?)
  (tags pure)
  (desc . "Returns the number of characters between start and end in string s. Note that the result is always non-negative if start and end are a valid start-end pair."))
 ((group
    ((name . "string-cursor->index")
     (signature lambda ((string? s) (string-cursor? cursor)) integer?)
     (tags pure))
    ((name . "string-index->cursor")
     (signature lambda ((string? s) (integer? index)) string-cursor?)
     (tags pure)))
  (desc . "Converts a cursor/index into s into the corresponding index/cursor. If the argument is already an index/cursor, it is returned unchanged."))
 ((name . "string-null?")
  (signature lambda ((string? s)) boolean?)
  (tags pure)
  (desc . "Is s the empty string?"))
 ((group
    ((name . "string-every")
     (signature
       case-lambda
       (((procedure? pred) (string? s)) *)
       (((procedure? pred) (string? s) ((or integer? string-cursor?) start)) *)
       (((procedure? pred)
         (string? s)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        *))
     (subsigs (pred (lambda ((char? c)) *)))
     (tags pure))
    ((name . "string-any")
     (signature
       case-lambda
       (((procedure? pred) (string? s)) *)
       (((procedure? pred) (string? s) ((or integer? string-cursor?) start)) *)
       (((procedure? pred)
         (string? s)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        *))
     (subsigs (pred (lambda ((char? c)) *)))
     (tags pure)))
  (desc . "Checks to see if every/any character in s satisfies pred proceeding from left (index start) to right (index end). The predicate is \"witness-generating\":
* If string-any returns true, the returned true value is the one produced by the application of the predicate.
* If string-every returns true, the returned true value is the one produced by the final application of the predicate to s[end-1]. If string-every is applied to an empty sequence of characters, it simply returns #t. 

The names of these procedures do not end with a question mark — this is to indicate that they do not return a simple boolean (#t or #f), but a general value."))
 ((name . "string-tabulate")
  (signature
   case-lambda
   (((procedure? proc) (integer? len)) string?)
   (((procedure? proc) (integer? len)) string?))
  (subsigs (proc (lambda ((integer? index)) char?)))
  (tags pure)
  (desc . "Proc is an integer → char procedure. Construct a string of size len by applying proc to each value from 0 (inclusive) to len (exclusive) to produce the corresponding string element. The order in which proc is applied to the indexes is not specified.
Note that the order of arguments is not the same as SRFI 1's list-tabulate, but is the same as tabulation functions in other SRFIs. When this discrepancy was discovered in SRFI 13, it was too late to change SRFI 1."))
 ((name . "string-unfold")
  (signature
   case-lambda
   (((procedure? stop?) (procedure? mapper) seed) string?)
   (((procedure? stop?) (procedure? mapper) seed (string? base)) string?)
   (((procedure? stop?)
     (procedure? mapper)
     seed
     (string? base)
     (procedure? make-final))
    string?))
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) char?))
   (make-final (lambda (seed) string?)))
  (tags pure)
  (desc . "This is a fundamental constructor for strings.
* Successor is used to generate a series of \"seed\" values from the initial seed: seed, (successor seed), (successor2 seed), (successor3 seed), ...
* Stop? tells us when to stop — when it returns true when applied to one of these seed values.
* Mapper maps each seed value to the corresponding character in the result string. These chars are assembled into the string in a left-to-right order.
* Base is the optional initial/leftmost portion of the constructed string; it defaults to the empty string \"\".
* Make-final is applied to the terminal seed value (on which stop? returns true) to produce the final/rightmost portion of the constructed string. It defaults to (lambda (x) \"\"). 

string-unfold is a fairly powerful string constructor — you can use it to convert a list to a string, read a port into a string, reverse a string, copy a string, and so forth."))
 ((name . "string-unfold-right")
  (signature
   case-lambda
   (((procedure? stop?) (procedure? mapper) seed) string?)
   (((procedure? stop?) (procedure? mapper) seed (string? base)) string?)
   (((procedure? stop?)
     (procedure? mapper)
     seed
     (string? base)
     (procedure? make-final))
    string?))
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) char?))
   (make-final (lambda (seed) string?)))
  (tags pure)
  (desc . "This is a fundamental constructor for strings. It is equivalent to string-unfold, except that the results of mapper are assembled into the string in a right-to-left order, base is the optional rightmost portion of the constructed string, and make-final produces the leftmost portion of the constructed string."))
 ((group
    ((name . "string->list/cursors")
     (signature
       case-lambda
       (((string? s)) list?)
       (((string? s) ((or integer? string-cursor?) start)) list?)
       (((string? s)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        list?))
     (subsigs
       (return (list (char? c))))
     (tags pure))
    ((name . "string->vector/cursors")
     (signature
       case-lambda
       (((string? s)) vector?)
       (((string? s) ((or integer? string-cursor?) start)) vector?)
       (((string? s)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        vector?))
     (subsigs
       (return (vector (char? c))))
     (tags pure)))
  (desc . "string->list/cursors and string->vector/cursors return a newly allocated list or vector of the characters that make up the given string. They differ from the R7RS procedures string->list and string->vector by accepting either cursors or indexes."))
 ((name . "reverse-list->string")
  (signature lambda ((list? char-list)) string?)
  (tags pure)
  (desc . "An efficient implementation of (compose list->string reverse):
(reverse-list->string '(#\\a #\\B #\\c)) → \"cBa\"

This is a common idiom in the epilog of string-processing loops that accumulate an answer in a reverse-order list. (See also string-concatenate-reverse for the \"chunked\" variant.)"))
 ((name . "string-join")
  (signature
   case-lambda
   (((list? string-list)) string?)
   (((list? string-list) (string? delimiter)) string?)
   (((list? string-list) (string? delimiter) (symbol? grammar)) string?))
  (tags pure)
  (desc . "This procedure is a simple unparser —- it pastes strings together using the delimiter string.
The grammar argument is a symbol that determines how the delimiter is used, and defaults to 'infix.
    'infix means an infix or separator grammar: insert the delimiter between list elements. An empty list will produce an empty string — note, however, that parsing an empty string with an infix or separator grammar is ambiguous. Is it an empty list, or a list of one element, the empty string?
    'strict-infix means the same as 'infix, but will signal an error if given an empty list.
    'suffix means a suffix or terminator grammar: insert the delimiter after every list element. This grammar has no ambiguities.
    'prefix means a prefix grammar: insert the delimiter before every list element. This grammar has no ambiguities. 

The delimiter is the string used to delimit elements; it defaults to a single space \" \"."))
 ((name . "string-ref/cursor")
  (signature lambda ((string? s) ((or integer? string-cursor?) cursor)) char?)
  (tags pure)
  (desc . "Returns character s[i] using a valid cursor or index of s. It differs from the R7RS procedure string-ref by accepting either a cursor or an index."))
 ((group
    ((name . "substring/cursors")
     (signature
       lambda
       ((string? s)
        ((or integer? string-cursor?) start)
        ((or integer? string-cursor?) end))
       string?)
     (tags pure))
    ((name . "string-copy/cursors")
     (signature
       case-lambda
       (((string? s)) string?)
       (((string? s) ((or integer? string-cursor?) start)) string?)
       (((string? s)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        string?))
     (tags pure)))
  (desc . "These procedures return a string whose contents are the characters of s beginning with index start (inclusive) and ending with index end (exclusive). If substring/cursors produces the entire string, it may return either s or a copy of s; in some implementations, proper substrings may share memory with s. However, string-copy/cursors always returns a newly allocated string. They differ from the R7RS procedures substring and string-copy by accepting either cursors or indexes."))
 ((group
    ((name . "string-take")
     (signature lambda ((string? s) (integer? nchars)) string?)
     (tags pure))
    ((name . "string-drop")
     (signature lambda ((string? s) (integer? nchars)) string?)
     (tags pure))
    ((name . "string-take-right")
     (signature lambda ((string? s) (integer? nchars)) string?)
     (tags pure))
    ((name . "string-drop-right")
     (signature lambda ((string? s) (integer? nchars)) string?)
     (tags pure)))
  (desc . "string-take returns the first nchars of s; string-drop returns all but the first nchars of s. string-take-right returns the last nchars of s; string-drop-right returns all but the last nchars of s. If these procedures produce the entire string, they may return either s or a copy of s; in some implementations, proper substrings may share memory with s."))
 ((group
    ((name . "string-pad")
     (signature
       case-lambda
       (((string? s) (integer? len)) string?)
       (((string? s) (integer? len) (char? char)) string?)
       (((string? s)
         (integer? len)
         (char? char)
         ((or integer? string-cursor?) start))
        string?)
       (((string? s)
         (integer? len)
         (char? char)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        string?))
     (tags pure))
    ((name . "string-pad-right")
     (signature
       case-lambda
       (((string? s) (integer? len)) string?)
       (((string? s) (integer? len) (char? char)) string?)
       (((string? s)
         (integer? len)
         (char? char)
         ((or integer? string-cursor?) start))
        string?)
       (((string? s)
         (integer? len)
         (char? char)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        string?))
     (tags pure)))
  (desc . "Build a string of length len comprised of s padded on the left (right) by as many occurrences of the character char as needed. If s has more than len chars, it is truncated on the left (right) to length len. Char defaults to #\\space.
If len <= end-start, the returned value is allowed to share storage with s, or be exactly s (if len = end-start)."))
 ((group
    ((name . "string-trim")
     (signature
       case-lambda
       (((string? s)) string?)
       (((string? s) (procedure? pred)) string?)
       (((string? s) (procedure? pred) ((or integer? string-cursor?) start))
        string?)
       (((string? s)
         (procedure? pred)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        string?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure))
    ((name . "string-trim-right")
     (signature
       case-lambda
       (((string? s)) string?)
       (((string? s) (procedure? pred)) string?)
       (((string? s) (procedure? pred) ((or integer? string-cursor?) start))
        string?)
       (((string? s)
         (procedure? pred)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        string?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure))
    ((name . "string-trim-both")
     (signature
       case-lambda
       (((string? s)) string?)
       (((string? s) (procedure? pred)) string?)
       (((string? s) (procedure? pred) ((or integer? string-cursor?) start))
        string?)
       (((string? s)
         (procedure? pred)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        string?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure)))
  (desc . "Trim s by skipping over all characters on the left / on the right / on both sides that satisfy the second parameter pred: pred defaults to char-whitespace?.
If no trimming occurs, these functions may return either s or a copy of s; in some implementations, proper substrings may share memory with s."))
 ((group
    ((name . "string-prefix-length")
     (signature
       case-lambda
       (((string? s1) (string? s2)) integer?)
       (((string? s1) (string? s2) ((or integer? string-cursor?) start1)) integer?)
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1))
        integer?)
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1)
         ((or integer? string-cursor?) start2))
        integer?)
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1)
         ((or integer? string-cursor?) start2)
         ((or integer? string-cursor?) end2))
        integer?))
     (tags pure))
    ((name . "string-suffix-length")
     (signature
       case-lambda
       (((string? s1) (string? s2)) integer?)
       (((string? s1) (string? s2) ((or integer? string-cursor?) start1)) integer?)
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1))
        integer?)
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1)
         ((or integer? string-cursor?) start2))
        integer?)
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1)
         ((or integer? string-cursor?) start2)
         ((or integer? string-cursor?) end2))
        integer?))
     (tags pure)))
  (desc . "Return the length of the longest common prefix/suffix of the two strings. For prefixes, this is equivalent to the \"mismatch index\" for the strings (modulo the start cursors).
The optional start/end cursors or indexes restrict the comparison to the indicated substrings of s1 and s2."))
 ((group
    ((name . "string-prefix?")
     (signature
       case-lambda
       (((string? s1) (string? s2)) boolean?)
       (((string? s1) (string? s2) ((or integer? string-cursor?) start1)) boolean?)
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1))
        boolean?)
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1)
         ((or integer? string-cursor?) start2))
        boolean?)
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1)
         ((or integer? string-cursor?) start2)
         ((or integer? string-cursor?) end2))
        boolean?))
     (tags pure))
    ((name . "string-suffix?")
     (signature
       case-lambda
       (((string? s1) (string? s2)) boolean?)
       (((string? s1) (string? s2) ((or integer? string-cursor?) start1)) boolean?)
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1))
        boolean?)
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1)
         ((or integer? string-cursor?) start2))
        boolean?)
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1)
         ((or integer? string-cursor?) start2)
         ((or integer? string-cursor?) end2))
        boolean?))
     (tags pure)))
  (desc . "Is s1 a prefix/suffix of s2?
The optional start/end cursors or indexes restrict the comparison to the indicated substrings of s1 and s2."))
 ((group
    ((name . "string-index")
     (signature
       case-lambda
       (((string? s) (procedure? pred)) string-cursor?)
       (((string? s) (procedure? pred) ((or integer? string-cursor?) start))
        string-cursor?)
       (((string? s)
         (procedure? pred)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        string-cursor?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure))
    ((name . "string-index-right")
     (signature
       case-lambda
       (((string? s) (procedure? pred)) string-cursor?)
       (((string? s) (procedure? pred) ((or integer? string-cursor?) start))
        string-cursor?)
       (((string? s)
         (procedure? pred)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        string-cursor?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure))
    ((name . "string-skip")
     (signature
       case-lambda
       (((string? s) (procedure? pred)) string-cursor?)
       (((string? s) (procedure? pred) ((or integer? string-cursor?) start))
        string-cursor?)
       (((string? s)
         (procedure? pred)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        string-cursor?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure))
    ((name . "string-skip-right")
     (signature
       case-lambda
       (((string? s) (procedure? pred)) string-cursor?)
       (((string? s) (procedure? pred) ((or integer? string-cursor?) start))
        string-cursor?)
       (((string? s)
         (procedure? pred)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        string-cursor?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure)))
  (desc . "string-index searches through s from the left, returning the cursor of the first occurrence of a character which satisfies the predicate pred. If no match is found, it returns end. string-index-right searches through s from the right, returning the cursor of the successor of the first occurrence of a character which satisfies the predicate pred. If no match is found, it returns start.
The start and end parameters specify the beginning and end cursors or indexes of the search; the search includes the start, but not the end. Be careful of \"fencepost\" considerations: when searching right-to-left, the first position considered is (string-cursor-prev end), whereas when searching left-to-right, the first index considered is start. That is, the start/end indexes describe the same half-open interval [start,end) in these procedures that they do in all the other SRFI 130 procedures.
The skip functions are similar, but use the complement of the criteria: they search for the first char that doesn't satisfy pred. E.g., to skip over initial whitespace, say
(substring/cursors s (string-skip s char-whitespace?))

Note that the result is always a cursor, even when start and end are indexes. Use string-cursor->index to convert the result to an index. Therefore, these four functions are not entirely compatible with their SRFI 13 counterparts, which return #f on failure.
These functions can be trivially composed with string-take and string-drop to produce take-while, drop-while, span, and break procedures without loss of efficiency."))
 ((group
    ((name . "string-contains")
     (signature
       case-lambda
       (((string? s1) (string? s2)) (or #f string-cursor?))
       (((string? s1) (string? s2) ((or integer? string-cursor?) start1))
        (or #f string-cursor?))
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1))
        (or #f string-cursor?))
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1)
         ((or integer? string-cursor?) start2))
        (or #f string-cursor?))
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1)
         ((or integer? string-cursor?) start2)
         ((or integer? string-cursor?) end2))
        (or #f string-cursor?)))
     (tags pure))
    ((name . "string-contains-right")
     (signature
       case-lambda
       (((string? s1) (string? s2)) (or #f string-cursor?))
       (((string? s1) (string? s2) ((or integer? string-cursor?) start1))
        (or #f string-cursor?))
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1))
        (or #f string-cursor?))
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1)
         ((or integer? string-cursor?) start2))
        (or #f string-cursor?))
       (((string? s1)
         (string? s2)
         ((or integer? string-cursor?) start1)
         ((or integer? string-cursor?) end1)
         ((or integer? string-cursor?) start2)
         ((or integer? string-cursor?) end2))
        (or #f string-cursor?)))
     (tags pure)))
  (desc . "Does string s1 contain string s2?
Returns the cursor in s1 referring to the first character of the first/last instance of s2 as a substring, or #f if there is no match. The optional start/end indexes restrict the operation to the indicated substrings.
The returned cursor is in the range [start1,end1). A successful match must lie entirely in the [start1,end1) range of s1.
Note that the result is always a cursor, even when start1 and end1 are indexes.
Use string-cursor->index to convert a cursor result to an index.
(string-contains \"eek -- what a geek.\" \"ee\"
                 12 18) ; Searches \"a geek\"
    => {Cursor 15}

The name of this procedure does not end with a question mark — this is to indicate that it does not return a simple boolean (#t or #f). Rather, it returns either false (#f) or a cursor."))
 ((name . "string-reverse")
  (signature
   case-lambda
   (((string? s)) string?)
   (((string? s) ((or integer? string-cursor?) start)) string?)
   (((string? s)
     ((or integer? string-cursor?) start)
     ((or integer? string-cursor?) end))
    string?))
  (tags pure)
  (desc . "Reverse the string."))
 ((name . "string-concatenate")
  (signature lambda ((list? string-list)) string?)
  (tags pure)
  (desc . "Append the elements of string-list together into a single string. Guaranteed to return a freshly allocated string.
Note that the (apply string-append string-list) idiom is not robust for long lists of strings, as some Scheme implementations limit the number of arguments that may be passed to an n-ary procedure."))
 ((name . "string-concatenate-reverse")
  (signature
   case-lambda
   (((list? string-list)) string?)
   (((list? string-list) (string? final-string)) string?)
   (((list? string-list)
     (string? final-string)
     ((or integer? string-cursor?) end))
    string?))
  (tags pure)
  (desc . "With no optional arguments, this function is equivalent to
(string-concatenate (reverse string-list))

If the optional argument final-string is specified, it is consed onto the beginning of string-list before performing the list-reverse and string-concatenate operations.
If the optional argument end is given, only the characters up to but not including end in final-string are added to the result."))
 ((name . "string-fold")
  (signature
   case-lambda
   (((procedure? kons) knil (string? s)) *)
   (((procedure? kons) knil (string? s) ((or integer? string-cursor?) start))
    *)
   (((procedure? kons)
     knil
     (string? s)
     ((or integer? string-cursor?) start)
     ((or integer? string-cursor?) end))
    *))
  (subsigs (kons (lambda ((char? c) state) *)))
  (tags pure)
  (desc . "The left-fold operator maps the kons procedure across the string from left to right
(... (kons s[2] (kons s[1] (kons s[0] knil))))"))
 ((name . "string-fold-right")
  (signature
   case-lambda
   (((procedure? kons) knil (string? s)) *)
   (((procedure? kons) knil (string? s) ((or integer? string-cursor?) start))
    *)
   (((procedure? kons)
     knil
     (string? s)
     ((or integer? string-cursor?) start)
     ((or integer? string-cursor?) end))
    *))
  (subsigs (kons (lambda ((char? c) state) *)))
  (tags pure)
  (desc . "The right-fold operator maps the kons procedure across the string from right to left
(kons s[0] (... (kons s[end-3] (kons s[end-2] (kons s[end-1] knil)))))"))
 ((name . "string-for-each-cursor")
  (signature
   case-lambda
   (((procedure? proc) (string? s)) undefined)
   (((procedure? proc) (string? s) ((or integer? string-cursor?) start))
    undefined)
   (((procedure? proc)
     (string? s)
     ((or integer? string-cursor?) start)
     ((or integer? string-cursor?) end))
    undefined))
  (subsigs (proc (lambda ((string-cursor? cursor)) undefined)))
  (desc . "Apply proc to each cursor of s, in order, excluding the post-end cursor. The optional start/end pairs restrict the endpoints of the loop. This is simply a method of looping over a string that is guaranteed to be safe and correct."))
 ((name . "string-replicate")
  (signature
   case-lambda
   (((string? s) (integer? from) (integer? to)) string?)
   (((string? s)
     (integer? from)
     (integer? to)
     ((or integer? string-cursor?) start))
    string?)
   (((string? s)
     (integer? from)
     (integer? to)
     ((or integer? string-cursor?) start)
     ((or integer? string-cursor?) end))
    string?))
  (tags pure)
  (desc . "
This is an \"extended substring\" procedure that implements replicated copying of a substring of some string.
S is a string; start and end are optional arguments that demarcate a substring of s, defaulting to 0 and the length of s (i.e., the whole string). Replicate this substring up and down index space, in both the positive and negative directions.
string-replicate returns the substring of this string beginning at index from, and ending at to. Note that these arguments cannot be cursors. It is an error if from is greater than to.
Note that
* The from/to indexes give a half-open range — the characters from index from up to, but not including, index to.
* The from/to indexes are not in terms of the index space for string s. They are in terms of the replicated index space of the substring defined by s, start, and end. 

It is an error if start=end — although this is allowed by special dispensation when from=to."))
 ((name . "string-count")
  (signature
   case-lambda
   (((string? s) (procedure? pred)) integer?)
   (((string? s) (procedure? pred) ((or integer? string-cursor?) start))
    integer?)
   (((string? s)
     (procedure? pred)
     ((or integer? string-cursor?) start)
     ((or integer? string-cursor?) end))
    integer?))
  (subsigs (pred (lambda ((char? c)) boolean?)))
  (tags pure)
  (desc . "Return a count of the number of characters in s that satisfy the pred argument."))
 ((name . "string-replace")
  (signature
   case-lambda
   (((string? s1)
     (string? s2)
     ((or integer? string-cursor?) start1)
     ((or integer? string-cursor?) end1))
    string?)
   (((string? s1)
     (string? s2)
     ((or integer? string-cursor?) start1)
     ((or integer? string-cursor?) end1)
     ((or integer? string-cursor?) start2))
    string?)
   (((string? s1)
     (string? s2)
     ((or integer? string-cursor?) start1)
     ((or integer? string-cursor?) end1)
     ((or integer? string-cursor?) start2)
     ((or integer? string-cursor?) end2))
    string?))
  (tags pure)
  (desc . "The segment of characters in s1 from start1 to end1 is replaced by the segment of characters in s2 from start2 to end2. If start1=end1, this simply splices the s2 characters into s1 at the specified index."))
 ((name . "string-split")
  (signature
   case-lambda
   (((string? s) (string? delimiter)) list?)
   (((string? s) (string? delimiter) (symbol? grammar)) list?)
   (((string? s)
     (string? delimiter)
     (symbol? grammar)
     ((or integer? #f) limit))
    list?)
   (((string? s)
     (string? delimiter)
     (symbol? grammar)
     ((or integer? #f) limit)
     ((or integer? string-cursor?) start))
    list?)
   (((string? s)
     (string? delimiter)
     (symbol? grammar)
     ((or integer? #f) limit)
     ((or integer? string-cursor?) start)
     ((or integer? string-cursor?) end))
    list?))
  (tags pure)
  (desc . "Returns a list of the words contained in the substring of string from start (inclusive) to end (exclusive). Delimiter specifies a string that is to be used as the word separator. This will often be a single character, but multiple characters are allowed for cases like splitting on \"\\r\\n\". The returned list will then have one more item than the number of non-overlapping occurrences of the delimiter in the string. If delimiter is an empty string, then the returned list contains a list of strings, each of which contains a single character.
Grammar is a symbol with the same meaning as in the string-join procedure. If it is infix, which is the default, processing is done as described above, except that an empty s produces the empty list; if it is strict-infix, an empty s signals an error. The values prefix and suffix cause a leading/trailing empty string in the result to be suppressed.
If limit is a non-negative exact integer, at most that many splits occur, and the remainder of string is returned as the final element of the list (thus, the result will have at most limit+1 elements). If limit is not specified or is #f, then as many splits as possible are made. It is an error if limit is any other value.
Use SRFI 115's regexp-split to split on a regular expression rather than a simple string."))
 ((group
    ((name . "string-filter")
     (signature
       case-lambda
       (((procedure? pred) (string? s)) string?)
       (((procedure? pred) (string? s) ((or integer? string-cursor?) start))
        string?)
       (((procedure? pred)
         (string? s)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        string?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure))
    ((name . "string-remove")
     (signature
       case-lambda
       (((procedure? pred) (string? s)) string?)
       (((procedure? pred) (string? s) ((or integer? string-cursor?) start))
        string?)
       (((procedure? pred)
         (string? s)
         ((or integer? string-cursor?) start)
         ((or integer? string-cursor?) end))
        string?))
     (subsigs (pred (lambda ((char? c)) boolean?)))
     (tags pure)))
  (desc . "Filter the string s, retaining only those characters that satisfy / do not satisfy pred.
If the string is unaltered by the filtering operation, these functions may return either s or a copy of s.
Compatibility note: string-remove is identical to the string-delete procedure of SRFI 13, but the name string-delete is inconsistent with the conventions of SRFI 1 and other SRFIs.")))
