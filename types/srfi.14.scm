(((name . "char-set?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Is the object obj a character set?"))
 ((name . "char-set=")
  (signature lambda ((char-set? cs1) ...) boolean?)
  (tags pure)
  (desc . "Are the character sets equal?
    Boundary cases:
    (char-set=) => true
    (char-set= cs) => true

    Rationale: transitive binary relations are generally extended to n-ary relations in Scheme, which enables clearer, more concise code to be written. While the zero-argument and one-argument cases will almost certainly not arise in first-order uses of such relations, they may well arise in higher-order cases or macro-generated code. E.g., consider
    (apply char-set= cset-list)

    This is well-defined if the list is empty or a singleton list. Hence we extend these relations to any number of arguments. Implementors have reported actual uses of n-ary relations in higher-order cases allowing for fewer than two arguments. The way of Scheme is to handle the general case; we provide the fully general extension.
    A counter-argument to this extension is that R5RS's transitive binary arithmetic relations (=, <, etc.) require at least two arguments, hence this decision is a break with the prior convention -- although it is at least one that is backwards-compatible."))
 ((name . "char-set<=")
  (signature lambda ((char-set? cs1) ...) boolean?)
  (tags pure)
  (desc . "Returns true if every character set csi is a subset of character set csi+1.
    Boundary cases:
    (char-set<=) => true
    (char-set<= cs) => true

    Rationale: See char-set= for discussion of zero- and one-argument applications. Consider testing a list of char-sets for monotonicity with
    (apply char-set<= cset-list)"))
 ((name . "char-set-hash")
  (signature
   case-lambda
   (((char-set? cs)) integer?)
   (((char-set? cs) (integer? bound)) integer?))
  (tags pure)
  (desc . "Compute a hash value for the character set cs. Bound is a non-negative exact integer specifying the range of the hash function. A positive value restricts the return value to the range [0,bound).
    If bound is either zero or not given, the implementation may use an implementation-specific default value, chosen to be as large as is efficiently practical. For instance, the default range might be chosen for a given implementation to map all strings into the range of integers that can be represented with a single machine word.

    Invariant:
    (char-set= cs1 cs2) => (= (char-set-hash cs1 b) (char-set-hash cs2 b))

    A legal but nonetheless discouraged implementation:
    (define (char-set-hash cs . maybe-bound) 1)

    Rationale: allowing the user to specify an explicit bound simplifies user code by removing the mod operation that typically accompanies every hash computation, and also may allow the implementation of the hash function to exploit a reduced range to efficiently compute the hash value. E.g., for small bounds, the hash function may be computed in a fashion such that intermediate values never overflow into bignum integers, allowing the implementor to provide a fixnum-specific \"fast path\" for computing the common cases very rapidly."))
 ((name . "char-set-cursor")
  (signature lambda ((char-set? cset)) *)
  (desc . "(shared description for char-set-cursor, char-set-ref, char-set-cursor-next, end-of-char-set?)
    Cursors are a low-level facility for iterating over the characters in a set. A cursor is a value that indexes a character in a char set. char-set-cursor produces a new cursor for a given char set. The set element indexed by the cursor is fetched with char-set-ref. A cursor index is incremented with char-set-cursor-next; in this way, code can step through every character in a char set. Stepping a cursor \"past the end\" of a char set produces a cursor that answers true to end-of-char-set?. It is an error to pass such a cursor to char-set-ref or to char-set-cursor-next.
    A cursor value may not be used in conjunction with a different character set; if it is passed to char-set-ref or char-set-cursor-next with a character set other than the one used to create it, the results and effects are undefined.
    Cursor values are not necessarily distinct from other types. They may be integers, linked lists, records, procedures or other values. This license is granted to allow cursors to be very \"lightweight\" values suitable for tight iteration, even in fairly simple implementations.
    Note that these primitives are necessary to export an iteration facility for char sets to loop macros.
    Rationale: Note that the cursor API's four functions \"fit\" the functional protocol used by the unfolders provided by the list, string and char-set SRFIs (see the example above). By way of contrast, here is a simpler, two-function API that was rejected for failing this criterion. Besides char-set-cursor, it provided a single function that mapped a cursor and a character set to two values, the indexed character and the next cursor. If the cursor had exhausted the character set, then this function returned false instead of the character value, and another end-of-char-set cursor. In this way, the other three functions of the current API were combined together."))
 ((name . "char-set-ref")
  (signature lambda ((char-set? cset) cursor) char?)
  (desc . "(shared description for char-set-cursor, char-set-ref, char-set-cursor-next, end-of-char-set?)
    Cursors are a low-level facility for iterating over the characters in a set. A cursor is a value that indexes a character in a char set. char-set-cursor produces a new cursor for a given char set. The set element indexed by the cursor is fetched with char-set-ref. A cursor index is incremented with char-set-cursor-next; in this way, code can step through every character in a char set. Stepping a cursor \"past the end\" of a char set produces a cursor that answers true to end-of-char-set?. It is an error to pass such a cursor to char-set-ref or to char-set-cursor-next.
    A cursor value may not be used in conjunction with a different character set; if it is passed to char-set-ref or char-set-cursor-next with a character set other than the one used to create it, the results and effects are undefined.
    Cursor values are not necessarily distinct from other types. They may be integers, linked lists, records, procedures or other values. This license is granted to allow cursors to be very \"lightweight\" values suitable for tight iteration, even in fairly simple implementations.
    Note that these primitives are necessary to export an iteration facility for char sets to loop macros.
    Rationale: Note that the cursor API's four functions \"fit\" the functional protocol used by the unfolders provided by the list, string and char-set SRFIs (see the example above). By way of contrast, here is a simpler, two-function API that was rejected for failing this criterion. Besides char-set-cursor, it provided a single function that mapped a cursor and a character set to two values, the indexed character and the next cursor. If the cursor had exhausted the character set, then this function returned false instead of the character value, and another end-of-char-set cursor. In this way, the other three functions of the current API were combined together."))
 ((name . "char-set-cursor-next")
  (signature lambda ((char-set? cset) cursor) *)
  (desc . "(shared description for char-set-cursor, char-set-ref, char-set-cursor-next, end-of-char-set?)
    Cursors are a low-level facility for iterating over the characters in a set. A cursor is a value that indexes a character in a char set. char-set-cursor produces a new cursor for a given char set. The set element indexed by the cursor is fetched with char-set-ref. A cursor index is incremented with char-set-cursor-next; in this way, code can step through every character in a char set. Stepping a cursor \"past the end\" of a char set produces a cursor that answers true to end-of-char-set?. It is an error to pass such a cursor to char-set-ref or to char-set-cursor-next.
    A cursor value may not be used in conjunction with a different character set; if it is passed to char-set-ref or char-set-cursor-next with a character set other than the one used to create it, the results and effects are undefined.
    Cursor values are not necessarily distinct from other types. They may be integers, linked lists, records, procedures or other values. This license is granted to allow cursors to be very \"lightweight\" values suitable for tight iteration, even in fairly simple implementations.
    Note that these primitives are necessary to export an iteration facility for char sets to loop macros.
    Rationale: Note that the cursor API's four functions \"fit\" the functional protocol used by the unfolders provided by the list, string and char-set SRFIs (see the example above). By way of contrast, here is a simpler, two-function API that was rejected for failing this criterion. Besides char-set-cursor, it provided a single function that mapped a cursor and a character set to two values, the indexed character and the next cursor. If the cursor had exhausted the character set, then this function returned false instead of the character value, and another end-of-char-set cursor. In this way, the other three functions of the current API were combined together."))
 ((name . "end-of-char-set?")
  (signature lambda (cursor) boolean?)
  (desc . "(shared description for char-set-cursor, char-set-ref, char-set-cursor-next, end-of-char-set?)
    Cursors are a low-level facility for iterating over the characters in a set. A cursor is a value that indexes a character in a char set. char-set-cursor produces a new cursor for a given char set. The set element indexed by the cursor is fetched with char-set-ref. A cursor index is incremented with char-set-cursor-next; in this way, code can step through every character in a char set. Stepping a cursor \"past the end\" of a char set produces a cursor that answers true to end-of-char-set?. It is an error to pass such a cursor to char-set-ref or to char-set-cursor-next.
    A cursor value may not be used in conjunction with a different character set; if it is passed to char-set-ref or char-set-cursor-next with a character set other than the one used to create it, the results and effects are undefined.
    Cursor values are not necessarily distinct from other types. They may be integers, linked lists, records, procedures or other values. This license is granted to allow cursors to be very \"lightweight\" values suitable for tight iteration, even in fairly simple implementations.
    Note that these primitives are necessary to export an iteration facility for char sets to loop macros.
    Rationale: Note that the cursor API's four functions \"fit\" the functional protocol used by the unfolders provided by the list, string and char-set SRFIs (see the example above). By way of contrast, here is a simpler, two-function API that was rejected for failing this criterion. Besides char-set-cursor, it provided a single function that mapped a cursor and a character set to two values, the indexed character and the next cursor. If the cursor had exhausted the character set, then this function returned false instead of the character value, and another end-of-char-set cursor. In this way, the other three functions of the current API were combined together."))
 ((name . "char-set-fold")
  (signature lambda ((procedure? kons) knil (char-set? cs)) *)
  (subsigs (kons (lambda ((char? c) state) *)))
  (tags pure)
  (desc . "This is the fundamental iterator for character sets. Applies the function kons across the character set cs using initial state value knil. That is, if cs is the empty set, the procedure returns knil. Otherwise, some element c of cs is chosen; let cs' be the remaining, unchosen characters. The procedure returns
(char-set-fold kons (kons c knil) cs')"))
 ((name . "char-set-unfold")
  (signature
   case-lambda
   (((procedure? f) (procedure? p) (procedure? g) seed) char-set?)
   (((procedure? f) (procedure? p) (procedure? g) seed (char-set? base-cs))
    char-set?))
  (subsigs
   (f (lambda (seed) char?))
   (p (lambda (seed) boolean?))
   (g (lambda (seed) *)))
  (tags pure)
  (desc . "(shared description for char-set-unfold, char-set-unfold!) This is a fundamental constructor for char-sets.
* G is used to generate a series of \"seed\" values from the initial seed: seed, (g seed), (g2 seed), (g3 seed), ...
* P tells us when to stop -- when it returns true when applied to one of these seed values.
* F maps each seed value to a character. These characters are added to the base character set base-cs to form the result; base-cs defaults to the empty set. char-set-unfold! adds the characters to base-cs in a linear-update -- it is allowed, but not required, to side-effect and use base-cs's storage to construct the result."))
 ((name . "char-set-unfold!")
  (signature
   lambda
   ((procedure? f) (procedure? p) (procedure? g) seed (char-set? base-cs))
   char-set?)
  (subsigs
   (f (lambda (seed) char?))
   (p (lambda (seed) boolean?))
   (g (lambda (seed) *)))
  (tags pure)
  (desc . "(shared description for char-set-unfold, char-set-unfold!) This is a fundamental constructor for char-sets.
* G is used to generate a series of \"seed\" values from the initial seed: seed, (g seed), (g2 seed), (g3 seed), ...
* P tells us when to stop -- when it returns true when applied to one of these seed values.
* F maps each seed value to a character. These characters are added to the base character set base-cs to form the result; base-cs defaults to the empty set. char-set-unfold! adds the characters to base-cs in a linear-update -- it is allowed, but not required, to side-effect and use base-cs's storage to construct the result."))
 ((name . "char-set-for-each")
  (signature lambda ((procedure? proc) (char-set? cs)) undefined)
  (subsigs (proc (lambda ((char? c)) undefined)))
  (desc . "Apply procedure proc to each character in the character set cs. Note that the order in which proc is applied to the characters in the set is not specified, and may even change from one procedure application to another.
    Nothing at all is specified about the value returned by this procedure; it is not even required to be consistent from call to call. It is simply required to be a value (or values) that may be passed to a command continuation, e.g. as the value of an expression appearing as a non-terminal subform of a begin expression. Note that in R5RS, this restricts the procedure to returning a single value; non-R5RS systems may not even provide this restriction."))
 ((name . "char-set-map")
  (signature lambda ((procedure? proc) (char-set? cs)) char-set?)
  (subsigs (proc (lambda ((char? c)) char?)))
  (tags pure)
  (desc . "proc is a char->char procedure. Apply it to all the characters in the char-set cs, and collect the results into a new character set.
Essentially lifts proc from a char->char procedure to a char-set -> char-set procedure."))
 ((name . "char-set-copy")
  (signature lambda ((char-set? cs)) char-set?)
  (tags pure)
  (desc . "Returns a copy of the character set cs. \"Copy\" means that if either the input parameter or the result value of this procedure is passed to one of the linear-update procedures described below, the other character set is guaranteed not to be altered.
    A system that provides pure-functional implementations of the linear-operator suite could implement this procedure as the identity function -- so copies are not guaranteed to be distinct by eq?."))
 ((name . "char-set")
  (signature lambda ((char? char1) ...) char-set?)
  (tags pure)
  (desc . "Return a character set containing the given characters."))
 ((name . "list->char-set")
  (signature
   case-lambda
   (((list? char-list)) char-set?)
   (((list? char-list) (char-set? base-cs)) char-set?))
  (tags pure)
  (desc . "(shared description for list->char-set, list->char-set!) Return a character set containing the characters in the list of characters char-list.
    If character set base-cs is provided, the characters from char-list are added to it. list->char-set! is allowed, but not required, to side-effect and reuse the storage in base-cs; list->char-set produces a fresh character set."))
 ((name . "list->char-set!")
  (signature lambda ((list? char-list) (char-set? base-cs)) char-set?)
  (desc . "(shared description for list->char-set, list->char-set!) Return a character set containing the characters in the list of characters char-list.
    If character set base-cs is provided, the characters from char-list are added to it. list->char-set! is allowed, but not required, to side-effect and reuse the storage in base-cs; list->char-set produces a fresh character set."))
 ((name . "string->char-set")
  (signature
   case-lambda
   (((string? s)) char-set?)
   (((string? s) (char-set? base-cs)) char-set?))
  (tags pure)
  (desc . "(shared description for string->char-set, string->char-set!) Return a character set containing the characters in the string s.
    If character set base-cs is provided, the characters from s are added to it. string->char-set! is allowed, but not required, to side-effect and reuse the storage in base-cs; string->char-set produces a fresh character set."))
 ((name . "string->char-set!")
  (signature lambda ((string? s) (char-set? base-cs)) char-set?)
  (desc . "(shared description for string->char-set, string->char-set!) Return a character set containing the characters in the string s.
    If character set base-cs is provided, the characters from s are added to it. string->char-set! is allowed, but not required, to side-effect and reuse the storage in base-cs; string->char-set produces a fresh character set."))
 ((name . "char-set-filter")
  (signature
   case-lambda
   (((procedure? pred) (char-set? cs)) char-set?)
   (((procedure? pred) (char-set? cs) (char-set? base-cs)) char-set?))
  (subsigs (pred (lambda ((char? c)) boolean?)))
  (tags pure)
  (desc . "(shared description for char-set-filter, char-set-filter!) Returns a character set containing every character c in cs such that (pred c) returns true.
    If character set base-cs is provided, the characters specified by pred are added to it. char-set-filter! is allowed, but not required, to side-effect and reuse the storage in base-cs; char-set-filter produces a fresh character set.
    An implementation may not save away a reference to pred and invoke it after char-set-filter or char-set-filter! returns -- that is, \"lazy\", on-demand implementations are not allowed, as pred may have external dependencies on mutable data or have other side-effects.
    Rationale: This procedure provides a means of converting a character predicate into its equivalent character set; the cs parameter allows the programmer to bound the predicate's domain. Programmers should be aware that filtering a character set such as char-set:full could be a very expensive operation in an implementation that provided an extremely large character type, such as 32-bit Unicode. An earlier draft of this library provided a simple predicate->char-set procedure, which was rejected in favor of char-set-filter for this reason."))
 ((name . "char-set-filter!")
  (signature
   lambda
   ((procedure? pred) (char-set? cs) (char-set? base-cs))
   char-set?)
  (subsigs (pred (lambda ((char? c)) boolean?)))
  (desc . "(shared description for char-set-filter, char-set-filter!) Returns a character set containing every character c in cs such that (pred c) returns true.
    If character set base-cs is provided, the characters specified by pred are added to it. char-set-filter! is allowed, but not required, to side-effect and reuse the storage in base-cs; char-set-filter produces a fresh character set.
    An implementation may not save away a reference to pred and invoke it after char-set-filter or char-set-filter! returns -- that is, \"lazy\", on-demand implementations are not allowed, as pred may have external dependencies on mutable data or have other side-effects.
    Rationale: This procedure provides a means of converting a character predicate into its equivalent character set; the cs parameter allows the programmer to bound the predicate's domain. Programmers should be aware that filtering a character set such as char-set:full could be a very expensive operation in an implementation that provided an extremely large character type, such as 32-bit Unicode. An earlier draft of this library provided a simple predicate->char-set procedure, which was rejected in favor of char-set-filter for this reason."))
 ((name . "ucs-range->char-set")
  (signature
   case-lambda
   (((integer? lower) (integer? upper)) char-set?)
   (((integer? lower) (integer? upper) (boolean? error?)) char-set?)
   (((integer? lower) (integer? upper) (boolean? error?) (char-set? base-cs))
    char-set?))
  (tags pure)
  (desc . "(shared description for ucs-range->char-set, ucs-range->char-set!) Lower and upper are exact non-negative integers; lower <= upper.
    Returns a character set containing every character whose ISO/IEC 10646 UCS-4 code lies in the half-open range [lower,upper).
    * If the requested range includes unassigned UCS values, these are silently ignored (the current UCS specification has \"holes\" in the space of assigned codes).
    * If the requested range includes \"private\" or \"user space\" codes, these are handled in an implementation-specific manner; however, a UCS- or Unicode-based Scheme implementation should pass them through transparently.
    * If any code from the requested range specifies a valid, assigned UCS character that has no corresponding representative in the implementation's character type, then (1) an error is raised if error? is true, and (2) the code is ignored if error? is false (the default). This might happen, for example, if the implementation uses ASCII characters, and the requested range includes non-ASCII characters.

    If character set base-cs is provided, the characters specified by the range are added to it. ucs-range->char-set! is allowed, but not required, to side-effect and reuse the storage in base-cs; ucs-range->char-set produces a fresh character set.
    Note that ASCII codes are a subset of the Latin-1 codes, which are in turn a subset of the 16-bit Unicode codes, which are themselves a subset of the 32-bit UCS-4 codes. We commit to a specific encoding in this routine, regardless of the underlying representation of characters, so that client code using this library will be portable. I.e., a conformant Scheme implementation may use EBCDIC or SHIFT-JIS to encode characters; it must simply map the UCS characters from the given range into the native representation when possible, and report errors when not possible."))
 ((name . "ucs-range->char-set!")
  (signature
   lambda
   ((integer? lower) (integer? upper) (boolean? error?) (char-set? base-cs))
   char-set?)
  (desc . "(shared description for ucs-range->char-set, ucs-range->char-set!) Lower and upper are exact non-negative integers; lower <= upper.
    Returns a character set containing every character whose ISO/IEC 10646 UCS-4 code lies in the half-open range [lower,upper).
    * If the requested range includes unassigned UCS values, these are silently ignored (the current UCS specification has \"holes\" in the space of assigned codes).
    * If the requested range includes \"private\" or \"user space\" codes, these are handled in an implementation-specific manner; however, a UCS- or Unicode-based Scheme implementation should pass them through transparently.
    * If any code from the requested range specifies a valid, assigned UCS character that has no corresponding representative in the implementation's character type, then (1) an error is raised if error? is true, and (2) the code is ignored if error? is false (the default). This might happen, for example, if the implementation uses ASCII characters, and the requested range includes non-ASCII characters.

    If character set base-cs is provided, the characters specified by the range are added to it. ucs-range->char-set! is allowed, but not required, to side-effect and reuse the storage in base-cs; ucs-range->char-set produces a fresh character set.
    Note that ASCII codes are a subset of the Latin-1 codes, which are in turn a subset of the 16-bit Unicode codes, which are themselves a subset of the 32-bit UCS-4 codes. We commit to a specific encoding in this routine, regardless of the underlying representation of characters, so that client code using this library will be portable. I.e., a conformant Scheme implementation may use EBCDIC or SHIFT-JIS to encode characters; it must simply map the UCS characters from the given range into the native representation when possible, and report errors when not possible."))
 ((name . "->char-set")
  (signature
   case-lambda
   (((string? x)) char-set?)
   (((char? x)) char-set?)
   (((char-set? x)) char-set?))
  (tags pure)
  (desc . "Coerces x into a char-set. X may be a string, character or char-set. A string is converted to the set of its constituent characters; a character is converted to a singleton set; a char-set is returned as-is. This procedure is intended for use by other procedures that want to provide \"user-friendly\", wide-spectrum interfaces to their clients."))
 ((name . "char-set-size")
  (signature lambda ((char-set? cs)) integer?)
  (tags pure)
  (desc . "Returns the number of elements in character set cs."))
 ((name . "char-set-count")
  (signature lambda ((procedure? pred) (char-set? cs)) integer?)
  (subsigs (pred (lambda ((char? c)) boolean?)))
  (tags pure)
  (desc . "Apply pred to the chars of character set cs, and return the number of chars that caused the predicate to return true."))
 ((name . "char-set->list")
  (signature lambda ((char-set? cs)) list?)
  (desc . "This procedure returns a list of the members of character set cs. The order in which cs's characters appear in the list is not defined, and may be different from one call to another."))
 ((name . "char-set->string")
  (signature lambda ((char-set? cs)) string?)
  (desc . "This procedure returns a string containing the members of character set cs. The order in which cs's characters appear in the string is not defined, and may be different from one call to another."))
 ((name . "char-set-contains?")
  (signature lambda ((char-set? cs) (char? char)) boolean?)
  (desc . "This procedure tests char for membership in character set cs.
    The MIT Scheme character-set package called this procedure char-set-member?, but the argument order isn't consistent with the name."))
 ((name . "char-set-every")
  (signature lambda ((procedure? pred) (char-set? cs)) boolean?)
  (subsigs (pred (lambda ((char? c)) boolean?)))
  (tags pure)
  (desc . "(shared description for char-set-every, char-set-any)  The char-set-every procedure returns true if predicate pred returns true of every character in the character set cs. Likewise, char-set-any applies pred to every character in character set cs, and returns the first true value it finds. If no character produces a true value, it returns false. The order in which these procedures sequence through the elements of cs is not specified.
Note that if you need to determine the actual character on which a predicate returns true, use char-set-any and arrange for the predicate to return the character parameter as its true value"))
 ((name . "char-set-any")
  (signature lambda ((procedure? pred) (char-set? cs)) boolean?)
  (subsigs (pred (lambda ((char? c)) boolean?)))
  (tags pure)
  (desc . "(shared description for char-set-every, char-set-any)  The char-set-every procedure returns true if predicate pred returns true of every character in the character set cs. Likewise, char-set-any applies pred to every character in character set cs, and returns the first true value it finds. If no character produces a true value, it returns false. The order in which these procedures sequence through the elements of cs is not specified.
Note that if you need to determine the actual character on which a predicate returns true, use char-set-any and arrange for the predicate to return the character parameter as its true value"))
 ((name . "char-set-adjoin")
  (signature lambda ((char-set? cs) (char? char1) ...) char-set?)
  (tags pure)
  (desc . "Add the chari characters to character set cs."))
 ((name . "char-set-delete")
  (signature lambda ((char-set? cs) (char? char1) ...) char-set?)
  (tags pure)
  (desc . "Delete the chari characters from character set cs."))
 ((name . "char-set-adjoin!")
  (signature lambda ((char-set? cs) (char? char1) ...) char-set?)
  (desc . "Add the chari characters to character set cs. Linear-update variant. The procedure is allowed, but not required, to side-effect its first parameter."))
 ((name . "char-set-delete!")
  (signature lambda ((char-set? cs) (char? char1) ...) char-set?)
  (desc . "Delete the chari characters from character set cs. Linear-update variant. The procedure is allowed, but not required, to side-effect its first parameter."))
 ((name . "char-set-complement")
  (signature lambda ((char-set? cs)) char-set?)
  (tags pure)
  (desc . "(shared description for char-set-complement, char-set-union, char-set-intersection, char-set-difference, char-set-xor, char-set-diff+intersection) These procedures implement set complement, union, intersection, difference, and exclusive-or for character sets. The union, intersection and xor operations are n-ary. The difference function is also n-ary, associates to the left (that is, it computes the difference between its first argument and the union of all the other arguments), and requires at least one argument.
    Boundary cases:
    (char-set-union) => char-set:empty
    (char-set-intersection) => char-set:full
    (char-set-xor) => char-set:empty
    (char-set-difference cs) => cs

    char-set-diff+intersection returns both the difference and the intersection of the arguments -- it partitions its first parameter. It is equivalent to
    (values (char-set-difference cs1 cs2 ...)
            (char-set-intersection cs1 (char-set-union cs2 ...)))

    but can be implemented more efficiently.
    Programmers should be aware that char-set-complement could potentially be a very expensive operation in Scheme implementations that provide a very large character type, such as 32-bit Unicode. If this is a possibility, sets can be complimented with respect to a smaller universe using char-set-difference."))
 ((name . "char-set-union")
  (signature lambda ((char-set? cs1) ...) char-set?)
  (tags pure)
  (desc . "(shared description for char-set-complement, char-set-union, char-set-intersection, char-set-difference, char-set-xor, char-set-diff+intersection) These procedures implement set complement, union, intersection, difference, and exclusive-or for character sets. The union, intersection and xor operations are n-ary. The difference function is also n-ary, associates to the left (that is, it computes the difference between its first argument and the union of all the other arguments), and requires at least one argument.
    Boundary cases:
    (char-set-union) => char-set:empty
    (char-set-intersection) => char-set:full
    (char-set-xor) => char-set:empty
    (char-set-difference cs) => cs

    char-set-diff+intersection returns both the difference and the intersection of the arguments -- it partitions its first parameter. It is equivalent to
    (values (char-set-difference cs1 cs2 ...)
            (char-set-intersection cs1 (char-set-union cs2 ...)))

    but can be implemented more efficiently.
    Programmers should be aware that char-set-complement could potentially be a very expensive operation in Scheme implementations that provide a very large character type, such as 32-bit Unicode. If this is a possibility, sets can be complimented with respect to a smaller universe using char-set-difference."))
 ((name . "char-set-intersection")
  (signature lambda ((char-set? cs1) ...) char-set?)
  (tags pure)
  (desc . "(shared description for char-set-complement, char-set-union, char-set-intersection, char-set-difference, char-set-xor, char-set-diff+intersection) These procedures implement set complement, union, intersection, difference, and exclusive-or for character sets. The union, intersection and xor operations are n-ary. The difference function is also n-ary, associates to the left (that is, it computes the difference between its first argument and the union of all the other arguments), and requires at least one argument.
    Boundary cases:
    (char-set-union) => char-set:empty
    (char-set-intersection) => char-set:full
    (char-set-xor) => char-set:empty
    (char-set-difference cs) => cs

    char-set-diff+intersection returns both the difference and the intersection of the arguments -- it partitions its first parameter. It is equivalent to
    (values (char-set-difference cs1 cs2 ...)
            (char-set-intersection cs1 (char-set-union cs2 ...)))

    but can be implemented more efficiently.
    Programmers should be aware that char-set-complement could potentially be a very expensive operation in Scheme implementations that provide a very large character type, such as 32-bit Unicode. If this is a possibility, sets can be complimented with respect to a smaller universe using char-set-difference."))
 ((name . "char-set-difference")
  (signature lambda ((char-set? cs1) (char-set? cs2) ...) char-set?)
  (tags pure)
  (desc . "(shared description for char-set-complement, char-set-union, char-set-intersection, char-set-difference, char-set-xor, char-set-diff+intersection) These procedures implement set complement, union, intersection, difference, and exclusive-or for character sets. The union, intersection and xor operations are n-ary. The difference function is also n-ary, associates to the left (that is, it computes the difference between its first argument and the union of all the other arguments), and requires at least one argument.
    Boundary cases:
    (char-set-union) => char-set:empty
    (char-set-intersection) => char-set:full
    (char-set-xor) => char-set:empty
    (char-set-difference cs) => cs

    char-set-diff+intersection returns both the difference and the intersection of the arguments -- it partitions its first parameter. It is equivalent to
    (values (char-set-difference cs1 cs2 ...)
            (char-set-intersection cs1 (char-set-union cs2 ...)))

    but can be implemented more efficiently.
    Programmers should be aware that char-set-complement could potentially be a very expensive operation in Scheme implementations that provide a very large character type, such as 32-bit Unicode. If this is a possibility, sets can be complimented with respect to a smaller universe using char-set-difference."))
 ((name . "char-set-xor")
  (signature lambda ((char-set? cs1) ...) char-set?)
  (tags pure)
  (desc . "(shared description for char-set-complement, char-set-union, char-set-intersection, char-set-difference, char-set-xor, char-set-diff+intersection) These procedures implement set complement, union, intersection, difference, and exclusive-or for character sets. The union, intersection and xor operations are n-ary. The difference function is also n-ary, associates to the left (that is, it computes the difference between its first argument and the union of all the other arguments), and requires at least one argument.
    Boundary cases:
    (char-set-union) => char-set:empty
    (char-set-intersection) => char-set:full
    (char-set-xor) => char-set:empty
    (char-set-difference cs) => cs

    char-set-diff+intersection returns both the difference and the intersection of the arguments -- it partitions its first parameter. It is equivalent to
    (values (char-set-difference cs1 cs2 ...)
            (char-set-intersection cs1 (char-set-union cs2 ...)))

    but can be implemented more efficiently.
    Programmers should be aware that char-set-complement could potentially be a very expensive operation in Scheme implementations that provide a very large character type, such as 32-bit Unicode. If this is a possibility, sets can be complimented with respect to a smaller universe using char-set-difference."))
 ((name . "char-set-diff+intersection")
  (signature
   lambda
   ((char-set? cs1) (char-set? cs2) ...)
   (values char-set? char-set?))
  (tags pure)
  (desc . "(shared description for char-set-complement, char-set-union, char-set-intersection, char-set-difference, char-set-xor, char-set-diff+intersection) These procedures implement set complement, union, intersection, difference, and exclusive-or for character sets. The union, intersection and xor operations are n-ary. The difference function is also n-ary, associates to the left (that is, it computes the difference between its first argument and the union of all the other arguments), and requires at least one argument.
    Boundary cases:
    (char-set-union) => char-set:empty
    (char-set-intersection) => char-set:full
    (char-set-xor) => char-set:empty
    (char-set-difference cs) => cs

    char-set-diff+intersection returns both the difference and the intersection of the arguments -- it partitions its first parameter. It is equivalent to
    (values (char-set-difference cs1 cs2 ...)
            (char-set-intersection cs1 (char-set-union cs2 ...)))

    but can be implemented more efficiently.
    Programmers should be aware that char-set-complement could potentially be a very expensive operation in Scheme implementations that provide a very large character type, such as 32-bit Unicode. If this is a possibility, sets can be complimented with respect to a smaller universe using char-set-difference."))
 ((name . "char-set-complement!")
  (signature lambda ((char-set? cs)) char-set?)
  (desc . "(shared description for char-set-complement!, char-set-union!, char-set-intersection!, char-set-difference!, char-set-xor!, char-set-diff+intersection!)
    These are linear-update variants of the set-algebra functions. They are allowed, but not required, to side-effect their first (required) parameter.
    char-set-diff+intersection! is allowed to side-effect both of its two required parameters, cs1 and cs2."))
 ((name . "char-set-union")
  (signature lambda ((char-set? cs1) (char-set? cs2) ...) char-set?)
  (desc . "(shared description for char-set-complement!, char-set-union!, char-set-intersection!, char-set-difference!, char-set-xor!, char-set-diff+intersection!)
    These are linear-update variants of the set-algebra functions. They are allowed, but not required, to side-effect their first (required) parameter.
    char-set-diff+intersection! is allowed to side-effect both of its two required parameters, cs1 and cs2."))
 ((name . "char-set-intersection!")
  (signature lambda ((char-set? cs1) (char-set? cs2) ...) char-set?)
  (desc . "(shared description for char-set-complement!, char-set-union!, char-set-intersection!, char-set-difference!, char-set-xor!, char-set-diff+intersection!)
    These are linear-update variants of the set-algebra functions. They are allowed, but not required, to side-effect their first (required) parameter.
    char-set-diff+intersection! is allowed to side-effect both of its two required parameters, cs1 and cs2."))
 ((name . "char-set-difference!")
  (signature lambda ((char-set? cs1) (char-set? cs2) ...) char-set?)
  (desc . "(shared description for char-set-complement!, char-set-union!, char-set-intersection!, char-set-difference!, char-set-xor!, char-set-diff+intersection!)
    These are linear-update variants of the set-algebra functions. They are allowed, but not required, to side-effect their first (required) parameter.
    char-set-diff+intersection! is allowed to side-effect both of its two required parameters, cs1 and cs2."))
 ((name . "char-set-xor!")
  (signature lambda ((char-set? cs1) (char-set? cs2) ...) char-set?)
  (desc . "(shared description for char-set-complement!, char-set-union!, char-set-intersection!, char-set-difference!, char-set-xor!, char-set-diff+intersection!)
    These are linear-update variants of the set-algebra functions. They are allowed, but not required, to side-effect their first (required) parameter.
    char-set-diff+intersection! is allowed to side-effect both of its two required parameters, cs1 and cs2."))
 ((name . "char-set-diff+intersection!")
  (signature
   lambda
   ((char-set? cs1) (char-set? cs2) (char-set? cs3) ...)
   (values char-set? char-set?))
  (desc . "(shared description for char-set-complement!, char-set-union!, char-set-intersection!, char-set-difference!, char-set-xor!, char-set-diff+intersection!)
    These are linear-update variants of the set-algebra functions. They are allowed, but not required, to side-effect their first (required) parameter.
    char-set-diff+intersection! is allowed to side-effect both of its two required parameters, cs1 and cs2."))
 ((name . "char-set:lower-case") (signature value char-set?) (desc . "Lower-case letters"))
 ((name . "char-set:upper-case") (signature value char-set?) (desc . "Upper-case letters"))
 ((name . "char-set:title-case") (signature value char-set?) (desc . "Title-case letters"))
 ((name . "char-set:letter") (signature value char-set?) (desc . "Letters"))
 ((name . "char-set:digit") (signature value char-set?) (desc . "Digits"))
 ((name . "char-set:letter+digit") (signature value char-set?) (desc . "Letters and digits"))
 ((name . "char-set:graphic") (signature value char-set?) (desc . "Printing characters except spaces"))
 ((name . "char-set:printing") (signature value char-set?) (desc . "Printing characters including spaces"))
 ((name . "char-set:whitespace") (signature value char-set?) (desc . "Whitespace characters"))
 ((name . "char-set:iso-control") (signature value char-set?) (desc . "The ISO control characters"))
 ((name . "char-set:punctuation") (signature value char-set?) (desc . "Punctuation characters"))
 ((name . "char-set:symbol") (signature value char-set?) (desc . "Symbol characters"))
 ((name . "char-set:hex-digit") (signature value char-set?) (desc . "A hexadecimal digit: 0-9, A-F, a-f"))
 ((name . "char-set:blank") (signature value char-set?) (desc . "Blank characters -- horizontal whitespace"))
 ((name . "char-set:ascii") (signature value char-set?) (desc . "All characters in the ASCII set."))
 ((name . "char-set:empty") (signature value char-set?) (desc . "Empty set"))
 ((name . "char-set:full") (signature value char-set?) (desc . "All characters")))
