(((name . "regexp")
  (signature lambda (re) regexp?)
  (tags pure)
  (desc . "Compiles a regexp if given an object whose structure matches the SRE syntax. This may be written as a literal or partial literal with quote or quasiquote, or may be generated entirely programmatically. Returns re unmodified if it is already a regexp. Raises an error if re is neither a regexp nor a valid representation of an SRE.
Mutating re may invalidate the resulting regexp, causing unspecified results if subsequently used for matching.
SRE syntax:

    <sre> ::=
     | <string>                    ; A literal string match.
     | <cset-sre>                  ; A character set match.
     | (* <sre> ...)               ; 0 or more matches.
     | (zero-or-more <sre> ...)
     | (+ <sre> ...)               ; 1 or more matches.
     | (one-or-more <sre> ...)
     | (? <sre> ...)               ; 0 or 1 matches.
     | (optional <sre> ...)
     | (= <n> <sre> ...)           ; <n> matches.
     | (exactly <n> <sre> ...)
     | (>= <n> <sre> ...)          ; <n> or more matches.
     | (at-least <n> <sre> ...)
     | (** <n> <m> <sre> ...)      ; <n> to <m> matches.
     | (repeated <n> <m> <sre> ...)

     | (|  <sre> ...)              ; Alternation.
     | (or <sre> ...)

     | (:   <sre> ...)             ; Sequence.
     | (seq <sre> ...)     
     | ($ <sre> ...)               ; Numbered submatch.
     | (submatch <sre> ...)
     | (-> <name> <sre> ...)               ;  Named submatch.  <name> is
     | (submatch-named <name> <sre> ...)   ;  a symbol.

     | (w/case   <sre> ...)        ; Introduce a case-sensitive context.
     | (w/nocase <sre> ...)        ; Introduce a case-insensitive context.

     | (w/unicode   <sre> ...)     ; Introduce a unicode context.
     | (w/ascii <sre> ...)         ; Introduce an ascii context.

     | (w/nocapture <sre> ...)     ; Ignore all enclosed submatches.

     | bos                         ; Beginning of string.
     | eos                         ; End of string.

     | bol                         ; Beginning of line.
     | eol                         ; End of line.

     | bog                         ; Beginning of grapheme cluster.
     | eog                         ; End of grapheme cluster.
     | grapheme                    ; A single grapheme cluster.

     | bow                         ; Beginning of word.
     | eow                         ; End of word.
     | nwb                         ; A non-word boundary.
     | (word <sre> ...)            ; An SRE wrapped in word boundaries.
     | (word+ <cset-sre> ...)      ; A single word restricted to a cset.
     | word                        ; A single word.

     | (?? <sre> ...)              ; A non-greedy pattern, 0 or 1 match.
     | (non-greedy-optional <sre> ...)
     | (*? <sre> ...)              ; Non-greedy 0 or more matches.
     | (non-greedy-zero-or-more <sre> ...)
     | (**? <m> <n> <sre> ...)     ; Non-greedy <m> to <n> matches.
     | (non-greedy-repeated <sre> ...)
     | (look-ahead <sre> ...)      ; Zero-width look-ahead assertion.
     | (look-behind <sre> ...)     ; Zero-width look-behind assertion.
     | (neg-look-ahead <sre> ...)  ; Zero-width negative look-ahead assertion.
     | (neg-look-behind <sre> ...) ; Zero-width negative look-behind assertion.

     | (backref <n-or-name>)       ; Match a previous submatch.

The grammar for cset-sre is as follows.

    <cset-sre> ::=
     | <char>                      ; literal char
     | \"<char>\"                    ; string of one char
     | <char-set>                  ; embedded SRFI 14 char set
     | (<string>)                  ; literal char set
     | (char-set <string>)
     | (/ <range-spec> ...)        ; ranges
     | (char-range <range-spec> ...)
     | (or <cset-sre> ...)         ; union
     | (|\\|| <cset-sre> ...)
     | (and <cset-sre> ...)        ; intersection
     | (& <cset-sre> ...)
     | (- <cset-sre> ...)          ; difference
     | (- <difference> ...)
     | (~ <cset-sre> ...)          ; complement of union
     | (complement <cset-sre> ...)
     | (w/case <cset-sre> ...)     ; case and unicode toggling
     | (w/nocase <cset-sre> ...)
     | (w/ascii <cset-sre> ...)
     | (w/unicode <cset-sre> ...)
     | any | nonl | ascii | lower-case | lower
     | upper-case | upper | title-case | title
     | alphabetic | alpha | alphanumeric | alphanum | alnum
     | numeric | num | punctuation | punct | symbol
     | graphic | graph | whitespace | white | space
     | printing | print | control | cntrl | hex-digit | xdigit

    <range-spec> ::= <string> | <char>"))
 ((name . "rx")
  (signature
   syntax-rules
   (*
    one-or-more
    ?
    optional
    =
    exactly
    at-least
    **
    repeated
    |\||
    or
    :
    seq
    $
    submatch
    ->
    sumatch-named
    w/case
    w/nocase
    w/unicode
    w/ascii
    w/nocapture
    bos
    eos
    bol
    eol
    bog
    eog
    grapheme
    bow
    eow
    nwb
    word
    word+
    ??
    non-greedy-optional
    *?
    non-greedy-zero-or-more
    **?
    non-greedy-repeated
    look-ahead
    look-behind
    neg-look-ahead
    neg-look-behind
    backref)
   ((_ sre ...) regexp?))
  (subsigs
   (sre
    (pattern
     string
     charset
     (* sre ...)
     (one-or-more sre ...)
     (? sre ...)
     (optional sre ...)
     (= n sre ...)
     (exactly n sre ...)
     (>= n sre ...)
     (at-least n sre ...)
     (** n m sre ...)
     (repeated n m sre ...)
     (|\|| sre ...)
     (or sre ...)
     (: sre ...)
     (seq sre ...)
     ($ sre ...)
     (submatch sre ...)
     (-> name sre ...)
     (submatch-named name sre ...)
     (w/case sre ...)
     (w/nocase sre ...)
     (w/unicode sre ...)
     (w/ascii sre ...)
     (w/nocapture sre ...)
     bos
     eos
     bol
     eol
     bog
     eog
     grapheme
     bow
     eow
     nwb
     (word sre ...)
     (word+ cset-sre ...)
     word
     (?? sre ...)
     (non-greedy-optional sre ...)
     (*? sre ...)
     (non-greedy-zero-or-more sre ...)
     (**? m n sre ...)
     (non-greedy-repeated sre ...)
     (look-ahead sre ...)
     (look-behind sre ...)
     (neg-look-ahead sre ...)
     (neg-look-behind sre ...)
     (backref n-or-name))))
  (tags pure)
  (desc . "Macro shorthand for (regexp `(: sre ...)). May be able to perform some or all computation at compile time if sre is not unquoted. Note because of this equivalence with the procedural constructor regexp, the semantics of unquote differs from the original SCSH implementation in that unquoted expressions can expand into any object matching the SRE syntax, but not a compiled regexp object. Further, unquote and unquote-splicing both expand all matches."))
 ((name . "regexp->sre")
  (signature lambda ((regexp? re)) *)
  (tags pure)
  (desc . "Returns an SRE corresponding to the given regexp re. The SRE will be equivalent to (will match the same strings) but not necessarily equal? to the SRE originally used to compile re. Mutating the result may invalidate re, causing unspecified results if subsequently used for matching."))
 ((name . "char-set->sre")
  (signature lambda ((charset? char-set)) *)
  (tags pure)
  (desc . "Returns an SRE corresponding to the given SRFI 14 character set. The resulting SRE expands the character set into notation which does not make use of embedded SRFI 14 character sets, and so is suitable for writing portably."))
 ((name . "valid-sre?") (signature lambda (obj) boolean?) (tags pure) (desc . "Returns true iff obj can be safely passed to regexp."))
 ((name . "regexp?") (signature lambda (obj) boolean?) (tags predicate pure) (desc . "Returns true iff obj is a regexp."))
 ((name . "regexp-matches")
  (signature
   case-lambda
   ((re (string? str)) (or regexp-match? #f))
   ((re (string? str) (integer? start)) (or regexp-match? #f))
   ((re (string? str) (integer? start) (integer? end)) (or regexp-match? #f)))
  (tags pure)
  (desc . "Returns an regexp-match object if re successfully matches the entire string str from start (inclusive) to end (exclusive), or #f is the match fails. The regexp-match object will contain information needed to extract any submatches. "))
 ((name . "regexp-matches?")
  (signature
   case-lambda
   ((re (string? str)) boolean?)
   ((re (string? str) (integer? start)) boolean?)
   ((re (string? str) (integer? start) (integer? end)) boolean?))
  (tags pure)
  (desc . "Returns #t if re matches str as in regexp-matches, or #f otherwise. May be faster than regexp-matches since it doesn't need to return submatch data."))
 ((name . "regexp-search")
  (signature
   case-lambda
   ((re (string? str)) (or regexp-match? #f))
   ((re (string? str) (integer? start)) (or regexp-match? #f))
   ((re (string? str) (integer? start) (integer? end)) (or regexp-match? #f)))
  (tags pure)
  (desc . "Returns a regexp-match object if re successfully matches a substring of str between start (inclusive) and end (exclusive), or #f if the match fails. The regexp-match object will contain information needed to extract any submatches."))
 ((name . "regexp-fold")
  (signature
   case-lambda
   ((re (procedure? kons) knil (string? str)) *)
   ((re (procedure? kons) knil (string? str) (procedure? finish)) *)
   ((re
     (procedure? kons)
     knil
     (string? str)
     (procedure? finish)
     (integer? start))
    *)
   ((re
     (procedure? kons)
     knil
     (string? str)
     (procedure? finish)
     (integer? start)
     (integer? end))
    *))
  (subsigs
   (kons (lambda ((integer? i) (regexp-match? match) (string? str) acc) *))
   (finish (lambda ((integer? i) (#f match) (string? str) acc) *)))
  (tags pure)
  (desc . "The fundamental regexp matching iterator. Repeatedly searches str for the regexp re so long as a match can be found. On each successful match, applies (kons i regexp-match str acc) where i is the index since the last match (beginning with start), regexp-match is the resulting match, and acc is the result of the previous kons application, beginning with knil. When no more matches can be found, calls finish with the same arguments, except that regexp-match is #f.
By default finish just returns acc."))
 ((name . "regexp-extract")
  (signature
   case-lambda
   ((re (string? str)) list?)
   ((re (string? str) (integer? start)) list?)
   ((re (string? str) (integer? start) (integer? end)) list?))
  (tags pure)
  (desc . "Extracts all non-empty substrings of str which match re between start and end as a list of strings."))
 ((name . "regexp-split")
  (signature
   case-lambda
   ((re (string? str)) list?)
   ((re (string? str) (integer? start)) list?)
   ((re (string? str) (integer? start) (integer? end)) list?))
  (tags pure)
  (desc . "Splits str into a list of (possibly empty) strings separated by non-empty matches of re."))
 ((name . "regexp-partition")
  (signature
   case-lambda
   ((re (string? str)) list?)
   ((re (string? str) (integer? start)) list?)
   ((re (string? str) (integer? start) (integer? end)) list?))
  (tags pure)
  (desc . "Partitions str into a list of non-empty strings matching re, interspersed with the unmatched portions of the string. The first and every odd element is an unmatched substring, which will be the empty string if re matches at the beginning of the string or end of the previous match. The second and every even element will be a substring matching re. If the final match ends at the end of the string, no trailing empty string will be included. Thus, in the degenerate case where str is the empty string, the result is (\"\").
Note that regexp-partition is equivalent to interleaving the results of regexp-split and regexp-extract, starting with the former."))
 ((name . "regexp-replace")
  (signature
   case-lambda
   ((re (string? str) ((or string? integer? symbol?) subst)) string?)
   ((re (string? str) ((or string? integer? symbol?) subst) (integer? start))
    string?)
   ((re
     (string? str)
     ((or string? integer? symbol?) subst)
     (integer? start)
     (integer? end))
    string?)
   ((re
     (string? str)
     ((or string? integer? symbol?) subst)
     (integer? start)
     (integer? end)
     (integer? count))
    string?))
  (tags pure)
  (desc . "Returns a new string replacing the countth match of re in str the subst, where the zero-indexed count defaults to zero (i.e. the first match). If there are not count matches, returns the selected substring unmodified.
subst can be a string, an integer or symbol indicating the contents of a numbered or named submatch of re, 'pre for the substring to the left of the match, or 'post for the substring to the right of the match.
The optional parameters start and end restrict both the matching and the substitution, to the given indices, such that the result is equivalent to omitting these parameters and replacing on (substring str start end). As a convenience, a value of #f for end is equivalent to (string-length str)."))
 ((name . "regexp-replace-all")
  (signature
   case-lambda
   ((re (string? str) ((or string? integer? symbol?) subst) (integer? start))
    string?)
   ((re
     (string? str)
     ((or string? integer? symbol?) subst)
     (integer? start)
     (integer? end))
    string?))
  (tags pure)
  (desc . "Equivalent to regexp-replace, but replaces all occurrences of re in str."))
 ((name . "regexp-match?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns true iff obj is a successful match from regexp-matches or regexp-search."))
 ((name . "regexp-match-count")
  (signature lambda ((regexp-match? obj)) integer?)
  (tags pure)
  (desc . "Returns the number of submatches of regexp-match, regardless of whether they matched or not. Does not include the implicit zero full match in the count."))
 ((name . "regexp-match-submatch")
  (signature
   lambda
   ((regexp-match? obj) ((or integer? symbol?) field))
   (or string? #f))
  (tags pure)
  (desc . "Returns the substring matched in regexp-match corresponding to field, either an integer or a symbol for a named submatch. Index 0 refers to the entire match, index 1 to the first lexicographic submatch, and so on. If there are multiple submatches with the same name, the first which matched is returned. If passed an integer outside the range of matches, or a symbol which does not correspond to a named submatch of the pattern, it is an error. If the corresponding submatch did not match, returns false.
The result of extracting a submatch after the original matched string has been mutated is unspecified."))
 ((name . "regexp-match-submatch-start")
  (signature
   lambda
   ((regexp-match? obj) ((or integer? symbol?) field))
   (or integer? #f))
  (tags pure)
  (desc . "Returns the start index regexp-match corresponding to field, as in regexp-match-submatch."))
 ((name . "regexp-match-submatch-end")
  (signature
   lambda
   ((regexp-match? obj) ((or integer? symbol?) field))
   (or integer? #f))
  (tags pure)
  (desc . "Returns the end index in regexp-match corresponding to field, as in regexp-match-submatch."))
 ((name . "regexp-match->list")
  (signature lambda ((regexp-match? obj)) list?)
  (tags pure)
  (desc . "Returns a list of all submatches in regexp-match as string or false, beginning with the entire match 0.")))
