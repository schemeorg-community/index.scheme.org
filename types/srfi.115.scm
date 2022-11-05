(((name . "regexp") (signature lambda (re) regexp?) (tags pure))
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
    \|
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
     (\| sre ...)
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
  (tags pure))
 ((name . "regexp->sre") (signature lambda ((regexp? re)) *) (tags pure))
 ((name . "charset->sre")
  (signature lambda ((charset? char-set)) *)
  (tags pure))
 ((name . "valid-sre?") (signature lambda (obj) boolean?) (tags pure))
 ((name . "regexp?") (signature lambda (obj) boolean?) (tags predicate pure))
 ((name . "regexp-matches")
  (signature
   case-lambda
   ((re (string? str)) (or regexp-match? #f))
   ((re (string? str) (integer? start)) (or regexp-match? #f))
   ((re (string? str) (integer? start) (integer? end)) (or regexp-match? #f)))
  (tags pure))
 ((name . "regexp-matches?")
  (signature
   case-lambda
   ((re (string? str)) boolean?)
   ((re (string? str) (integer? start)) boolean?)
   ((re (string? str) (integer? start) (integer? end)) boolean?))
  (tags pure))
 ((name . "regexp-search")
  (signature
   case-lambda
   ((re (string? str)) (or regexp-match? #f))
   ((re (string? str) (integer? start)) (or regexp-match? #f))
   ((re (string? str) (integer? start) (integer? end)) (or regexp-match? #f)))
  (tags pure))
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
  (tags pure))
 ((name . "regexp-extract")
  (signature
   case-lambda
   ((re (string? str)) list?)
   ((re (string? str) (integer? start)) list?)
   ((re (string? str) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "regexp-split")
  (signature
   case-lambda
   ((re (string? str)) list?)
   ((re (string? str) (integer? start)) list?)
   ((re (string? str) (integer? start) (integer? end)) list?))
  (tags pure))
 ((name . "regexp-partition")
  (signature
   case-lambda
   ((re (string? str)) list?)
   ((re (string? str) (integer? start)) list?)
   ((re (string? str) (integer? start) (integer? end)) list?))
  (tags pure))
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
  (tags pure))
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
  (tags pure))
 ((name . "regexp-match?")
  (signature lambda (obj) boolean?)
  (tags pure predicate))
 ((name . "regexp-match-count")
  (signature lambda ((regexp-match? obj)) integer?)
  (tags pure))
 ((name . "regexp-match-submatch")
  (signature
   lambda
   ((regexp-match? obj) ((or integer? symbol?) field))
   (or string? #f))
  (tags pure))
 ((name . "regexp-match-submatch-start")
  (signature
   lambda
   ((regexp-match? obj) ((or integer? symbol?) field))
   (or integer? #f))
  (tags pure))
 ((name . "regexp-match-submatch-end")
  (signature
   lambda
   ((regexp-match? obj) ((or integer? symbol?) field))
   (or integer? #f))
  (tags pure))
 ((name . "regexp-match->list")
  (signature lambda ((regexp-match? obj)) list?)
  (tags pure)))
