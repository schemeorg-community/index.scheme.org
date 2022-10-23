(((name . string?)
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (subsigs)
  (supertypes string?))
 ((name . string?) (signature lambda (obj) boolean?) (tags pure predicate))
 ((name . string-null?)
  (signature lambda ((string? string)) boolean?)
  (tags pure))
 ((name . string-every)
  (signature lambda ((procedure? pred) (string? string)) *)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) *))))
 ((name . string-every)
  (signature lambda ((procedure? pred) (string? string) (integer? start)) *)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) *))))
 ((name . string-every)
  (signature
   lambda
   ((procedure? pred) (string? string) (integer? start) (integer? end))
   *)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) *))))
 ((name . string-any)
  (signature lambda ((procedure? pred) (string? string)) *)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) *))))
 ((name . string-any)
  (signature lambda ((procedure? pred) (string? string) (integer? start)) *)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) *))))
 ((name . string-any)
  (signature
   lambda
   ((procedure? pred) (string? string) (integer? start) (integer? end))
   *)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) *))))
 ((name . make-string)
  (signature lambda ((integer? len) (char? char)) string?)
  (tags pure))
 ((name . string) (signature lambda ((char? char) ...) string?) (tags pure))
 ((name . string-tabulate)
  (signature lambda ((procedure? proc) (integer? len)) string?)
  (tags pure)
  (subsigs (proc (lambda ((integer? k)) char?))))
 ((name . string-unfold)
  (signature
   lambda
   ((procedure? stop?) (procedure? mapper) (procedure? successor) seed)
   string?)
  (tags pure)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string?)))
   (success (lambda (seed) *))))
 ((name . string-unfold)
  (signature
   lambda
   ((procedure? stop?)
    (procedure? mapper)
    (procedure? successor)
    seed
    (string? base))
   string?)
  (tags pure)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string?)))
   (success (lambda (seed) *))))
 ((name . string-unfold)
  (signature
   lambda
   ((procedure? stop?)
    (procedure? mapper)
    (procedure? successor)
    seed
    (string? base)
    (procedure? make-final))
   string?)
  (tags pure)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string?)))
   (success (lambda (seed) *))
   (make-final (lambda (seed) (or char? string?)))))
 ((name . string-unfold-right)
  (signature
   lambda
   ((procedure? stop?) (procedure? mapper) (procedure? successor) seed)
   string?)
  (tags pure)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string?)))
   (success (lambda (seed) *))))
 ((name . string-unfold-right)
  (signature
   lambda
   ((procedure? stop?)
    (procedure? mapper)
    (procedure? successor)
    seed
    (string? base))
   string?)
  (tags pure)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string?)))
   (success (lambda (seed) *))))
 ((name . string-unfold-right)
  (signature
   lambda
   ((procedure? stop?)
    (procedure? mapper)
    (procedure? successor)
    seed
    (string? base)
    (procedure? make-final))
   string?)
  (tags pure)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string?)))
   (success (lambda (seed) *))
   (make-final (lambda (seed) (or char? string?)))))
 ((name . string->vector)
  (signature lambda ((string? string)) vector?)
  (tags pure))
 ((name . string->vector)
  (signature lambda ((string? string) (integer? start)) vector?)
  (tags pure))
 ((name . string->vector)
  (signature
   lambda
   ((string? string) (integer? start) (integer? end))
   vector?)
  (tags pure))
 ((name . string->list)
  (signature lambda ((string? string)) list?)
  (tags pure))
 ((name . string->list)
  (signature lambda ((string? string) (integer? start)) list?)
  (tags pure))
 ((name . string->list)
  (signature lambda ((string? string) (integer? start) (integer? end)) list?)
  (tags pure))
 ((name . vector->string)
  (signature lambda ((vector? vector)) string?)
  (tags pure))
 ((name . vector->string)
  (signature lambda ((vector? vector) (integer? start)) string?)
  (tags pure))
 ((name . vector->string)
  (signature lambda ((vector? vector) (integer? start) (integer? end)) string?)
  (tags pure))
 ((name . list->string) (signature lambda ((list? list)) string?) (tags pure))
 ((name . reverse-list->string)
  (signature lambda ((list? char-list)) string?)
  (tags pure))
 ((name . string-length) (signature lambda ((string? string)) integer?) (tags pure))
 ((name . string-ref)
  (signature lambda ((string? string) (integer? idx)) char?)
  (tags pure))
 ((name . substring)
  (signature lambda ((string? string) (integer? start) (integer? end)) string?)
  (tags pure))
 ((name . string-copy)
  (signature lambda ((string? string)) string?)
  (tags pure))
 ((name . string-copy)
  (signature lambda ((string? string) (integer? start)) string?)
  (tags pure))
 ((name . string-copy)
  (signature lambda ((string? string) (integer? start) (integer? end)) string?)
  (tags pure))
 ((name . string-take)
  (signature lambda ((string? string) (integer? nchars)) string?)
  (tags pure))
 ((name . string-drop)
  (signature lambda ((string? string) (integer? nchars)) string?)
  (tags pure))
 ((name . string-take-right)
  (signature lambda ((string? string) (integer? nchars)) string?)
  (tags pure))
 ((name . string-drop-right)
  (signature lambda ((string? string) (integer? nchars)) string?)
  (tags pure))
 ((name . string-pad)
  (signature lambda ((string? string) (integer? len)) string?)
  (tags pure))
 ((name . string-pad)
  (signature lambda ((string? string) (integer? len) (char? char)) string?)
  (tags pure))
 ((name . string-pad)
  (signature
   lambda
   ((string? string) (integer? len) (char? char) (integer? start))
   string?)
  (tags pure))
 ((name . string-pad)
  (signature
   lambda
   ((string? string)
    (integer? len)
    (char? char)
    (integer? start)
    (integer? end))
   string?)
  (tags pure))
 ((name . string-pad-right)
  (signature lambda ((string? string) (integer? len)) string?)
  (tags pure))
 ((name . string-pad-right)
  (signature lambda ((string? string) (integer? len) (char? char)) string?)
  (tags pure))
 ((name . string-pad-right)
  (signature
   lambda
   ((string? string) (integer? len) (char? char) (integer? start))
   string?)
  (tags pure))
 ((name . string-pad-right)
  (signature
   lambda
   ((string? string)
    (integer? len)
    (char? char)
    (integer? start)
    (integer? end))
   string?)
  (tags pure))
 ((name . string-trim)
  (signature lambda ((string? string)) string?)
  (tags pure))
 ((name . string-trim)
  (signature lambda ((string? string) (procedure? pred)) string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-trim)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start))
   string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-trim)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start) (integer? end))
   string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-trim-right)
  (signature lambda ((string? string)) string?)
  (tags pure))
 ((name . string-trim-right)
  (signature lambda ((string? string) (procedure? pred)) string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-trim-right)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start))
   string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-trim-right)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start) (integer? end))
   string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-trim-both)
  (signature lambda ((string? string)) string?)
  (tags pure))
 ((name . string-trim-both)
  (signature lambda ((string? string) (procedure? pred)) string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-trim-both)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start))
   string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-trim-both)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start) (integer? end))
   string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-replace)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1) (integer? end1))
   string?)
  (tags pure))
 ((name . string-replace)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   string?)
  (tags pure))
 ((name . string-replace)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   string?)
  (tags pure))
 ((name . string=?)
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure))
 ((name . string<?)
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure))
 ((name . string>?)
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure))
 ((name . string<=?)
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure))
 ((name . string>=?)
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure))
 ((name . string-ci=?)
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure))
 ((name . string-ci<?)
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure))
 ((name . string-ci>?)
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure))
 ((name . string-ci<=?)
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure))
 ((name . string-ci>=?)
  (signature
   lambda
   ((string? string1) (string? string2) (string? string3) ...)
   boolean?)
  (tags pure))
 ((name . string-prefix-length)
  (signature lambda ((string? string1) (string? string2)) integer?)
  (tags pure))
 ((name . string-prefix-length)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1))
   integer?)
  (tags pure))
 ((name . string-prefix-length)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1) (integer? end1))
   integer?)
  (tags pure))
 ((name . string-prefix-length)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   integer?)
  (tags pure))
 ((name . string-prefix-length)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   integer?)
  (tags pure))
 ((name . string-suffix-length)
  (signature lambda ((string? string1) (string? string2)) integer?)
  (tags pure))
 ((name . string-suffix-length)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1))
   integer?)
  (tags pure))
 ((name . string-suffix-length)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1) (integer? end1))
   integer?)
  (tags pure))
 ((name . string-suffix-length)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   integer?)
  (tags pure))
 ((name . string-suffix-length)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   integer?)
  (tags pure))
 ((name . string-prefix?)
  (signature lambda ((string? string1) (string? string2)) boolean?)
  (tags pure))
 ((name . string-prefix?)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1))
   boolean?)
  (tags pure))
 ((name . string-prefix?)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1) (integer? end1))
   boolean?)
  (tags pure))
 ((name . string-prefix?)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   boolean?)
  (tags pure))
 ((name . string-prefix?)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   boolean?)
  (tags pure))
 ((name . string-suffix?)
  (signature lambda ((string? string1) (string? string2)) boolean?)
  (tags pure))
 ((name . string-suffix?)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1))
   boolean?)
  (tags pure))
 ((name . string-suffix?)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1) (integer? end1))
   boolean?)
  (tags pure))
 ((name . string-suffix?)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   boolean?)
  (tags pure))
 ((name . string-suffix?)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   boolean?)
  (tags pure))
 ((name . string-index)
  (signature lambda ((string? string) (procedure? pred)) (or #f integer?))
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-index)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start))
   (or #f integer?))
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-index)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start) (integer? end))
   (or #f integer?))
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-index-right)
  (signature lambda ((string? string) (procedure? pred)) (or #f integer?))
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-index-right)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start))
   (or #f integer?))
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-index-right)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start) (integer? end))
   (or #f integer?))
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-skip)
  (signature lambda ((string? string) (procedure? pred)) (or #f integer?))
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-skip)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start))
   (or #f integer?))
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-skip)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start) (integer? end))
   (or #f integer?))
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-skip-right)
  (signature lambda ((string? string) (procedure? pred)) (or #f integer?))
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-skip-right)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start))
   (or #f integer?))
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-skip-right)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start) (integer? end))
   (or #f integer?))
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-contains)
  (signature lambda ((string? string1) (string? string2)) (or #f integer?))
  (tags pure))
 ((name . string-contains)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1))
   (or #f integer?))
  (tags pure))
 ((name . string-contains)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1) (integer? end1))
   (or #f integer?))
  (tags pure))
 ((name . string-contains)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   (or #f integer?))
  (tags pure))
 ((name . string-contains)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   (or #f integer?))
  (tags pure))
 ((name . string-contains-right)
  (signature lambda ((string? string1) (string? string2)) (or #f integer?))
  (tags pure))
 ((name . string-contains-right)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1))
   (or #f integer?))
  (tags pure))
 ((name . string-contains-right)
  (signature
   lambda
   ((string? string1) (string? string2) (integer? start1) (integer? end1))
   (or #f integer?))
  (tags pure))
 ((name . string-contains-right)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   (or #f integer?))
  (tags pure))
 ((name . string-contains-right)
  (signature
   lambda
   ((string? string1)
    (string? string2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   (or #f integer?))
  (tags pure))
 ((name . string-take-while)
  (signature lambda ((string? string) (procedure? pred)) string?)
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-take-while)
  (signature lambda ((string? string) (procedure? pred) (integer? start)) string?)
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-take-while)
  (signature lambda ((string? string) (procedure? pred) (integer? start) (integer? end)) string?)
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-take-while-right)
  (signature lambda ((string? string) (procedure? pred)) string?)
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-take-while-right)
  (signature lambda ((string? string) (procedure? pred) (integer? start)) string?)
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-take-while-right)
  (signature lambda ((string? string) (procedure? pred) (integer? start) (integer? end)) string?)
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-drop-while)
  (signature lambda ((string? string) (procedure? pred)) string?)
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-drop-while)
  (signature lambda ((string? string) (procedure? pred) (integer? start)) string?)
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-drop-while)
  (signature lambda ((string? string) (procedure? pred) (integer? start) (integer? end)) string?)
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-drop-while-right)
  (signature lambda ((string? string) (procedure? pred)) string?)
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-drop-while-right)
  (signature lambda ((string? string) (procedure? pred) (integer? start)) string?)
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-drop-while-right)
  (signature lambda ((string? string) (procedure? pred) (integer? start) (integer? end)) string?)
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-span)
  (signature lambda ((string? string) (procedure? pred)) (values string? string))
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-span)
  (signature lambda ((string? string) (procedure? pred) (integer? start)) (values string? string))
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-span)
  (signature lambda ((string? string) (procedure? pred) (integer? start) (integer? end)) (values string? string))
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-break)
  (signature lambda ((string? string) (procedure? pred)) (values string? string))
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-break)
  (signature lambda ((string? string) (procedure? pred) (integer? start)) (values string? string))
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-break)
  (signature lambda ((string? string) (procedure? pred) (integer? start) (integer? end)) (values string? string))
  (tags pure)
  (subsigs
    (pred (lambda ((char? c)) boolean?))))
 ((name . string-append)
  (signature lambda ((string? string) ...) string?)
  (tags pure))
 ((name . string-concatenate)
  (signature lambda ((list? string-list)) string?)
  (tags pure))
 ((name . string-concatenate-reverse)
  (signature lambda ((list? string-list)) string?)
  (tags pure))
 ((name . string-concatenate-reverse)
  (signature lambda ((list? string-list) (string? final-string)) string?)
  (tags pure))
 ((name . string-concatenate-reverse)
  (signature
   lambda
   ((list? string-list) (string? final-string) (integer? end))
   string?)
  (tags pure))
 ((name . string-join)
  (signature lambda ((list? string-list)) string?)
  (tags pure))
 ((name . string-join)
  (signature lambda ((list? string-list) (string? delimiter)) string?)
  (tags pure))
 ((name . string-join)
  (signature
   lambda
   ((list? string-list) (string? delimiter) (symbol? grammar))
   string?)
  (tags pure)
  (spec-values
    (grammar
      ("'infix" "infix or separator grammar: insert the delimiter between list elements. An empty list will produce an empty string.")
      ("'strict-infix" "same as 'infix if the string-list is non-empty, but will signal an error if given an empty list. (This avoids an ambiguity)")
      ("'suffix" "suffix or terminator grammar: insert the delimiter after every list element.")
      ("'prefix" "prefix grammar: insert the delimiter before every list element."))))
 ((name . string-fold)
  (signature lambda ((procedure? kons) knil (string? string)) *)
  (tags pure)
  (subsigs (kons (lambda ((char? char) state) *))))
 ((name . string-fold)
  (signature
   lambda
   ((procedure? kons) knil (string? string) (integer? start))
   *)
  (tags pure)
  (subsigs (kons (lambda ((char? char) state) *))))
 ((name . string-fold)
  (signature
   lambda
   ((procedure? kons) knil (string? string) (integer? start) (integer? end))
   *)
  (tags pure)
  (subsigs (kons (lambda ((char? char) state) *))))
 ((name . string-fold-right)
  (signature lambda ((procedure? kons) knil (string? string)) *)
  (tags pure)
  (subsigs (kons (lambda ((char? char) state) *))))
 ((name . string-fold-right)
  (signature
   lambda
   ((procedure? kons) knil (string? string) (integer? start))
   *)
  (tags pure)
  (subsigs (kons (lambda ((char? char) state) *))))
 ((name . string-fold-right)
  (signature
   lambda
   ((procedure? kons) knil (string? string) (integer? start) (integer? end))
   *)
  (tags pure)
  (subsigs (kons (lambda ((char? char) state) *))))
 ((name . string-map)
  (signature
   lambda
   ((procedure? proc) (string? string1) (string? string2) ...)
   string?)
  (tags pure)
  (subsigs
   (proc (lambda ((char? char1) (char? char2) ...) (or string? char?)))))
 ((name . string-for-each)
  (signature
   lambda
   ((procedure? proc) (string? string1) (string? string2) ...)
   undefined)
  (subsigs (proc (lambda ((char? char1) (char? char2) ...) undefined))))
 ((name . string-count)
  (signature lambda ((string? string) (procedure? pred)) integer?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-count)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start))
   integer?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-count)
  (signature
   lambda
   ((string? string) (procedure? pred) (integer? start) (integer? end))
   integer?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-filter)
  (signature lambda ((procedure? pred) (string? string)) string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-filter)
  (signature
   lambda
   ((procedure? pred) (string? string) (integer? start))
   string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-filter)
  (signature
   lambda
   ((procedure? pred) (string? string) (integer? start) (integer? end))
   string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-remove)
  (signature lambda ((procedure? pred) (string? string)) string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-remove)
  (signature
   lambda
   ((procedure? pred) (string? string) (integer? start))
   string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-remove)
  (signature
   lambda
   ((procedure? pred) (string? string) (integer? start) (integer? end))
   string?)
  (tags pure)
  (subsigs (pred (lambda ((char? char)) boolean?))))
 ((name . string-replicate)
  (signature lambda ((string? string) (integer? from) (integer? to)) string?)
  (tags pure))
 ((name . string-replicate)
  (signature
   lambda
   ((string? string) (integer? from) (integer? to) (integer? start))
   string?)
  (tags pure))
 ((name . string-replicate)
  (signature
   lambda
   ((string? string)
    (integer? from)
    (integer? to)
    (integer? start)
    (integer? end))
   string?)
  (tags pure))
 ((name . string-segment)
  (signature lambda ((string? string) (integer? k)) list?)
  (tags pure))
 ((name . string-split)
  (signature lambda ((string? string) (string? delimiter)) list?)
  (tags pure))
 ((name . string-split)
  (signature
   lambda
   ((string? string) (string? delimiter) (symbol? grammar))
   list?)
  (tags pure)
  (spec-values
    (grammar
      ("'infix" "empty string produces empty list")
      ("'strict-infix" "empty string signals an error")
      ("'suffix" "leading empty string is suppressed")
      ("'prefix" "trailing empty string is suppressed"))))
 ((name . string-split)
  (signature
   lambda
   ((string? string) (string? delimiter) (symbol? grammar) ((or #f integer?) limit))
   list?)
  (tags pure)
  (spec-values
    (grammar
      ("'infix" "empty string produces empty list")
      ("'strict-infix" "empty string signals an error")
      ("'suffix" "leading empty string is suppressed")
      ("'prefix" "trailing empty string is suppressed"))))
 ((name . string-split)
  (signature
   lambda
   ((string? string)
    (string? delimiter)
    (symbol? grammar)
    ((or #f integer?) limit)
    (integer? start))
   list?)
  (tags pure)
  (spec-values
    (grammar
      ("'infix" "empty string produces empty list")
      ("'strict-infix" "empty string signals an error")
      ("'suffix" "leading empty string is suppressed")
      ("'prefix" "trailing empty string is suppressed"))))
 ((name . string-split)
  (signature
   lambda
   ((string? string)
    (string? delimiter)
    (symbol? grammar)
    ((or #f integer?) limit)
    (integer? start)
    (integer? end))
   list?)
  (tags pure)
  (spec-values
    (grammar
      ("'infix" "empty string produces empty list")
      ("'strict-infix" "empty string signals an error")
      ("'suffix" "leading empty string is suppressed")
      ("'prefix" "trailing empty string is suppressed"))))
 ((name . read-string)
  (signature lambda ((integer? k)) (or eof-object? string?))
  (parameterized-by "(scheme base) current-input-port"))
 ((name . read-string)
  (signature
   lambda
   ((integer? k) (input-port? port))
   (or eof-object? string?)))
 ((name . write-string)
  (signature lambda ((string? string)) undefined)
  (parameterized-by "(scheme base) current-output-port"))
 ((name . write-string)
  (signature lambda ((string? string) (output-port? port)) undefined))
 ((name . write-string)
  (signature
   lambda
   ((string? string) (output-port? port) (integer? start))
   undefined))
 ((name . write-string)
  (signature
   lambda
   ((string? string) (output-port? port) (integer? start) (integer? end))
   undefined))
 ((name . string-set!)
  (signature lambda ((string? string) (integer? k) (char? char)) undefined))
 ((name . string-fill!)
  (signature lambda ((string? string) (char? fill)) undefined))
 ((name . string-fill!)
  (signature
   lambda
   ((string? string) (char? fill) (integer? start))
   undefined))
 ((name . string-fill!)
  (signature
   lambda
   ((string? string) (char? fill) (integer? start) (integer? end))
   undefined))
 ((name . string-copy!)
  (signature lambda ((string? to) (integer? at) (string? from)) undefined))
 ((name . string-copy!)
  (signature
   lambda
   ((string? to) (integer? at) (string? from) (integer? start))
   undefined))
 ((name . string-copy!)
  (signature
   lambda
   ((string? to) (integer? at) (string? from) (integer? start) (integer? end))
   undefined)))
