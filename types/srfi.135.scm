(((name . "text?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (supertypes textual?))
 ((name . "textual?") (signature lambda (obj) boolean?) (tags pure predicate))
 ((name . "textual-null?")
  (signature lambda ((textual? textual)) boolean?)
  (tags pure))
 ((name . "textual-every")
  (signature lambda ((procedure? pred) (textual? textual)) *)
  (subsigs (pred (lambda ((char? char)) *)))
  (tags pure))
 ((name . "textual-every")
  (signature lambda ((procedure? pred) (textual? textual) (integer? start)) *)
  (subsigs (pred (lambda ((char? char)) *)))
  (tags pure))
 ((name . "textual-every")
  (signature
   lambda
   ((procedure? pred) (textual? textual) (integer? start) (integer? end))
   *)
  (subsigs (pred (lambda ((char? char)) *)))
  (tags pure))
 ((name . "textual-any")
  (signature lambda ((procedure? pred) (textual? textual)) *)
  (subsigs (pred (lambda ((char? char)) *)))
  (tags pure))
 ((name . "textual-any")
  (signature lambda ((procedure? pred) (textual? textual) (integer? start)) *)
  (subsigs (pred (lambda ((char? char)) *)))
  (tags pure))
 ((name . "textual-any")
  (signature
   lambda
   ((procedure? pred) (textual? textual) (integer? start) (integer? end))
   *)
  (subsigs (pred (lambda ((char? char)) *)))
  (tags pure))
 ((name . "make-text")
  (signature lambda ((integer? len) (char? char)) text?)
  (tags pure))
 ((name . "text") (signature lambda ((char? char) ...) text?) (tags pure))
 ((name . "text-tabulate")
  (signature lambda ((procedure? proc) (integer? len)) text?)
  (subsigs (proc (lambda ((integer? k)) char?)))
  (tags pure))
 ((name . "text-unfold")
  (signature
   lambda
   ((procedure? stop?) (procedure? mapper) (procedure? successor) seed)
   text?)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string? text?)))
   (success (lambda (seed) *)))
  (tags pure))
 ((name . "text-unfold")
  (signature
   lambda
   ((procedure? stop?)
    (procedure? mapper)
    (procedure? successor)
    seed
    (textual? base))
   text?)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string? text?)))
   (success (lambda (seed) *)))
  (tags pure))
 ((name . "text-unfold")
  (signature
   lambda
   ((procedure? stop?)
    (procedure? mapper)
    (procedure? successor)
    seed
    (textual? base)
    (procedure? make-final))
   text?)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string? text?)))
   (success (lambda (seed) *))
   (make-final (lambda (seed) (or char? string? text?))))
  (tags pure))
 ((name . "text-unfold-right")
  (signature
   lambda
   ((procedure? stop?) (procedure? mapper) (procedure? successor) seed)
   text?)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string? text?)))
   (success (lambda (seed) *)))
  (tags pure))
 ((name . "text-unfold-right")
  (signature
   lambda
   ((procedure? stop?)
    (procedure? mapper)
    (procedure? successor)
    seed
    (textual? base))
   text?)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string? text?)))
   (success (lambda (seed) *)))
  (tags pure))
 ((name . "text-unfold-right")
  (signature
   lambda
   ((procedure? stop?)
    (procedure? mapper)
    (procedure? successor)
    seed
    (textual? base)
    (procedure? make-final))
   text?)
  (subsigs
   (stop? (lambda (seed) boolean?))
   (mapper (lambda (seed) (or char? string? text?)))
   (success (lambda (seed) *))
   (make-final (lambda (seed) (or char? string? text?))))
  (tags pure))
 ((name . "textual->text")
  (signature lambda ((textual? textual)) text?)
  (tags pure))
 ((name . "textual->string")
  (signature lambda ((textual? textual)) string?)
  (tags pure))
 ((name . "textual->string")
  (signature lambda ((textual? textual) (integer? start)) string?)
  (tags pure))
 ((name . "textual->string")
  (signature
   lambda
   ((textual? textual) (integer? start) (integer? end))
   string?)
  (tags pure))
 ((name . "textual->vector")
  (signature lambda ((textual? textual)) vector?)
  (tags pure))
 ((name . "textual->vector")
  (signature lambda ((textual? textual) (integer? start)) vector?)
  (tags pure))
 ((name . "textual->vector")
  (signature
   lambda
   ((textual? textual) (integer? start) (integer? end))
   vector?)
  (tags pure))
 ((name . "textual->list")
  (signature lambda ((textual? textual)) list?)
  (tags pure))
 ((name . "textual->list")
  (signature lambda ((textual? textual) (integer? start)) list?)
  (tags pure))
 ((name . "textual->list")
  (signature lambda ((textual? textual) (integer? start) (integer? end)) list?)
  (tags pure))
 ((name . "string->text")
  (signature lambda ((string? string)) text?)
  (tags pure))
 ((name . "string->text")
  (signature lambda ((string? string) (integer? start)) text?)
  (tags pure))
 ((name . "string->text")
  (signature lambda ((string? string) (integer? start) (integer? end)) text?)
  (tags pure))
 ((name . "vector->text")
  (signature lambda ((vector? vector)) text?)
  (tags pure))
 ((name . "vector->text")
  (signature lambda ((vector? vector) (integer? start)) text?)
  (tags pure))
 ((name . "vector->text")
  (signature lambda ((vector? vector) (integer? start) (integer? end)) text?)
  (tags pure))
 ((name . "list->text") (signature lambda ((list? list)) text?) (tags pure))
 ((name . "list->text")
  (signature lambda ((list? list) (integer? start)) text?)
  (tags pure))
 ((name . "list->text")
  (signature lambda ((list? list) (integer? start) (integer? end)) text?)
  (tags pure))
 ((name . "reverse-list->text")
  (signature lambda ((list? char-list)) text?)
  (tags pure))
 ((name . "textual->utf8")
  (signature lambda ((textual? textual)) bytevector?)
  (tags pure))
 ((name . "textual->utf8")
  (signature lambda ((textual? textual) (integer? start)) bytevector?)
  (tags pure))
 ((name . "textual->utf8")
  (signature
   lambda
   ((textual? textual) (integer? start) (integer? end))
   bytevector?)
  (tags pure))
 ((name . "utf8->text")
  (signature lambda ((bytevector? bytevector)) text?)
  (tags pure))
 ((name . "utf8->text")
  (signature lambda ((bytevector? bytevector) (integer? start)) text?)
  (tags pure))
 ((name . "utf8->text")
  (signature
   lambda
   ((bytevector? bytevector) (integer? start) (integer? end))
   text?)
  (tags pure))
 ((name . "textual->utf16")
  (signature lambda ((textual? textual)) bytevector?)
  (tags pure))
 ((name . "textual->utf16")
  (signature lambda ((textual? textual) (integer? start)) bytevector?)
  (tags pure))
 ((name . "textual->utf16")
  (signature
   lambda
   ((textual? textual) (integer? start) (integer? end))
   bytevector?)
  (tags pure))
 ((name . "utf16->text")
  (signature lambda ((bytevector? bytevector)) text?)
  (tags pure))
 ((name . "utf16->text")
  (signature lambda ((bytevector? bytevector) (integer? start)) text?)
  (tags pure))
 ((name . "utf16->text")
  (signature
   lambda
   ((bytevector? bytevector) (integer? start) (integer? end))
   text?)
  (tags pure))
 ((name . "textual->utf16be")
  (signature lambda ((textual? textual)) bytevector?)
  (tags pure))
 ((name . "textual->utf16be")
  (signature lambda ((textual? textual) (integer? start)) bytevector?)
  (tags pure))
 ((name . "textual->utf16be")
  (signature
   lambda
   ((textual? textual) (integer? start) (integer? end))
   bytevector?)
  (tags pure))
 ((name . "utf16be->text")
  (signature lambda ((bytevector? bytevector)) text?)
  (tags pure))
 ((name . "utf16be->text")
  (signature lambda ((bytevector? bytevector) (integer? start)) text?)
  (tags pure))
 ((name . "utf16be->text")
  (signature
   lambda
   ((bytevector? bytevector) (integer? start) (integer? end))
   text?)
  (tags pure))
 ((name . "textual->utf16le")
  (signature lambda ((textual? textual)) bytevector?)
  (tags pure))
 ((name . "textual->utf16le")
  (signature lambda ((textual? textual) (integer? start)) bytevector?)
  (tags pure))
 ((name . "textual->utf16le")
  (signature
   lambda
   ((textual? textual) (integer? start) (integer? end))
   bytevector?)
  (tags pure))
 ((name . "utf16le->text")
  (signature lambda ((bytevector? bytevector)) text?)
  (tags pure))
 ((name . "utf16le->text")
  (signature lambda ((bytevector? bytevector) (integer? start)) text?)
  (tags pure))
 ((name . "utf16le->text")
  (signature
   lambda
   ((bytevector? bytevector) (integer? start) (integer? end))
   text?)
  (tags pure))
 ((name . "text-length")
  (signature lambda ((text? text)) integer?)
  (tags pure))
 ((name . "text-ref")
  (signature lambda ((text? text) (integer? idx)) char?)
  (tags pure))
 ((name . "textual-length")
  (signature lambda ((textual? textual)) integer?)
  (tags pure))
 ((name . "textual-ref")
  (signature lambda ((textual? text) (integer? idx)) char?)
  (tags pure))
 ((name . "subtext")
  (signature lambda ((text? text) (integer? start) (integer? end)) text?)
  (tags pure))
 ((name . "subtextual")
  (signature lambda ((textual? text) (integer? start) (integer? end)) text?)
  (tags pure))
 ((name . "textual-copy")
  (signature lambda ((textual? textual)) text?)
  (tags pure))
 ((name . "textual-copy")
  (signature lambda ((textual? textual) (integer? start)) text?)
  (tags pure))
 ((name . "textual-copy")
  (signature lambda ((textual? textual) (integer? start) (integer? end)) text?)
  (tags pure))
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
  (tags pure))
 ((name . "textual-pad")
  (signature lambda ((textual? textual) (integer? len)) text?)
  (tags pure))
 ((name . "textual-pad")
  (signature lambda ((textual? textual) (integer? len) (char? char)) text?)
  (tags pure))
 ((name . "textual-pad")
  (signature
   lambda
   ((textual? textual) (integer? len) (char? char) (integer? start))
   text?)
  (tags pure))
 ((name . "textual-pad")
  (signature
   lambda
   ((textual? textual)
    (integer? len)
    (char? char)
    (integer? start)
    (integer? end))
   text?)
  (tags pure))
 ((name . "textual-pad-right")
  (signature lambda ((textual? textual) (integer? len)) text?)
  (tags pure))
 ((name . "textual-pad-right")
  (signature lambda ((textual? textual) (integer? len) (char? char)) text?)
  (tags pure))
 ((name . "textual-pad-right")
  (signature
   lambda
   ((textual? textual) (integer? len) (char? char) (integer? start))
   text?)
  (tags pure))
 ((name . "textual-pad-right")
  (signature
   lambda
   ((textual? textual)
    (integer? len)
    (char? char)
    (integer? start)
    (integer? end))
   text?)
  (tags pure))
 ((name . "textual-trim")
  (signature lambda ((textual? textual)) text?)
  (tags pure))
 ((name . "textual-trim")
  (signature lambda ((textual? textual) (procedure? pred)) text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-trim")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start))
   text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-trim")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start) (integer? end))
   text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-trim-right")
  (signature lambda ((textual? textual)) text?)
  (tags pure))
 ((name . "textual-trim-right")
  (signature lambda ((textual? textual) (procedure? pred)) text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-trim-right")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start))
   text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-trim-right")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start) (integer? end))
   text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-trim-both")
  (signature lambda ((textual? textual)) text?)
  (tags pure))
 ((name . "textual-trim-both")
  (signature lambda ((textual? textual) (procedure? pred)) text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-trim-both")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start))
   text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-trim-both")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start) (integer? end))
   text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-replace")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
   text?)
  (tags pure))
 ((name . "textual-replace")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   text?)
  (tags pure))
 ((name . "textual-replace")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   text?)
  (tags pure))
 ((name . "textual=?")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (textual? textual3) ...)
   boolean?)
  (tags pure))
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
  (tags pure))
 ((name . "textual-ci=?")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (textual? textual3) ...)
   boolean?)
  (tags pure))
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
  (tags pure))
 ((name . "textual-prefix-length")
  (signature lambda ((textual? textual1) (textual? textual2)) integer?)
  (tags pure))
 ((name . "textual-prefix-length")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1))
   integer?)
  (tags pure))
 ((name . "textual-prefix-length")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
   integer?)
  (tags pure))
 ((name . "textual-prefix-length")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   integer?)
  (tags pure))
 ((name . "textual-prefix-length")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   integer?)
  (tags pure))
 ((name . "textual-suffix-length")
  (signature lambda ((textual? textual1) (textual? textual2)) integer?)
  (tags pure))
 ((name . "textual-suffix-length")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1))
   integer?)
  (tags pure))
 ((name . "textual-suffix-length")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
   integer?)
  (tags pure))
 ((name . "textual-suffix-length")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   integer?)
  (tags pure))
 ((name . "textual-suffix-length")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   integer?)
  (tags pure))
 ((name . "textual-prefix?")
  (signature lambda ((textual? textual1) (textual? textual2)) boolean?)
  (tags pure))
 ((name . "textual-prefix?")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1))
   boolean?)
  (tags pure))
 ((name . "textual-prefix?")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
   boolean?)
  (tags pure))
 ((name . "textual-prefix?")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   boolean?)
  (tags pure))
 ((name . "textual-prefix?")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   boolean?)
  (tags pure))
 ((name . "textual-suffix?")
  (signature lambda ((textual? textual1) (textual? textual2)) boolean?)
  (tags pure))
 ((name . "textual-suffix?")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1))
   boolean?)
  (tags pure))
 ((name . "textual-suffix?")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
   boolean?)
  (tags pure))
 ((name . "textual-suffix?")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   boolean?)
  (tags pure))
 ((name . "textual-suffix?")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   boolean?)
  (tags pure))
 ((name . "textual-index")
  (signature lambda ((textual? textual) (procedure? pred)) (or #f integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-index")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start))
   (or #f integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-index")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start) (integer? end))
   (or #f integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-index-right")
  (signature lambda ((textual? textual) (procedure? pred)) (or #f integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-index-right")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start))
   (or #f integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-index-right")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start) (integer? end))
   (or #f integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-skip")
  (signature lambda ((textual? textual) (procedure? pred)) (or #f integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-skip")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start))
   (or #f integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-skip")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start) (integer? end))
   (or #f integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-skip-right")
  (signature lambda ((textual? textual) (procedure? pred)) (or #f integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-skip-right")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start))
   (or #f integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-skip-right")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start) (integer? end))
   (or #f integer?))
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-contains")
  (signature lambda ((textual? textual1) (textual? textual2)) (or #f integer?))
  (tags pure))
 ((name . "textual-contains")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1))
   (or #f integer?))
  (tags pure))
 ((name . "textual-contains")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
   (or #f integer?))
  (tags pure))
 ((name . "textual-contains")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   (or #f integer?))
  (tags pure))
 ((name . "textual-contains")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   (or #f integer?))
  (tags pure))
 ((name . "textual-contains-right")
  (signature lambda ((textual? textual1) (textual? textual2)) (or #f integer?))
  (tags pure))
 ((name . "textual-contains-right")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1))
   (or #f integer?))
  (tags pure))
 ((name . "textual-contains-right")
  (signature
   lambda
   ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1))
   (or #f integer?))
  (tags pure))
 ((name . "textual-contains-right")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2))
   (or #f integer?))
  (tags pure))
 ((name . "textual-contains-right")
  (signature
   lambda
   ((textual? textual1)
    (textual? textual2)
    (integer? start1)
    (integer? end1)
    (integer? start2)
    (integer? end2))
   (or #f integer?))
  (tags pure))
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
  (tags pure))
 ((name . "textual-append")
  (signature lambda ((textual? textual) ...) text?)
  (tags pure))
 ((name . "textual-concatenate")
  (signature lambda ((list? textual-list)) text?)
  (tags pure))
 ((name . "textual-concatenate-reverse")
  (signature lambda ((list? textual-list)) text?)
  (tags pure))
 ((name . "textual-concatenate-reverse")
  (signature lambda ((list? textual-list) (textual? final-textual)) text?)
  (tags pure))
 ((name . "textual-concatenate-reverse")
  (signature
   lambda
   ((list? textual-list) (textual? final-textual) (integer? end))
   text?)
  (tags pure))
 ((name . "textual-join")
  (signature lambda ((list? textual-list)) text?)
  (tags pure))
 ((name . "textual-join")
  (signature lambda ((list? textual-list) (textual? delimiter)) text?)
  (tags pure))
 ((name . "textual-join")
  (signature
   lambda
   ((list? textual-list) (textual? delimiter) (symbol? grammar))
   text?)
  (tags pure)
  (spec-values
   (grammar
    ("'infix"
     "infix or separator grammar: insert the delimiter between list elements. An empty list will produce an empty text.")
    ("'strict-infix"
     "same as 'infix if the textual-list is non-empty, but will signal an error if given an empty list. (This avoids an ambiguity)")
    ("'suffix"
     "suffix or terminator grammar: insert the delimiter after every list element.")
    ("'prefix"
     "prefix grammar: insert the delimiter before every list element."))))
 ((name . "textual-fold")
  (signature lambda ((procedure? kons) knil (textual? textual)) *)
  (subsigs (kons (lambda ((char? char) state) *)))
  (tags pure))
 ((name . "textual-fold")
  (signature
   lambda
   ((procedure? kons) knil (textual? textual) (integer? start))
   *)
  (subsigs (kons (lambda ((char? char) state) *)))
  (tags pure))
 ((name . "textual-fold")
  (signature
   lambda
   ((procedure? kons) knil (textual? textual) (integer? start) (integer? end))
   *)
  (subsigs (kons (lambda ((char? char) state) *)))
  (tags pure))
 ((name . "textual-fold-right")
  (signature lambda ((procedure? kons) knil (textual? textual)) *)
  (subsigs (kons (lambda ((char? char) state) *)))
  (tags pure))
 ((name . "textual-fold-right")
  (signature
   lambda
   ((procedure? kons) knil (textual? textual) (integer? start))
   *)
  (subsigs (kons (lambda ((char? char) state) *)))
  (tags pure))
 ((name . "textual-fold-right")
  (signature
   lambda
   ((procedure? kons) knil (textual? textual) (integer? start) (integer? end))
   *)
  (subsigs (kons (lambda ((char? char) state) *)))
  (tags pure))
 ((name . "textual-map")
  (signature
   lambda
   ((procedure? proc) (textual? textual1) (textual? textual2) ...)
   text?)
  (subsigs
   (proc (lambda ((char? char1) (char? char2) ...) (or textual? char?))))
  (tags pure))
 ((name . "textual-for-each")
  (signature
   lambda
   ((procedure? proc) (textual? textual1) (textual? textual2) ...)
   undefined)
  (subsigs (proc (lambda ((char? char1) (char? char2) ...) undefined))))
 ((name . "textual-map-index")
  (signature lambda ((procedure? proc) (textual? textual)) text?)
  (subsigs (proc (lambda ((char? char)) (or textual? char?))))
  (tags pure))
 ((name . "textual-map-index")
  (signature
   lambda
   ((procedure? proc) (textual? textual) (integer? start))
   text?)
  (subsigs (proc (lambda ((char? char)) (or textual? char?))))
  (tags pure))
 ((name . "textual-map-index")
  (signature
   lambda
   ((procedure? proc) (textual? textual) (integer? start) (integer? end))
   text?)
  (subsigs (proc (lambda ((char? char)) (or textual? char?))))
  (tags pure))
 ((name . "textual-for-each-index")
  (signature lambda ((procedure? proc) (textual? textual)) undefined)
  (subsigs (proc (lambda ((char? char)) undefined))))
 ((name . "textual-for-each-index")
  (signature
   lambda
   ((procedure? proc) (textual? textual) (integer? start))
   undefined)
  (subsigs (proc (lambda ((char? char)) undefined))))
 ((name . "textual-for-each-index")
  (signature
   lambda
   ((procedure? proc) (textual? textual) (integer? start) (integer? end))
   undefined)
  (subsigs (proc (lambda ((char? char)) undefined))))
 ((name . "textual-count")
  (signature lambda ((textual? textual) (procedure? pred)) integer?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-count")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start))
   integer?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-count")
  (signature
   lambda
   ((textual? textual) (procedure? pred) (integer? start) (integer? end))
   integer?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-filter")
  (signature lambda ((procedure? pred) (textual? textual)) text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-filter")
  (signature
   lambda
   ((procedure? pred) (textual? textual) (integer? start))
   text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-filter")
  (signature
   lambda
   ((procedure? pred) (textual? textual) (integer? start) (integer? end))
   text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-remove")
  (signature lambda ((procedure? pred) (textual? textual)) text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-remove")
  (signature
   lambda
   ((procedure? pred) (textual? textual) (integer? start))
   text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-remove")
  (signature
   lambda
   ((procedure? pred) (textual? textual) (integer? start) (integer? end))
   text?)
  (subsigs (pred (lambda ((char? char)) boolean?)))
  (tags pure))
 ((name . "textual-replicate")
  (signature lambda ((textual? textual) (integer? from) (integer? to)) text?)
  (tags pure))
 ((name . "textual-replicate")
  (signature
   lambda
   ((textual? textual) (integer? from) (integer? to) (integer? start))
   text?)
  (tags pure))
 ((name . "textual-replicate")
  (signature
   lambda
   ((textual? textual)
    (integer? from)
    (integer? to)
    (integer? start)
    (integer? end))
   text?)
  (tags pure))
 ((name . "textual-split")
  (signature lambda ((textual? textual) (textual? delimiter)) list?)
  (tags pure))
 ((name . "textual-split")
  (signature
   lambda
   ((textual? textual) (textual? delimiter) (symbol? grammar))
   list?)
  (tags pure)
  (spec-values
   (grammar
    ("'infix" "empty textual produces empty list")
    ("'strict-infix" "empty textual signals an error")
    ("'suffix" "leading empty text is suppressed")
    ("'prefix" "trailing empty text is suppressed"))))
 ((name . "textual-split")
  (signature
   lambda
   ((textual? textual)
    (textual? delimiter)
    (symbol? grammar)
    ((or #f integer?) limit))
   list?)
  (tags pure)
  (spec-values
   (grammar
    ("'infix" "empty textual produces empty list")
    ("'strict-infix" "empty textual signals an error")
    ("'suffix" "leading empty text is suppressed")
    ("'prefix" "trailing empty text is suppressed"))))
 ((name . "textual-split")
  (signature
   lambda
   ((textual? textual)
    (textual? delimiter)
    (symbol? grammar)
    ((or #f integer?) limit)
    (integer? start))
   list?)
  (tags pure)
  (spec-values
   (grammar
    ("'infix" "empty textual produces empty list")
    ("'strict-infix" "empty textual signals an error")
    ("'suffix" "leading empty text is suppressed")
    ("'prefix" "trailing empty text is suppressed"))))
 ((name . "textual-split")
  (signature
   lambda
   ((textual? textual)
    (textual? delimiter)
    (symbol? grammar)
    ((or #f integer?) limit)
    (integer? start)
    (integer? end))
   list?)
  (tags pure)
  (spec-values
   (grammar
    ("'infix" "empty textual produces empty list")
    ("'strict-infix" "empty textual signals an error")
    ("'suffix" "leading empty text is suppressed")
    ("'prefix" "trailing empty text is suppressed")))))
