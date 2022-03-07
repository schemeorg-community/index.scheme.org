(
 
 (text?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (textual?))
 
 (textual?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (textual-null?
   (lambda ((textual? textual)) boolean?)
   (pure))
 
 (textual-every
   (lambda ((procedure? pred) (textual? textual)) *)
   (pure)
   ((pred (lambda ((char? char)) *))))
 
 (textual-every
   (lambda ((procedure? pred) (textual? textual) (integer? start)) *)
   (pure)
   ((pred (lambda ((char? char)) *))))
 
 (textual-every
   (lambda ((procedure? pred) (textual? textual) (integer? start) (integer? end)) *)
   (pure)
   ((pred (lambda ((char? char)) *))))
 
 (textual-any
   (lambda ((procedure? pred) (textual? textual)) *)
   (pure)
   ((pred (lambda ((char? char)) *))))
 
 (textual-any
   (lambda ((procedure? pred) (textual? textual) (integer? start)) *)
   (pure)
   ((pred (lambda ((char? char)) *))))
 
 (textual-any
   (lambda ((procedure? pred) (textual? textual) (integer? start) (integer? end)) *)
   (pure)
   ((pred (lambda ((char? char)) *))))
 
 (make-text
   (lambda ((integer? len) (char? char)) text?)
   (pure))
 
 (text
   (lambda ((char? char) ...) text?)
   (pure))
 
 (text-tabulate
   (lambda ((procedure? proc) (integer? len)) text?)
   (pure)
   ((proc (lambda ((integer? k)) char?))))

 (text-unfold
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed) text?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) (or char? string? text?)))
    (success (lambda (seed) *))))

 (text-unfold
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed (textual? base)) text?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) (or char? string? text?)))
    (success (lambda (seed) *))))

 (text-unfold
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed (textual? base) (procedure? make-final)) text?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) (or char? string? text?)))
    (success (lambda (seed) *))
    (make-final (lambda (seed) (or char? string? text?)))))
 
 (text-unfold-right
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed) text?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) (or char? string? text?)))
    (success (lambda (seed) *))))

 (text-unfold-right
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed (textual? base)) text?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) (or char? string? text?)))
    (success (lambda (seed) *))))

 (text-unfold-right
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed (textual? base) (procedure? make-final)) text?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) (or char? string? text?)))
    (success (lambda (seed) *))
    (make-final (lambda (seed) (or char? string? text?)))))
 
 (textual->text
   (lambda ((textual? textual)) text?)
   (pure))
 
 (textual->string
   (lambda ((textual? textual)) string?)
   (pure))
 
 (textual->string
   (lambda ((textual? textual) (integer? start)) string?)
   (pure))
 
 (textual->string
   (lambda ((textual? textual) (integer? start) (integer? end)) string?)
   (pure))
 
 (textual->vector
   (lambda ((textual? textual)) vector?)
   (pure))
 
 (textual->vector
   (lambda ((textual? textual) (integer? start)) vector?)
   (pure))
 
 (textual->vector
   (lambda ((textual? textual) (integer? start) (integer? end)) vector?)
   (pure))
 
 (textual->list
   (lambda ((textual? textual)) list?)
   (pure))
 
 (textual->list
   (lambda ((textual? textual) (integer? start)) list?)
   (pure))
 
 (textual->list
   (lambda ((textual? textual) (integer? start) (integer? end)) list?)
   (pure))
 
 (string->text
   (lambda ((string? string)) text?)
   (pure))
 
 (string->text
   (lambda ((string? string) (integer? start)) text?)
   (pure))
 
 (string->text
   (lambda ((string? string) (integer? start) (integer? end)) text?)
   (pure))
 
 (vector->text
   (lambda ((vector? vector)) text?)
   (pure))
 
 (vector->text
   (lambda ((vector? vector) (integer? start)) text?)
   (pure))
 
 (vector->text
   (lambda ((vector? vector) (integer? start) (integer? end)) text?)
   (pure))
 
 (list->text
   (lambda ((list? list)) text?)
   (pure))
 
 (list->text
   (lambda ((list? list) (integer? start)) text?)
   (pure))
 
 (list->text
   (lambda ((list? list) (integer? start) (integer? end)) text?)
   (pure))
 
 (reverse-list->text
   (lambda ((list? char-list)) text?)
   (pure))
 
 (textual->utf8
   (lambda ((textual? textual)) bytevector?)
   (pure))
 
 (textual->utf8
   (lambda ((textual? textual) (integer? start)) bytevector?)
   (pure))
 
 (textual->utf8
   (lambda ((textual? textual) (integer? start) (integer? end)) bytevector?)
   (pure))
 
 (utf8->text
   (lambda ((bytevector? bytevector)) text?)
   (pure))
 
 (utf8->text
   (lambda ((bytevector? bytevector) (integer? start)) text?)
   (pure))
 
 (utf8->text
   (lambda ((bytevector? bytevector) (integer? start) (integer? end)) text?)
   (pure))
 
 (textual->utf16
   (lambda ((textual? textual)) bytevector?)
   (pure))
 
 (textual->utf16
   (lambda ((textual? textual) (integer? start)) bytevector?)
   (pure))
 
 (textual->utf16
   (lambda ((textual? textual) (integer? start) (integer? end)) bytevector?)
   (pure))
 
 (utf16->text
   (lambda ((bytevector? bytevector)) text?)
   (pure))
 
 (utf16->text
   (lambda ((bytevector? bytevector) (integer? start)) text?)
   (pure))
 
 (utf16->text
   (lambda ((bytevector? bytevector) (integer? start) (integer? end)) text?)
   (pure))
 
 (textual->utf16be
   (lambda ((textual? textual)) bytevector?)
   (pure))
 
 (textual->utf16be
   (lambda ((textual? textual) (integer? start)) bytevector?)
   (pure))
 
 (textual->utf16be
   (lambda ((textual? textual) (integer? start) (integer? end)) bytevector?)
   (pure))
 
 (utf16be->text
   (lambda ((bytevector? bytevector)) text?)
   (pure))
 
 (utf16be->text
   (lambda ((bytevector? bytevector) (integer? start)) text?)
   (pure))
 
 (utf16be->text
   (lambda ((bytevector? bytevector) (integer? start) (integer? end)) text?)
   (pure))
 
 (textual->utf16le
   (lambda ((textual? textual)) bytevector?)
   (pure))
 
 (textual->utf16le
   (lambda ((textual? textual) (integer? start)) bytevector?)
   (pure))
 
 (textual->utf16le
   (lambda ((textual? textual) (integer? start) (integer? end)) bytevector?)
   (pure))
 
 (utf16le->text
   (lambda ((bytevector? bytevector)) text?)
   (pure))
 
 (utf16le->text
   (lambda ((bytevector? bytevector) (integer? start)) text?)
   (pure))
 
 (utf16le->text
   (lambda ((bytevector? bytevector) (integer? start) (integer? end)) text?)
   (pure))
 
 (text-length
   (lambda ((text? text)) integer?)
   (pure))
 
 (text-ref
   (lambda ((text? text) (integer? idx)) char?)
   (pure))
 
 (textual-length
   (lambda ((textual? textual)) integer?)
   (pure))
 
 (textual-ref
   (lambda ((textual? text) (integer? idx)) char?)
   (pure))
 
 (subtext
   (lambda ((text? text) (integer? start) (integer? end)) text?)
   (pure))
 
 (subtextual
   (lambda ((textual? text) (integer? start) (integer? end)) text?)
   (pure))
 
 (textual-copy
   (lambda ((textual? textual)) text?)
   (pure))
 
 (textual-copy
   (lambda ((textual? textual) (integer? start)) text?)
   (pure))
 
 (textual-copy
   (lambda ((textual? textual) (integer? start) (integer? end)) text?)
   (pure))
 
 (textual-take
   (lambda ((textual? textual) (integer? nchars)) text?)
   (pure))
 
 (textual-drop
   (lambda ((textual? textual) (integer? nchars)) text?)
   (pure))
 
 (textual-take-right
   (lambda ((textual? textual) (integer? nchars)) text?)
   (pure))
 
 (textual-drop-right
   (lambda ((textual? textual) (integer? nchars)) text?)
   (pure))
 
 (textual-pad
   (lambda ((textual? textual) (integer? len)) text?)
   (pure))
 
 (textual-pad
   (lambda ((textual? textual) (integer? len) (char? char)) text?)
   (pure))
 
 (textual-pad
   (lambda ((textual? textual) (integer? len) (char? char) (integer? start)) text?)
   (pure))
 
 (textual-pad
   (lambda ((textual? textual) (integer? len) (char? char) (integer? start) (integer? end)) text?)
   (pure))
 
 (textual-pad-right
   (lambda ((textual? textual) (integer? len)) text?)
   (pure))
 
 (textual-pad-right
   (lambda ((textual? textual) (integer? len) (char? char)) text?)
   (pure))
 
 (textual-pad-right
   (lambda ((textual? textual) (integer? len) (char? char) (integer? start)) text?)
   (pure))
 
 (textual-pad-right
   (lambda ((textual? textual) (integer? len) (char? char) (integer? start) (integer? end)) text?)
   (pure))
 
 (textual-trim
   (lambda ((textual? textual)) text?)
   (pure))
 
 (textual-trim
   (lambda ((textual? textual) (procedure? pred)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-trim
   (lambda ((textual? textual) (procedure? pred) (integer? start)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-trim
   (lambda ((textual? textual) (procedure? pred) (integer? start) (integer? end)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-trim-right
   (lambda ((textual? textual)) text?)
   (pure))
 
 (textual-trim-right
   (lambda ((textual? textual) (procedure? pred)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-trim-right
   (lambda ((textual? textual) (procedure? pred) (integer? start)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-trim-right
   (lambda ((textual? textual) (procedure? pred) (integer? start) (integer? end)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-trim-both
   (lambda ((textual? textual)) text?)
   (pure))
 
 (textual-trim-both
   (lambda ((textual? textual) (procedure? pred)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-trim-both
   (lambda ((textual? textual) (procedure? pred) (integer? start)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-trim-both
   (lambda ((textual? textual) (procedure? pred) (integer? start) (integer? end)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-replace
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1)) text?)
   (pure))
 
 (textual-replace
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2)) text?)
   (pure))
 
 (textual-replace
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) text?)
   (pure))
 
 (textual=?
   (lambda ((textual? textual1) (textual? textual2) (textual? textual3) ...) boolean?)
   (pure))
 
 (textual<?
   (lambda ((textual? textual1) (textual? textual2) (textual? textual3) ...) boolean?)
   (pure))
 
 (textual>?
   (lambda ((textual? textual1) (textual? textual2) (textual? textual3) ...) boolean?)
   (pure))
 
 (textual<=?
   (lambda ((textual? textual1) (textual? textual2) (textual? textual3) ...) boolean?)
   (pure))
 
 (textual>=?
   (lambda ((textual? textual1) (textual? textual2) (textual? textual3) ...) boolean?)
   (pure))
 
 (textual-ci=?
   (lambda ((textual? textual1) (textual? textual2) (textual? textual3) ...) boolean?)
   (pure))
 
 (textual-ci<?
   (lambda ((textual? textual1) (textual? textual2) (textual? textual3) ...) boolean?)
   (pure))
 
 (textual-ci>?
   (lambda ((textual? textual1) (textual? textual2) (textual? textual3) ...) boolean?)
   (pure))
 
 (textual-ci<=?
   (lambda ((textual? textual1) (textual? textual2) (textual? textual3) ...) boolean?)
   (pure))
 
 (textual-ci>=?
   (lambda ((textual? textual1) (textual? textual2) (textual? textual3) ...) boolean?)
   (pure))
 
 (textual-prefix-length
   (lambda ((textual? textual1) (textual? textual2)) integer?)
   (pure))
 
 (textual-prefix-length
   (lambda ((textual? textual1) (textual? textual2) (integer? start1)) integer?)
   (pure))
 
 (textual-prefix-length
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1)) integer?)
   (pure))
 
 (textual-prefix-length
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2)) integer?)
   (pure))
 
 (textual-prefix-length
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) integer?)
   (pure))

 (textual-suffix-length
   (lambda ((textual? textual1) (textual? textual2)) integer?)
   (pure))
 
 (textual-suffix-length
   (lambda ((textual? textual1) (textual? textual2) (integer? start1)) integer?)
   (pure))
 
 (textual-suffix-length
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1)) integer?)
   (pure))
 
 (textual-suffix-length
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2)) integer?)
   (pure))
 
 (textual-suffix-length
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) integer?)
   (pure))
 
 (textual-prefix?
   (lambda ((textual? textual1) (textual? textual2)) boolean?)
   (pure))
 
 (textual-prefix?
   (lambda ((textual? textual1) (textual? textual2) (integer? start1)) boolean?)
   (pure))
 
 (textual-prefix?
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1)) boolean?)
   (pure))
 
 (textual-prefix?
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2)) boolean?)
   (pure))
 
 (textual-prefix?
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) boolean?)
   (pure))
 
 (textual-suffix?
   (lambda ((textual? textual1) (textual? textual2)) boolean?)
   (pure))
 
 (textual-suffix?
   (lambda ((textual? textual1) (textual? textual2) (integer? start1)) boolean?)
   (pure))
 
 (textual-suffix?
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1)) boolean?)
   (pure))
 
 (textual-suffix?
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2)) boolean?)
   (pure))
 
 (textual-suffix?
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) boolean?)
   (pure))
 
 (textual-index
   (lambda ((textual? textual) (procedure? pred)) (or #f integer?))
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-index
   (lambda ((textual? textual) (procedure? pred) (integer? start)) (or #f integer?))
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-index
   (lambda ((textual? textual) (procedure? pred) (integer? start) (integer? end)) (or #f integer?))
   (pure)
   ((pred (lambda ((char? char)) boolean?))))

 (textual-index-right
   (lambda ((textual? textual) (procedure? pred)) (or #f integer?))
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-index-right
   (lambda ((textual? textual) (procedure? pred) (integer? start)) (or #f integer?))
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-index-right
   (lambda ((textual? textual) (procedure? pred) (integer? start) (integer? end)) (or #f integer?))
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-skip
   (lambda ((textual? textual) (procedure? pred)) (or #f integer?))
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-skip
   (lambda ((textual? textual) (procedure? pred) (integer? start)) (or #f integer?))
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-skip
   (lambda ((textual? textual) (procedure? pred) (integer? start) (integer? end)) (or #f integer?))
   (pure)
   ((pred (lambda ((char? char)) boolean?))))

 (textual-skip-right
   (lambda ((textual? textual) (procedure? pred)) (or #f integer?))
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-skip-right
   (lambda ((textual? textual) (procedure? pred) (integer? start)) (or #f integer?))
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-skip-right
   (lambda ((textual? textual) (procedure? pred) (integer? start) (integer? end)) (or #f integer?))
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-contains
   (lambda ((textual? textual1) (textual? textual2)) (or #f integer?))
   (pure))

 (textual-contains
   (lambda ((textual? textual1) (textual? textual2) (integer? start1)) (or #f integer?))
   (pure))

 (textual-contains
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1)) (or #f integer?))
   (pure))

 (textual-contains
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2)) (or #f integer?))
   (pure))

 (textual-contains
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) (or #f integer?))
   (pure))
 
 (textual-contains-right
   (lambda ((textual? textual1) (textual? textual2)) (or #f integer?))
   (pure))
 
 (textual-contains-right
   (lambda ((textual? textual1) (textual? textual2) (integer? start1)) (or #f integer?))
   (pure))

 (textual-contains-right
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1)) (or #f integer?))
   (pure))

 (textual-contains-right
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2)) (or #f integer?))
   (pure))

 (textual-contains-right
   (lambda ((textual? textual1) (textual? textual2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) (or #f integer?))
   (pure))
 
 (textual-upcase
   (lambda ((textual? textual)) text?)
   (pure))
 
 (textual-downcase
   (lambda ((textual? textual)) text?)
   (pure))
 
 (textual-foldcase
   (lambda ((textual? textual)) text?)
   (pure))
 
 (textual-titlecase
   (lambda ((textual? textual)) text?)
   (pure))
 
 (textual-append
   (lambda ((textual? textual) ...) text?)
   (pure))
 
 (textual-concatenate
   (lambda ((list? textual-list)) text?)
   (pure))
 
 (textual-concatenate-reverse
   (lambda ((list? textual-list)) text?)
   (pure))
 
 (textual-concatenate-reverse
   (lambda ((list? textual-list) (textual? final-textual)) text?)
   (pure))
 
 (textual-concatenate-reverse
   (lambda ((list? textual-list) (textual? final-textual) (integer? end)) text?)
   (pure))
 
 (textual-join
   (lambda ((list? textual-list)) text?)
   (pure))
 
 (textual-join
   (lambda ((list? textual-list) (textual? delimiter)) text?)
   (pure))
 
 (textual-join
   (lambda ((list? textual-list) (textual? delimiter) (symbol? grammar)) text?)
   (pure))
 
 (textual-fold
   (lambda ((procedure? kons) knil (textual? textual)) *)
   (pure)
   ((kons (lambda ((char? char) state) *))))
 
 (textual-fold
   (lambda ((procedure? kons) knil (textual? textual) (integer? start)) *)
   (pure)
   ((kons (lambda ((char? char) state) *))))
 
 (textual-fold
   (lambda ((procedure? kons) knil (textual? textual) (integer? start) (integer? end)) *)
   (pure)
   ((kons (lambda ((char? char) state) *))))
 
 (textual-fold-right
   (lambda ((procedure? kons) knil (textual? textual)) *)
   (pure)
   ((kons (lambda ((char? char) state) *))))
 
 (textual-fold-right
   (lambda ((procedure? kons) knil (textual? textual) (integer? start)) *)
   (pure)
   ((kons (lambda ((char? char) state) *))))
 
 (textual-fold-right
   (lambda ((procedure? kons) knil (textual? textual) (integer? start) (integer? end)) *)
   (pure)
   ((kons (lambda ((char? char) state) *))))
 
 (textual-map
   (lambda ((procedure? proc) (textual? textual1) (textual? textual2) ...) text?)
   (pure)
   ((proc (lambda ((char? char1) (char? char2) ...) (or textual? char?)))))
 
 (textual-for-each
   (lambda ((procedure? proc) (textual? textual1) (textual? textual2) ...) undefined)
   ()
   ((proc (lambda ((char? char1) (char? char2) ...) undefined))))
 
 (textual-map-index
   (lambda ((procedure? proc) (textual? textual)) text?)
   (pure)
   ((proc (lambda ((char? char)) (or textual? char?)))))
 
 (textual-map-index
   (lambda ((procedure? proc) (textual? textual) (integer? start)) text?)
   (pure)
   ((proc (lambda ((char? char)) (or textual? char?)))))

 (textual-map-index
   (lambda ((procedure? proc) (textual? textual) (integer? start) (integer? end)) text?)
   (pure)
   ((proc (lambda ((char? char)) (or textual? char?)))))
 
 (textual-for-each-index
   (lambda ((procedure? proc) (textual? textual)) undefined)
   ()
   ((proc (lambda ((char? char)) undefined))))
 
 (textual-for-each-index
   (lambda ((procedure? proc) (textual? textual) (integer? start)) undefined)
   ()
   ((proc (lambda ((char? char)) undefined))))

 (textual-for-each-index
   (lambda ((procedure? proc) (textual? textual) (integer? start) (integer? end)) undefined)
   ()
   ((proc (lambda ((char? char)) undefined))))
 
 (textual-count
   (lambda ((textual? textual) (procedure? pred)) integer?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-count
   (lambda ((textual? textual) (procedure? pred) (integer? start)) integer?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-count
   (lambda ((textual? textual) (procedure? pred) (integer? start) (integer? end)) integer?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-filter
   (lambda ((procedure? pred) (textual? textual)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-filter
   (lambda ((procedure? pred) (textual? textual) (integer? start)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-filter
   (lambda ((procedure? pred) (textual? textual) (integer? start) (integer? end)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-remove
   (lambda ((procedure? pred) (textual? textual)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-remove
   (lambda ((procedure? pred) (textual? textual) (integer? start)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-remove
   (lambda ((procedure? pred) (textual? textual) (integer? start) (integer? end)) text?)
   (pure)
   ((pred (lambda ((char? char)) boolean?))))
 
 (textual-replicate
   (lambda ((textual? textual) (integer? from) (integer? to)) text?)
   (pure))
 
 (textual-replicate
   (lambda ((textual? textual) (integer? from) (integer? to) (integer? start)) text?)
   (pure))
 
 (textual-replicate
   (lambda ((textual? textual) (integer? from) (integer? to) (integer? start) (integer? end)) text?)
   (pure))
 
 (textual-split
   (lambda ((textual? textual) (textual? delimiter)) list?)
   (pure))
 
 (textual-split
   (lambda ((textual? textual) (textual? delimiter) (symbol? grammar)) list?)
   (pure))
 
 (textual-split
   (lambda ((textual? textual) (textual? delimiter) (symbol? grammar) (integer? limit)) list?)
   (pure))
 
 (textual-split
   (lambda ((textual? textual) (textual? delimiter) (symbol? grammar) (integer? limit) (integer? start)) list?)
   (pure))
 
 (textual-split
   (lambda ((textual? textual) (textual? delimiter) (symbol? grammar) (integer? limit) (integer? start) (integer? end)) list?)
   (pure))
 
 )
