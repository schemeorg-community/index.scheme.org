(((name . show)
  (signature lambda ((#f port) (formatter fmt) ...) string?)
  (tags pure))
 ((name . show)
  (signature
   lambda
   (((or output-port? boolean?) port) (formatter fmt) ...)
   undefined))
 ((name . displayed) (signature lambda (obj) formatter) (tags pure))
 ((name . written) (signature lambda (obj) formatter) (tags pure))
 ((name . written-simply) (signature lambda (obj) formatter) (tags pure))
 ((name . pretty) (signature lambda (obj) formatter) (tags pure))
 ((name . pretty-simply) (signature lambda (obj) formatter) (tags pure))
 ((name . escaped) (signature lambda ((string? str)) formatter) (tags pure))
 ((name . escaped)
  (signature lambda ((string? str) (char? quote-ch)) formatter)
  (tags pure))
 ((name . escaped)
  (signature lambda ((string? str) (char? quote-ch) (char? esc-ch)) formatter)
  (tags pure))
 ((name . escaped)
  (signature
   lambda
   ((string? str) (char? quote-ch) (char? esc-ch) (procedure? renamer))
   formatter)
  (tags pure)
  (subsigs (renamer (lambda ((char? c)) char?))))
 ((name . maybe-escaped)
  (signature lambda ((string? str) (procedure? pred)) formatter)
  (tags pure)
  (subsigs (pred (lambda ((char? c)) boolean?))))
 ((name . maybe-escaped)
  (signature
   lambda
   ((string? str) (procedure? pred) (char? quote-ch))
   formatter)
  (tags pure)
  (subsigs (pred (lambda ((char? c)) boolean?))))
 ((name . maybe-escaped)
  (signature
   lambda
   ((string? str) (procedure? pred) (char? quote-ch) (char? esc-ch))
   formatter)
  (tags pure)
  (subsigs (pred (lambda ((char? c)) boolean?))))
 ((name . maybe-escaped)
  (signature
   lambda
   ((string? str)
    (procedure? pred)
    (char? quote-ch)
    (char? esc-ch)
    (procedure? renamer))
   formatter)
  (tags pure)
  (subsigs
   (pred (lambda ((char? c)) boolean?))
   (renamer (lambda ((char? c)) char?))))
 ((name . numeric) (signature lambda ((number? num)) formatter) (tags pure))
 ((name . numeric)
  (signature lambda ((number? num) (integer? radix)) formatter)
  (tags pure))
 ((name . numeric)
  (signature
   lambda
   ((number? num) (integer? radix) (integer? precision))
   formatter)
  (tags pure))
 ((name . numeric)
  (signature
   lambda
   ((number? num)
    (integer? radix)
    (integer? precision)
    ((or boolean? pair?) sign))
   formatter)
  (tags pure))
 ((name . numeric)
  (signature
   lambda
   ((number? num)
    (integer? radix)
    (integer? precision)
    ((or boolean? pair?) sign)
    (integer? comma))
   formatter)
  (tags pure))
 ((name . numeric)
  (signature
   lambda
   ((number? num)
    (integer? radix)
    (integer? precision)
    ((or boolean? pair?) sign)
    (integer? comma)
    (char? comma-sep))
   formatter)
  (tags pure))
 ((name . numeric)
  (signature
   lambda
   ((number? num)
    (integer? radix)
    (integer? precision)
    ((or boolean? pair?) sign)
    (integer? comma)
    (char? comma-sep)
    (char? decimal-sep))
   formatter)
  (tags pure))
 ((name . numeric/comma)
  (signature lambda ((number? num)) formatter)
  (tags pure))
 ((name . numeric/comma)
  (signature lambda ((number? num) (integer? radix)) formatter)
  (tags pure))
 ((name . numeric/comma)
  (signature
   lambda
   ((number? num) (integer? radix) (integer? precision))
   formatter)
  (tags pure))
 ((name . numeric/comma)
  (signature
   lambda
   ((number? num)
    (integer? radix)
    (integer? precision)
    ((or boolean? pair?) sign))
   formatter)
  (tags pure))
 ((name . numeric/si) (signature lambda ((number? num)) formatter) (tags pure))
 ((name . numeric/si)
  (signature lambda ((number? num) (integer? base)) formatter)
  (tags pure))
 ((name . numeric/si)
  (signature
   lambda
   ((number? num) (integer? base) (string? separator))
   formatter)
  (tags pure))
 ((name . numeric/fitted)
  (signature lambda ((number? num)) formatter)
  (tags pure))
 ((name . numeric/fitted)
  (signature lambda ((number? num) (integer? radix)) formatter)
  (tags pure))
 ((name . numeric/fitted)
  (signature
   lambda
   ((number? num) (integer? radix) (integer? precision))
   formatter)
  (tags pure))
 ((name . numeric/fitted)
  (signature
   lambda
   ((number? num)
    (integer? radix)
    (integer? precision)
    ((or boolean? pair?) sign))
   formatter)
  (tags pure))
 ((name . numeric/fitted)
  (signature
   lambda
   ((number? num)
    (integer? radix)
    (integer? precision)
    ((or boolean? pair?) sign)
    (integer? comma))
   formatter)
  (tags pure))
 ((name . numeric/fitted)
  (signature
   lambda
   ((number? num)
    (integer? radix)
    (integer? precision)
    ((or boolean? pair?) sign)
    (integer? comma)
    (char? comma-sep))
   formatter)
  (tags pure))
 ((name . numeric/fitted)
  (signature
   lambda
   ((number? num)
    (integer? radix)
    (integer? precision)
    ((or boolean? pair?) sign)
    (integer? comma)
    (char? comma-sep)
    (char? decimal-sep))
   formatter)
  (tags pure))
 ((name . nl) (signature value formatter))
 ((name . fl) (signature value formatter))
 ((name . space-to)
  (signature lambda ((integer? column)) formatter)
  (tags pure))
 ((name . tab-to) (signature lambda () formatter) (tags pure))
 ((name . tab-to)
  (signature lambda ((integer? tab-width)) formatter)
  (tags pure))
 ((name . nothing) (signature value formatter))
 ((name . each) (signature lambda ((formatter fmt) ...) formatter) (tags pure))
 ((name . each-in-list)
  (signature lambda ((list? list-of-fmts)) formatter)
  (tags pure))
 ((name . joined)
  (signature lambda ((procedure? mapper) (list? list)) formatter)
  (tags pure)
  (subsigs (mapper (lambda (element) formatter))))
 ((name . joined)
  (signature
   lambda
   ((procedure? mapper) (list? list) ((or string? formatter) separator))
   formatter)
  (tags pure)
  (subsigs (mapper (lambda (element) formatter))))
 ((name . joined/prefix)
  (signature lambda ((procedure? mapper) (list? list)) formatter)
  (tags pure)
  (subsigs (mapper (lambda (element) formatter))))
 ((name . joined/prefix)
  (signature
   lambda
   ((procedure? mapper) (list? list) ((or string? formatter) separator))
   formatter)
  (tags pure)
  (subsigs (mapper (lambda (element) formatter))))
 ((name . joined/suffix)
  (signature lambda ((procedure? mapper) (list? list)) formatter)
  (tags pure)
  (subsigs (mapper (lambda (element) formatter))))
 ((name . joined/suffix)
  (signature
   lambda
   ((procedure? mapper) (list? list) ((or string? formatter) separator))
   formatter)
  (tags pure)
  (subsigs (mapper (lambda (element) formatter))))
 ((name . joined/last)
  (signature
   lambda
   ((procedure? mapper) (procedure? last-mapper) (list? list))
   formatter)
  (tags pure)
  (subsigs
   (mapper (lambda (element) formatter))
   (last-mapper (lambda (element) formatter))))
 ((name . joined/last)
  (signature
   lambda
   ((procedure? mapper)
    (procedure? last-mapper)
    (list? list)
    ((or string? formatter) separator))
   formatter)
  (tags pure)
  (subsigs
   (mapper (lambda (element) formatter))
   (last-mapper (lambda (element) formatter))))
 ((name . joined/dot)
  (signature
   lambda
   ((procedure? mapper) (procedure? dot-mapper) ((or list? dotted-list?) list))
   formatter)
  (tags pure)
  (subsigs
   (mapper (lambda (element) formatter))
   (dot-mapper (lambda (tail) formatter))))
 ((name . joined/dot)
  (signature
   lambda
   ((procedure? mapper)
    (procedure? dot-mapper)
    ((or list? dotted-list?) list)
    ((or string? formatter) separator))
   formatter)
  (tags pure)
  (subsigs
   (mapper (lambda (element) formatter))
   (dot-mapper (lambda (tail) formatter))))
 ((name . joined/range)
  (signature lambda ((procedure? mapper) (integer? start)) formatter)
  (tags pure)
  (subsigs (mapper (lambda ((integer? value)) formatter))))
 ((name . joined/range)
  (signature
   lambda
   ((procedure? mapper) (integer? start) ((or integer? #f) end))
   formatter)
  (tags pure)
  (subsigs (mapper (lambda ((integer? value)) formatter))))
 ((name . joined/range)
  (signature
   lambda
   ((procedure? mapper)
    (integer? start)
    ((or integer? #f) end)
    ((or string? formatter) separator))
   formatter)
  (tags pure)
  (subsigs (mapper (lambda ((integer? value)) formatter))))
 ((name . padded)
  (signature lambda ((integer? width) (formatter fmt) ...) formatter)
  (tags pure))
 ((name . padded/right)
  (signature lambda ((integer? width) (formatter fmt) ...) formatter)
  (tags pure))
 ((name . padded/both)
  (signature lambda ((integer? width) (formatter fmt) ...) formatter)
  (tags pure))
 ((name . trimmed)
  (signature lambda ((integer? width) (formatter fmt) ...) formatter)
  (tags pure))
 ((name . trimmed/right)
  (signature lambda ((integer? width) (formatter fmt) ...) formatter)
  (tags pure))
 ((name . trimmed/both)
  (signature lambda ((integer? width) (formatter fmt) ...) formatter)
  (tags pure))
 ((name . trimmed/lazy)
  (signature lambda ((integer? width) (formatter fmt) ...) formatter)
  (tags pure))
 ((name . fitted)
  (signature lambda ((integer? width) (formatter fmt) ...) formatter)
  (tags pure))
 ((name . fitted/right)
  (signature lambda ((integer? width) (formatter fmt) ...) formatter)
  (tags pure))
 ((name . fitted/both)
  (signature lambda ((integer? width) (formatter fmt) ...) formatter)
  (tags pure))
 ((name . columnar)
  (signature
   lambda
   (((or formatter string? symbol? number?) column) ...)
   formatter)
  (tags pure))
 ((name . tabular)
  (signature
   lambda
   (((or formatter string? symbol? number?) column) ...)
   formatter)
  (tags pure))
 ((name . wrapped)
  (signature lambda ((formatter fmt) ...) formatter)
  (tags pure))
 ((name . wrapped/list)
  (signature lambda ((list? list-of-strings) ...) formatter)
  (tags pure))
 ((name . wrapped/char)
  (signature lambda ((formatter fmt) ...) formatter)
  (tags pure))
 ((name . justified)
  (signature lambda ((formatter fmt) ...) formatter)
  (tags pure))
 ((name . from-file) (signature lambda ((string? pathname)) formatter))
 ((name . line-numbers) (signature lambda () formatter) (tags pure))
 ((name . line-numbers)
  (signature lambda ((integer? start)) formatter)
  (tags pure))
 ((name . as-red) (signature lambda ((formatter fmt) ...) formatter))
 ((name . as-blue) (signature lambda ((formatter fmt) ...) formatter))
 ((name . as-green) (signature lambda ((formatter fmt) ...) formatter))
 ((name . as-cyan) (signature lambda ((formatter fmt) ...) formatter))
 ((name . as-yellow) (signature lambda ((formatter fmt) ...) formatter))
 ((name . as-magenta) (signature lambda ((formatter fmt) ...) formatter))
 ((name . as-white) (signature lambda ((formatter fmt) ...) formatter))
 ((name . as-black) (signature lambda ((formatter fmt) ...) formatter))
 ((name . as-bold) (signature lambda ((formatter fmt) ...) formatter))
 ((name . as-underline) (signature lambda ((formatter fmt) ...) formatter))
 ((name . as-unicode) (signature lambda ((formatter fmt) ...) formatter))
 ((name . unicode-terminal-width)
  (signature lambda ((string? str)) integer?)
  (tags pure))
 ((name . fn)
  (signature syntax-rules () ((_ (binding ...) expr ... fmt) formatter))
  (subsigs (binding (id state-var) id))
  (syntax-param-signatures (state-var formatter-variable)))
 ((name . with)
  (signature syntax-rules () ((_ ((state-var value) ...) fmt ...)))
  (syntax-param-signatures (state-var formatter-variable)))
 ((name . with!)
  (signature syntax-rules () ((_ (state-var value) ...)))
  (syntax-param-signatures (state-var formatter-variable)))
 ((name . forked)
  (signature lambda ((formatter fmt1) (formatter fmt2)) formatter)
  (tags pure))
 ((name . call-with-output)
  (signature lambda ((formatter fmt) (procedure? mapper)) formatter)
  (tags pure)
  (subsigs (mapper (lambda ((string? result-string)) formatter))))
 ((name . port) (signature value formatter-variable))
 ((name . row) (signature value formatter-variable))
 ((name . col) (signature value formatter-variable))
 ((name . width) (signature value formatter-variable))
 ((name . output) (signature value formatter-variable))
 ((name . writer) (signature value formatter-variable))
 ((name . string-width) (signature value formatter-variable))
 ((name . pad-char) (signature value formatter-variable))
 ((name . ellipsis) (signature value formatter-variable))
 ((name . radix) (signature value formatter-variable))
 ((name . precision) (signature value formatter-variable))
 ((name . decimal-sep) (signature value formatter-variable))
 ((name . decimal-align) (signature value formatter-variable))
 ((name . word-separator?) (signature value formatter-variable)))