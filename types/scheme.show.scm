(

 (show
   (lambda ((#f port) (formatter fmt) ...) string?)
   (pure))

 (show
   (lambda (((or output-port? boolean?) port) (formatter fmt) ...) undefined)
   ())

 (displayed
   (lambda (obj) formatter)
   (pure))

 (written
   (lambda (obj) formatter)
   (pure))

 (written-simply
   (lambda (obj) formatter)
   (pure))

 (pretty
   (lambda (obj) formatter)
   (pure))

 (pretty-simply
   (lambda (obj) formatter)
   (pure))

 (escaped
   (lambda ((string? str)) formatter)
   (pure))

 (escaped
   (lambda ((string? str) (char? quote-ch)) formatter)
   (pure))

 (escaped
   (lambda ((string? str) (char? quote-ch) (char? esc-ch)) formatter)
   (pure))

 (escaped
   (lambda ((string? str) (char? quote-ch) (char? esc-ch) (procedure? renamer)) formatter)
   (pure)
   ((renamer (lambda (char? c) char?))))

 (maybe-escaped
   (lambda ((string? str) (procedure? pred)) formatter)
   (pure)
   ((pred (lambda (char? c) boolean?))))

 (maybe-escaped
   (lambda ((string? str) (procedure? pred) (char? quote-ch)) formatter)
   (pure)
   ((pred (lambda (char? c) boolean?))))

 (maybe-escaped
   (lambda ((string? str) (procedure? pred) (char? quote-ch) (char? esc-ch)) formatter)
   (pure)
   ((pred (lambda (char? c) boolean?))))

 (maybe-escaped
   (lambda ((string? str) (procedure? pred) (char? quote-ch) (char? esc-ch) (procedure? renamer)) formatter)
   (pure)
   ((pred (lambda (char? c) boolean?))
    (renamer (lambda (char? c) char?))))

 (numeric
   (lambda ((number? num)) formatter)
   (pure))

 (numeric
   (lambda ((number? num) (integer? radix)) formatter)
   (pure))

 (numeric
   (lambda ((number? num) (integer? radix) (integer? precision)) formatter)
   (pure))

 (numeric
   (lambda ((number? num) (integer? radix) (integer? precision) ((or boolean? pair?) sign)) formatter)
   (pure))

 (numeric
   (lambda ((number? num) (integer? radix) (integer? precision) ((or boolean? pair?) sign) (integer? comma)) formatter)
   (pure))

 (numeric
   (lambda ((number? num) (integer? radix) (integer? precision) ((or boolean? pair?) sign) (integer? comma) (char? comma-sep)) formatter)
   (pure))

 (numeric
   (lambda ((number? num) (integer? radix) (integer? precision) ((or boolean? pair?) sign) (integer? comma) (char? comma-sep) (char? decimal-sep)) formatter)
   (pure))

 (numeric/comma
   (lambda ((number? num)) formatter)
   (pure))

 (numeric/comma
   (lambda ((number? num) (integer? radix)) formatter)
   (pure))

 (numeric/comma
   (lambda ((number? num) (integer? radix) (integer? precision)) formatter)
   (pure))

 (numeric/comma
   (lambda ((number? num) (integer? radix) (integer? precision) ((or boolean? pair?) sign)) formatter)
   (pure))

 (numeric/si
   (lambda ((number? num)) formatter)
   (pure))

 (numeric/si
   (lambda ((number? num) (integer? base)) formatter)
   (pure))

 (numeric/si
   (lambda ((number? num) (integer? base) (string? separator)) formatter)
   (pure))

 (numeric/fitted
   (lambda ((number? num)) formatter)
   (pure))

 (numeric/fitted
   (lambda ((number? num) (integer? radix)) formatter)
   (pure))

 (numeric/fitted
   (lambda ((number? num) (integer? radix) (integer? precision)) formatter)
   (pure))

 (numeric/fitted
   (lambda ((number? num) (integer? radix) (integer? precision) ((or boolean? pair?) sign)) formatter)
   (pure))

 (numeric/fitted
   (lambda ((number? num) (integer? radix) (integer? precision) ((or boolean? pair?) sign) (integer? comma)) formatter)
   (pure))

 (numeric/fitted
   (lambda ((number? num) (integer? radix) (integer? precision) ((or boolean? pair?) sign) (integer? comma) (char? comma-sep)) formatter)
   (pure))

 (numeric/fitted
   (lambda ((number? num) (integer? radix) (integer? precision) ((or boolean? pair?) sign) (integer? comma) (char? comma-sep) (char? decimal-sep)) formatter)
   (pure))

 (nl (value formatter))

 (fl (value formatter))

 (space-to
   (lambda ((integer? column)) formatter)
   (pure))

 (tab-to
   (lambda () formatter)
   (pure))

 (tab-to
   (lambda ((integer? tab-width)) formatter)
   (pure))

 (nothing (value formatter))

 (each
   (lambda ((formatter fmt) ...) formatter)
   (pure))

 (each-in-list
   (lambda ((list? list-of-fmts)) formatter)
   (pure))

 (joined
   (lambda ((procedure? mapper) (list? list)) formatter)
   (pure)
   ((mapper (lambda (element) formatter))))

 (joined
   (lambda ((procedure? mapper) (list? list) ((or string? formatter) separator)) formatter)
   (pure)
   ((mapper (lambda (element) formatter))))

 (joined/prefix
   (lambda ((procedure? mapper) (list? list)) formatter)
   (pure)
   ((mapper (lambda (element) formatter))))

 (joined/prefix
   (lambda ((procedure? mapper) (list? list) ((or string? formatter) separator)) formatter)
   (pure)
   ((mapper (lambda (element) formatter))))

 (joined/suffix
   (lambda ((procedure? mapper) (list? list)) formatter)
   (pure)
   ((mapper (lambda (element) formatter))))

 (joined/suffix
   (lambda ((procedure? mapper) (list? list) ((or string? formatter) separator)) formatter)
   (pure)
   ((mapper (lambda (element) formatter))))

 (joined/last
   (lambda ((procedure? mapper) (procedure? last-mapper) (list? list)) formatter)
   (pure)
   ((mapper (lambda (element) formatter))
    (last-mapper (lambda (element) formatter))))

 (joined/last
   (lambda ((procedure? mapper) (procedure? last-mapper) (list? list) ((or string? formatter) separator)) formatter)
   (pure)
   ((mapper (lambda (element) formatter))
    (last-mapper (lambda (element) formatter))))

 (joined/dot
   (lambda ((procedure? mapper) (procedure? dot-mapper) ((or list? dotted-list?) list)) formatter)
   (pure)
   ((mapper (lambda (element) formatter))
    (dot-mapper (lambda (tail) formatter))))

 (joined/dot
   (lambda ((procedure? mapper) (procedure? dot-mapper) ((or list? dotted-list?) list) ((or string? formatter) separator)) formatter)
   (pure)
   ((mapper (lambda (element) formatter))
    (dot-mapper (lambda (tail) formatter))))

 (joined/range
   (lambda ((procedure? mapper) (integer? start)) formatter)
   (pure)
   ((mapper (lambda ((integer? value)) formatter))))

 (joined/range
   (lambda ((procedure? mapper) (integer? start) ((or integer? #f) end)) formatter)
   (pure)
   ((mapper (lambda ((integer? value)) formatter))))

 (joined/range
   (lambda ((procedure? mapper) (integer? start) ((or integer? #f) end) ((or string? formatter) separator)) formatter)
   (pure)
   ((mapper (lambda ((integer? value)) formatter))))

 (padded
   (lambda ((integer? width) (formatter fmt) ...) formatter)
   (pure))

 (padded/right
   (lambda ((integer? width) (formatter fmt) ...) formatter)
   (pure))

 (padded/both
   (lambda ((integer? width) (formatter fmt) ...) formatter)
   (pure))

 (trimmed
   (lambda ((integer? width) (formatter fmt) ...) formatter)
   (pure))

 (trimmed/right
   (lambda ((integer? width) (formatter fmt) ...) formatter)
   (pure))

 (trimmed/both
   (lambda ((integer? width) (formatter fmt) ...) formatter)
   (pure))

 (trimmed/lazy
   (lambda ((integer? width) (formatter fmt) ...) formatter)
   (pure))

 (fitted
   (lambda ((integer? width) (formatter fmt) ...) formatter)
   (pure))

 (fitted/right
   (lambda ((integer? width) (formatter fmt) ...) formatter)
   (pure))

 (fitted/both
   (lambda ((integer? width) (formatter fmt) ...) formatter)
   (pure))

 (columnar
   (lambda (((or formatter string? symbol? number?) column) ...) formatter)
   (pure))

 (tabular
   (lambda (((or formatter string? symbol? number?) column) ...) formatter)
   (pure))

 (wrapped
   (lambda ((formatter fmt) ...) formatter)
   (pure))

 (wrapped/list
   (lambda ((list? list-of-strings) ...) formatter)
   (pure))

 (wrapped/char
   (lambda ((formatter fmt) ...) formatter)
   (pure))

 (justified
   (lambda ((formatter fmt) ...) formatter)
   (pure))

 (from-file
   (lambda ((string? pathname)) formatter))

 (line-numbers
   (lambda () formatter)
   (pure))

 (line-numbers
   (lambda ((integer? start)) formatter)
   (pure))

 (as-red
   (lambda ((formatter fmt) ...) formatter))

 (as-blue
   (lambda ((formatter fmt) ...) formatter))

 (as-green
   (lambda ((formatter fmt) ...) formatter))

 (as-cyan
   (lambda ((formatter fmt) ...) formatter))

 (as-yellow
   (lambda ((formatter fmt) ...) formatter))

 (as-magenta
   (lambda ((formatter fmt) ...) formatter))

 (as-white
   (lambda ((formatter fmt) ...) formatter))

 (as-black
   (lambda ((formatter fmt) ...) formatter))

 (as-bold
   (lambda ((formatter fmt) ...) formatter))

 (as-underline
   (lambda ((formatter fmt) ...) formatter))

 (as-unicode
   (lambda ((formatter fmt) ...) formatter))

 (unicode-terminal-width
   (lambda ((string? str)) integer?)
   (pure))

 (fn
   (syntax-rules ()
     ((_ (binding ...) expr ... fmt)))
   ()
   ((binding (id state-var)
             id)))

 (with
   (syntax-rules ()
     ((_ ((state-var value) ...) fmt ...))))

 (with!
   (syntax-rules ()
     ((_ (state-var value) ...))))

 (forked
   (lambda ((formatter fmt1) (formatter fmt2)) formatter)
   (pure))

 (call-with-output
   (lambda ((formatter fmt) (procedure? mapper)) formatter)
   (pure)
   ((mapper (lambda ((string? result-string)) formatter))))

 (port (value formatter-variable))
 (row (value formatter-variable))
 (col (value formatter-variable))
 (width (value formatter-variable))
 (output (value formatter-variable))
 (writer (value formatter-variable))
 (string-width (value formatter-variable))
 (pad-char (value formatter-variable))
 (ellipsis (value formatter-variable))
 (radix (value formatter-variable))
 (precision (value formatter-variable))
 (decimal-sep (value formatter-variable))
 (decimal-align (value formatter-variable))
 (word-separator? (value formatter-variable))

 )
