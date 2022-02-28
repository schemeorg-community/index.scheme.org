(
 
 (*
   (lambda ((number? z) ...) number?)
   (pure))

 (+
   (lambda ((number? z) ...) number?)
   (pure))

 (-
   (lambda ((number? z) ...) number?)
   (pure))


 (/
   (lambda ((number? z1) (number? z2) ...) number?)
   (pure))

 (<
   (lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
   (pure))

 (<=
   (lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
   (pure))

 (=
   (lambda ((number? z1) (number? z2) (number? z3) ...) boolean?)
   (pure))

 (=>
   (lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
   (pure))

 (>
   (lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
   (pure))

 (>=
   (lambda ((real? x1) (real? x2) (real? x3) ...) boolean?)
   (pure))

 (abs
   (lambda ((real? x)) number?)
   (pure))
 
 (and
   (syntax-rules ()
     ((_ test1 ...))))

 (append
   (lambda ((list? list) ...)  list?)
   (pure))

 (append
   (lambda ((list? list) ... obj)  *)
   (pure))

 (apply
   (lambda ((procedure? proc) arg1 ... (list? args)) *)
   (pure))

 (assoc
   (lambda (obj (list? alist)) (or pair? boolean?))
   (pure))

 (assoc
   (lambda (obj (list? alist) (procedure? =)) (or pair? boolean?))
   (pure)
   ((= (lambda (a b) *))))

 (assq
   (lambda (obj (list? alist)) (or pair? boolean?))
   (pure))

 (assv
   (lambda (obj (list? alist)) (or pair? boolean?))
   (pure))
 
 (begin
   (syntax-rules ()
     ((_ expression-or-definition ...))))

 (binary-port?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (port?))

 (boolean=?
   (lambda ((boolean? boolean1) (boolean? boolean2) (boolean? boolean3) ...) boolean?)
   (pure))

 (boolean?
   (lambda (obj) boolean?)
   (pure predicate))

 (bytevector
   (lambda ((integer? byte) ...) bytevector?)
   (pure))

 (bytevector-append
   (lambda ((bytevector? bytevector) ...) bytevector?)
   (pure))

 (bytevector-copy
   (lambda ((bytevector? bytevector)) bytevector?)
   (pure))

 (bytevector-copy
   (lambda ((bytevector? bytevector) (integer? start)) bytevector?)
   (pure))

 (bytevector-copy
   (lambda ((bytevector? bytevector) (integer? start) (integer? end)) bytevector?)
   (pure))

 (bytevector-copy!
   (lambda ((bytevector? to) (integer? at) (bytevector? from)) undefined)
   ())

 (bytevector-copy!
   (lambda ((bytevector? to) (integer? at) (bytevector? from) (integer? start)) undefined)
   ())

 (bytevector-copy!
   (lambda ((bytevector? to) (integer? at) (bytevector? from) (integer? start) (integer? end)) undefined)
   ())

 (bytevector-length
   (lambda ((bytevector? bytevector)) integer?)
   (pure))

 (bytevector-u8-ref
   (lambda ((bytevector? bytevector) (integer? k)) integer?)
   (pure)) 

 (bytevector-u8-set!
   (lambda ((bytevector? bytevector) (integer? k) (integer? byte)) undefined))

 (bytevector?
   (lambda (obj) boolean?)
   (pure predicate))

 (caar
   (lambda ((pair? pair)) *)
   (pure))

 (
  cadr
  (lambda ((pair? pair)) *)
  (pure))

 (call-with-current-continuation
   (lambda ((procedure? proc)) *)
   ()
   ((proc (lambda ((procedure? k)) *))))

 (call-with-port
  (lambda ((port? port) (procedure? proc)) *)
  ()
  ((proc (lambda ((port? port)) *))))

 (call-with-values
   (lambda ((procedure? producer) (procedure? consumer)) *)
   (pure)
   ((producer (lambda () *))
    (consumer (lambda (obj ...) *)))) 

 (
  call/cc
  (lambda ((procedure? proc)) *)
  ()
  ((proc (lambda ((procedure? k)) *))))

 (car
   (lambda ((pair? pair)) *)
   (pure))
 
 (case
   (syntax-rules (=> else)
     ((_ key clause1 clause2 ...)))
   ()
   ((clause ((datum1 ...) expression1 expression2 ...)
            ((datum1 ...) => expression)
            (else expression1 expression2 ...))))

 (cdar
   (lambda ((pair? pair)) *)
   (pure))

 (cddr
   (lambda ((pair? pair)) *)
   (pure))

 (cdr
   (lambda ((pair? pair)) *)
   (pure))

 (ceiling
   (lambda ((real? x)) real?)
   (pure))

 (char->integer
   (lambda ((char? char)) integer?)
   (pure))

 (char-ready?
   (lambda () boolean?)
   (parameterized))

 (char-ready?
   (lambda ((input-port? port)) boolean?)
   ())

 (char<=?
   (lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
   (pure))

 (char<?
   (lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
   (pure))

 (char=?
   (lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
   (pure))

 (char>=?
   (lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
   (pure))

 (char>?
   (lambda ((char? char1) (char? char2) (char? char3) ...) boolean?)
   (pure))

 (char?
   (lambda (obj) boolean?)
   (pure predicate))

 (close-input-port
   (lambda ((input-port? port)) undefined))

 (close-output-port
   (lambda ((output-port? port)) undefined))

 (close-port
   (lambda ((port? port)) undefined))

 (complex?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (number?))
 
 (cond
   (syntax-rules (else =>)
     ((_ clause1 clause2 ...)))
   ()
   ((clause (test expression1 ...)
            (test => expression)
            (else expression1 expression2 ...))))
 
 (cond-expand
   (syntax-rules (library and or not else)
     ((_ ce-clause1 ce-clause2 ...)))
   ()
   ((ce-clause (feature-requirement expression ...)
               (else expression))
    (feature-requirement feature-identifier
                         (library library-name)
                         (and feature-requirement ...)
                         (or feature-requirement ...)
                         (not feature-requirement))))

 (cons
   (lambda (obj1 obj2) pair?)
   (pure))

 (current-error-port
   (lambda () output-port?)
   (parameter))

 (current-input-port
   (lambda () input-port?)
   (parameter))

 (current-output-port
  (lambda () output-port?)
  (parameter))
 
 (define
   (syntax-rules ()
     ((_ variable expression))
     ((_ (variable parameter1 ...) body))
     ((_ (variable parameter1 ... . parameter) body))))
 
 (define-record-type
   (syntax-rules ()
     ((name constructor pred field ...)))
   ()
   ((constructor (constructor-name field-name ...))
    (field (field-name accessor-name)
           (field-name accessor-name modifier-name))))
 
 (define-syntax
   (syntax-rules ()
     ((_ keyword transformer-spec))))
 
 (define-values
   (syntax-rules ()
     ((_ formals expression)))
   ()
   ((formals (variable1 ...)
             variable
             (variable1 ... variable_n . variable_n+1))))

 (denominator
   (lambda ((rational? q)) integer?)
   (pure))
 
 (do
    (syntax-rules ()
      ((_ (variable-decl1 ...)
          (test expression ...)
          command ...)))
    ()
    ((variable-decl (variable init step)
                    (variable init))))

 (dynamic-wind
   (lambda ((procedure? before) (procedure? thunk) (procedure? after)) *)
   ()
   ((before (lambda () undefined))
    (thunk (lambda () *))
    (after (lambda () undefined))))


 (eof-object
   (lambda () eof-object?)
   (pure))

 (eof-object?
   (lambda (obj) boolean?)
   (pure predicate))

 (eq?
   (lambda (obj1 obj2) boolean?)
   (pure))

 (equal?
   (lambda (obj1 obj2) boolean?)
   (pure))

 (eqv?
   (lambda (obj1 obj2) boolean?)
   (pure))

 (error
   (lambda ((string? message) obj ...) undefined)
   ())

 (error-object-irritants
   (lambda ((error-object? error-object)) list?)
   (pure))

 (error-object-message
   (lambda ((error-object? error-object)) string?)
   (pure))

 (error-object?
   (lambda (obj) boolean?)
   (pure predicate))

 (even?
   (lambda ((integer? n)) boolean?)
   (pure predicate))

 (exact
   (lambda ((number? z)) exact?)
   (pure))

 (exact-integer-sqrt
   (lambda ((integer? k)) (values integer? integer?))
   (pure))

 (exact-integer?
   (lambda ((number? z)) boolean?)
   (pure)
   ()
   (exact? integer?)
   )

 (exact?
   (lambda ((number? z)) boolean?)
   (pure)
   ()
   (number?))

 (expt
   (lambda ((number? z1) (number? z2)) number?)
   (pure))

 (features
   (lambda () list?)
   (pure))

 (file-error?
   (lambda (obj) boolean?)
   (pure predicate))

 (floor
   (lambda ((real? x)) integer?)
   (pure))

 (floor-quotient
   (lambda ((integer? n1) (integer? n2)) integer?)
   (pure))

 (floor-remainder
   (lambda ((integer? n1) (integer? n2)) integer?)
   (pure))

 (floor/
   (lambda ((integer? n1) (integer? n2)) (values integer? integer?))
   (pure))

 (flush-output-port
   (lambda () undefined)
   (parameterized))

 (flush-output-port
   (lambda ((output-port? port)) undefined)
   ())

 (for-each
   (lambda ((procedure? proc) (list? list1) (list? list2) ...) undefined)
   ()
   ((proc (lambda (obj1 obj2 ...) undefined))))

 (gcd
   (lambda ((integer? n) ...) integer?)
   (pure))

 (get-output-bytevector
   (lambda ((output-port? port)) bytevector?)
   ())

 (get-output-string
   (lambda ((output-port? port)) string?)
   ())
 
 (guard
   (syntax-rules (=> else)
     ((_ (variable cond-clause1 cond-clause2 ...) body)))
   ()
   ((cond-clause (test expression1 ...)
                 (test => expression)
                 (else expression1 expression2 ...))))
 
 (if
    (syntax-rules ()
      ((_ test consequent))
      ((_ test consequent alternate))))
 
 (include
   (syntax-rules ()
     ((_ string1 string2 ...))))
 
 (include-ci
   (syntax-rules ()
     ((_ string1 string2 ...))))

 (inexact
   (lambda ((number? z)) inexact?)
   (pure))

 (inexact?
   (lambda ((number? z)) boolean?)
   (pure)
   ()
   (number?))

 (input-port-open?
   (lambda ((input-port? port)) boolean?))

 (input-port?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (port?))

 (integer->char
   (lambda ((integer? n)) char?)
   (pure))

 (integer?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (rational?))

 (lambda
   (syntax-rules ()
     ((_ formals body)))
   ()
   ((formals (variable1 ...)
             variable
             (variable1 ... variable_n . variable_n+1))))

 (lcm
   (lambda ((integer? n) ...) integer?)
   (pure))

 (length
   (lambda ((list? list)) integer?)
   (pure))
 
 (let
  (syntax-rules ()
    ((_ bindings body)))
  ()
  ((bindings ((variable1 init1) ...))))
 
 (let*
  (syntax-rules ()
    ((_ bindings body)))
  ()
  ((bindings ((variable1 init1) ...))))

 (let*-values
   (syntax-rules ()
     ((_ mv-binding-spec body)))
   ()
   ((mv-binding-spec ((formals1 init1) ...))
    (formals (variable1 ...)
             variable
             (variable1 ... variable_n . variable_n+1))))
 
 (let-syntax
   (syntax-rules ()
     ((_ bindings body)))
   ()
   ((bindings ((keyword transformer-spec) ...))))
 
 (let-values
   (syntax-rules ()
     ((_ mv-binding-spec body)))
   ()
   ((mv-binding-spec ((formals1 init1) ...))
    (formals (variable1 ...)
             variable
             (variable1 ... variable_n . variable_n+1))))
 
 (letrec
  (syntax-rules ()
    ((_ bindings body)))
  ()
  ((bindings ((variable1 init1) ...))))
 
 (letrec*
  (syntax-rules ()
    ((_ bindings body)))
  ()
  ((bindings ((variable1 init1) ...))))

 (letrec-syntax
   (syntax-rules ()
     ((_ bindings body)))
   ()
   ((bindings ((keyword transformer-spec) ...))))

 (list
   (lambda (obj ...) list?)
   (pure))

 (list->string
   (lambda ((list? list)) string?)
   (pure))

 (list->vector
   (lambda ((list? list)) vector?)
   (pure))

 (list-copy
   (lambda (obj) *)
   (pure))

 (list-ref
   (lambda ((list? list) (integer? k)) *)
   (pure))

 (list-set!
   (lambda ((list? list) (integer? k) obj) undefined)
   ())

 (list-tail
   (lambda ((list? list) (integer? k)) list?)
   (pure))

 (list?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (pair? null?))

 (make-bytevector
   (lambda ((integer? k)) bytevector?)
   ())

 (make-bytevector
   (lambda ((integer? k) (integer? byte)) bytevector?)
   (pure))

 (make-list
   (lambda ((integer? k)) list?)
   ()) 

 (make-list
   (lambda ((integer? k) obj) list?)
   (pure))

 (make-parameter
   (lambda (obj) procedure?)
   (pure))

 (make-parameter
   (lambda (obj (procedure? converter)) procedure?)
   (pure)
   ((converter (lambda (obj) *))))

 (make-string
   (lambda ((integer? k)) string?)
   ())

 (make-string
   (lambda ((integer? k) (char? char)) string?)
   (pure))

 (make-vector
   (lambda ((integer? k)) vector?)
   ())

 (make-vector
   (lambda ((integer? k) fill) vector?)
   (pure))

 (map
   (lambda ((procedure? proc) (list? list1) (list? list2) ...) list?)
   (pure)
   ((proc (lambda (obj1 obj2 ...) *))))

 (max
   (lambda ((real? x1) (real? x2) ...) real?)
   (pure))

 (member
   (lambda (obj (list? list)) (or boolean? list?))
   (pure))

 (member
   (lambda (obj (list? list) (procedure? compare)) (or boolean? list?))
   (pure)
   ((compare (lambda (obj1 obj2) *))))

 (memq
   (lambda (obj (list? list)) (or boolean? list?))
   (pure))

 (memv
   (lambda (obj (list? list)) (or boolean? list?))
   (pure))

 (min
   (lambda ((real? x1) (real? x2) ...) real?)
   (pure))

 (modulo
   (lambda ((integer? n1) (integer? n2)) integer?)
   (pure))

 (negative?
   (lambda ((real? x)) boolean?)
   (pure))

 (newline
   (lambda () undefined)
   (parameterized))

 (newline
   (lambda ((output-port? port)) undefined)
   ())
 
 (not
   (lambda (obj) boolean?)
   (pure))

 (null?
   (lambda (obj) boolean?)
   (pure predicate))

 (number->string
   (lambda ((number? z)) string?)
   (pure))

 (number->string
   (lambda ((number? z) (integer? radix)) string?)
   (pure))

 (number?
   (lambda (obj) boolean?)
   (pure predicate))

 (numerator
   (lambda ((rational? q)) integer?)
   (pure))

 (odd?
   (lambda ((integer? n)) boolean?)
   (pure predicate))

 (open-input-bytevector
   (lambda ((bytevector? bytevector)) input-port?)
   (pure))

 (open-input-string
   (lambda ((string? string)) input-port?)
   (pure))

 (open-output-bytevector
   (lambda () output-port?)
   (pure))

 (open-output-string
   (lambda () output-port?)
   (pure))
 
 (or
   (syntax-rules ()
     ((_ test1 ...))))

 (output-port-open?
   (lambda ((output-port? port)) boolean?)
   ())

 (output-port?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (port?))

 (pair?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (parameterize
   (syntax-rules ()
     ((_ ((param1 value1) ...) body))))

 (peek-char
   (lambda () (or eof-object? char?))
   (parameterized))

 (peek-char
   (lambda ((input-port? port)) (or eof-object? char?))
   ())

 (peek-u8
   (lambda () (or eof-object? integer?))
   (parameterized))

 (peek-u8
   (lambda ((input-port? port)) (or eof-object? integer?))
   ())

 (port?
   (lambda (obj) boolean?)
   (pure predicate))

 (positive?
   (lambda ((real? x)) boolean?)
   (pure))

 (procedure?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (quasiquote
   (syntax-rules ()
     ((_ qq-template))))
 
 (quote
   (syntax-rules ()
     ((_ datum))))

 (quotient
   (lambda ((integer? n1) (integer? n2)) integer?)
   (pure))

 (raise
   (lambda (obj) undefined)
   ())

 (raise-continuable
   (lambda (obj) undefined)
   ())

 (rational?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (real?))

 (rationalize
   (lambda ((real? x) (real? y)) rational?)
   (pure))

 (read-bytevector
   (lambda ((integer? k)) bytevector?)
   (parameterized))

 (read-bytevector
   (lambda ((integer? k) (input-port? port)) bytevector?)
   ())

 (read-bytevector!
   (lambda ((bytevector? bytevector)) (or eof-object? integer?))
   (parameterized))

 (read-bytevector!
   (lambda ((bytevector? bytevector) (input-port? port)) (or eof-object? integer?))
   ())

 (read-bytevector!
   (lambda ((bytevector? bytevector) (input-port? port) (integer? start)) (or eof-object? integer?))
   ())

 (read-bytevector!
   (lambda ((bytevector? bytevector) (input-port? port) (integer? start) (integer? end)) (or eof-object? integer?))
   ())

 (read-char
   (lambda () (or eof-object? char?))
   (parameterized))

 (read-char
   (lambda ((input-port? port)) (or eof-object? char?))
   ())

 (read-error?
   (lambda (obj) boolean?)
   (pure predicate))

 (read-line
   (lambda () (or eof-object? string?))
   (parameterized))

 (read-line
   (lambda ((input-port? port)) (or eof-object? string?))
   ())

 (read-string
   (lambda ((integer? k)) (or eof-object? string?))
   (parameterized))

 (read-string
   (lambda ((integer? k) (input-port? port)) (or eof-object? string?))
   ())


 (read-u8
   (lambda () (or eof-object? integer?))
   (parameterized))

 (read-u8
   (lambda ((input-port? port)) (or eof-object? integer?))
   ())

 (real?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (complex?))

 (remainder
   (lambda ((integer? n1) (integer? n2)) integer?)
   (pure))

 (reverse
   (lambda ((list? list)) list?)
   (pure))

 (round
   (lambda ((real? x)) integer?)
   (pure))
 
 (set!
   (syntax-rules ()
     ((_ variable expression))))
 
 (set-car!
   (lambda ((pair?  pair) obj) undefined))
 
 (set-cdr!
   (lambda ((pair?  pair) obj) undefined))

 (square
   (lambda ((number? z)) number?)
   (pure))

 (string
   (lambda ((char? char) ...) string?)
   (pure))

 (string->list
   (lambda ((string? string)) list?)
   (pure))

 (string->list
   (lambda ((string? string) (integer? start)) list?)
   (pure))

 (string->list
   (lambda ((string? string) (integer? start) (integer? end)) list?)
   (pure))

 (string->number
   (lambda ((string? string)) number?)
   (pure))

 (string->number
   (lambda ((string? string) (integer? radix)) number?)
   (pure))

 (string->symbol
   (lambda ((string? string)) symbol?)
   (pure))

 (string->utf8
   (lambda ((string? string)) bytevector?)
   (pure))

 (string->utf8
   (lambda ((string? string) (integer? start)) bytevector?)
   (pure))

 (string->utf8
   (lambda ((string? string) (integer? start) (integer? end)) bytevector?)
   (pure))

 (string->vector
   (lambda ((string? string)) vector?)
   (pure))

 (string->vector
   (lambda ((string? string) (integer? start)) vector?)
   (pure))

 (string->vector
   (lambda ((string? string) (integer? start) (integer? end)) vector?)
   (pure))

 (string-append
   (lambda ((string? string) ...) string?)
   (pure))

 (string-copy
   (lambda ((string? string)) string?)
   (pure))

 (string-copy
   (lambda ((string? string) (integer? start)) string?)
   (pure))

 (string-copy
   (lambda ((string? string) (integer? start) (integer? end)) string?)
   (pure))

 (string-copy!
   (lambda ((string? to) (integer? at) (string? from)) undefined)
   ())

 (string-copy!
   (lambda ((string? to) (integer? at) (string? from) (integer? start)) undefined)
   ())

 (string-copy!
   (lambda ((string? to) (integer? at) (string? from) (integer? start) (integer? end)) undefined)
   ())

 (string-fill!
   (lambda ((string? string) (char? fill)) undefined)
   ())

 (string-fill!
   (lambda ((string? string) (char? fill) (integer? start)) undefined)
   ())

 (string-fill!
   (lambda ((string? string) (char? fill) (integer? start) (integer? end)) undefined)
   ())

 (string-for-each
   (lambda ((procedure? proc) (string? string1) (string? string2) ...) undefined)
   ()
   ((proc (lambda ((string? string) ...) undefined))))

 (string-length
   (lambda ((string? string)) integer?)
   (pure))

 (string-map
   (lambda ((procedure? proc) (string? string1) (string? string2) ...) string?)
   (pure)
   ((proc (lambda ((string? string) ...) char?))))

 (string-ref
   (lambda ((string? string) (integer? k)) char?)
   (pure))

 (string-set!
   (lambda ((string? string) (integer? k) (char? char)) undefined)
   ())

 (string<=?
   (lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
   (pure))

 (string<?
   (lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
   (pure))

 (string=?
   (lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
   (pure))

 (string>=?
   (lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
   (pure))

 (string>?
   (lambda ((string? string1) (string? string2) (string? string3) ...) boolean?)
   (pure))

 (string?
   (lambda (obj) boolean?)
   (pure predicate))

 (substring
   (lambda ((string? string) (integer? start) (integer? end)) string?)
   (pure))

 (symbol->string
   (lambda ((symbol? symbol)) string?)
   (pure))

 (symbol=?
   (lambda ((symbol? symbol1) (symbol? symbol2) (symbol? symbol3) ...) boolean?)
   (pure))

 (symbol?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (syntax-error
   (syntax-rules ()
     ((_ message args ...))))
 
 (syntax-rules
   (syntax-rules ()
     ((_ (literal ...) syntax-rule ...))
     ((_ ellipsis (literal ...) syntax-rule ...)))
   ()
   ((syntax-rule (pattern template))
    (pattern identifier
             constant
             (pattern ...)
             (pattern pattern ... . pattern)
             (pattern ... pattern ellipsis pattern ...)
             (pattern ... pattern ellipsis pattern ... . pattern)
             (_append |#| (pattern ...))
             (_append |#| (pattern ... pattern ellipsis pattern ...)))
    (template identifier
              constant
              (element ...)
              (element element ... . template)
              (ellipsis template)
              (_append |#| (element ...)))
    (element template
             (_append template ellipsis))))

 (textual-port?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (port?))

 (truncate
   (lambda ((real? x)) integer?)
   (pure))

 (truncate-quotient
   (lambda ((integer? n1) (integer? n2)) integer?)
   (pure))

 (truncate-remainder
   (lambda ((integer? n1) (integer? n2)) integer?)
   (pure))

 (truncate/
   (lambda ((integer? n1) (integer? n2)) (values integer? integer?))
   (pure))


 (u8-ready?
   (lambda () boolean?)
   (parameterized))
 
 (unless
   (syntax-rules ()
     ((_ test expression1 expression2 ...))))
 
 (unquote
   (syntax-rules ()
     ((_ expression))))
 
 (unquote-splicing
   (syntax-rules ()
     ((_ expression))))

 (u8-ready?
   (lambda ((input-port? port)) boolean?)
   ())

 (utf8->string
   (lambda ((bytevector? bytevector)) string?)
   (pure))

 (utf8->string
   (lambda ((bytevector? bytevector) (integer? start)) string?)
   (pure))

 (utf8->string
   (lambda ((bytevector? bytevector) (integer? start) (integer? end)) string?)
   (pure))

 (values
   (lambda (obj ...) (values * ...))
   (pure))

 (vector
   (lambda (obj ...) vector?)
   (pure))

 (vector->list
   (lambda ((vector? vector)) list?)
   (pure))

 (vector->list
   (lambda ((vector? vector) (integer? start)) list?)
   (pure))

 (vector->list
   (lambda ((vector? vector) (integer? start) (integer? end)) list?)
   (pure))

 (vector->string
   (lambda ((vector? vector)) string?)
   (pure))

 (vector->string
   (lambda ((vector? vector) (integer? start)) string?)
   (pure))

 (vector->string
   (lambda ((vector? vector) (integer? start) (integer? end)) string?)
   (pure))

 (vector-append
   (lambda ((vector? vector) ...) vector?)
   (pure))

 (vector-copy
   (lambda ((vector? vector)) vector?)
   (pure))

 (vector-copy
   (lambda ((vector? vector) (integer? start)) vector?)
   (pure))

 (vector-copy
   (lambda ((vector? vector) (integer? start) (integer? end)) vector?)
   (pure))

 (vector-copy!
   (lambda ((vector? to) (integer? at) (vector? from)) undefined)
   ())

 (vector-copy!
   (lambda ((vector? to) (integer? at) (vector? from) (integer? start)) undefined)
   ())

 (vector-copy!
   (lambda ((vector? to) (integer? at) (vector? from) (integer? start) (integer? end)) undefined)
   ())

 (vector-fill!
   (lambda ((vector? vector) fill) undefined) 
   ())

 (vector-fill!
   (lambda ((vector? vector) fill (integer? start)) undefined) 
   ())

 (vector-fill!
   (lambda ((vector? vector) fill (integer? start) (integer? end)) undefined) 
   ())

 (vector-for-each
   (lambda ((procedure? proc) (vector? vector1) (vector? vector2) ...) undefined)
   ()
   ((proc (lambda (obj ...) undefined))))

 (vector-length
   (lambda ((vector? vector)) integer?)
   (pure))

 (vector-map
   (lambda ((procedure? proc) (vector? vector1) (vector? vector2) ...) vector?)
   (pure)
   ((proc (lambda (obj ...) *))))

 (vector-ref
   (lambda ((vector? vector) (integer? k)) *)
   (pure))

 (vector-set!
   (lambda ((vector? vector) (integer? k) obj) undefined)
   ())

 (vector?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (when
   (syntax-rules ()
     ((_ test expression1 expression2 ...))))

 (with-exception-handler
   (lambda ((procedure? handler) (procedure? thunk)) *)
   ()
   ((handler (lambda (obj) *))
    (thunk (lambda () *))))

 (write-bytevector
   (lambda ((bytevector? bytevector)) undefined)
   (parameterized))

 (write-bytevector
   (lambda ((bytevector? bytevector) (output-port? port)) undefined)
   ())

 (write-bytevector
   (lambda ((bytevector? bytevector) (output-port? port) (integer? start)) undefined)
   ())

 (write-bytevector
   (lambda ((bytevector? bytevector) (output-port? port) (integer? start) (integer? end)) undefined)
   ())

 (write-char
   (lambda ((char? char)) undefined)
   (parameterized))

 (write-char
   (lambda ((char? char) (output-port? port)) undefined)
   ())

 (write-string
   (lambda ((string? string)) undefined)
   (parameterized))

 (write-string
   (lambda ((string? string) (output-port? port)) undefined)
   ())

 (write-string
   (lambda ((string? string) (output-port? port) (integer? start)) undefined)
   ())

 (write-string
   (lambda ((string? string) (output-port? port) (integer? start) (integer? end)) undefined)
   ())

 (write-u8
   (lambda ((integer? byte)) undefined)
   (parameterized))

 (write-u8
   (lambda ((integer? byte) (output-port port)) undefined)
   ())

 (zero?
   (lambda ((number? z)) boolean?)
   (pure predicate))

 )
