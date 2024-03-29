(((name . "fx-width")
  (signature value integer?)
  (desc . "Bound to the value w that specifies the implementation-defined range. (R6RS fixnum-width is a procedure that always returns this value.)"))
 ((name . "fx-greatest") 
  (signature value fixnum?)
  (desc . "Bound to the value 2^(w-1)-1, the largest representable fixnum. (R6RS greatest-fixnum is a procedure that always returns this value.)"))
 ((name . "fx-least")
  (signature value fixnum?)
  (desc . "Bound to the value -2^(w-1), the smallest representable fixnum. (R6RS least-fixnum is a procedure that always returns this value.)"))
 ((name . "fixnum?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is an exact integer within the fixnum range, and #f otherwise."))
 ((name . "fx=?") (signature lambda ((fixnum? i) ...) boolean?) (tags pure) (desc . "Semantically equivalent to =."))
 ((name . "fx<?") (signature lambda ((fixnum? i) ...) boolean?) (tags pure) (desc . "Semantically equivalent to <."))
 ((name . "fx>?") (signature lambda ((fixnum? i) ...) boolean?) (tags pure) (desc . "Semantically equivalent to >."))
 ((name . "fx<=?") (signature lambda ((fixnum? i) ...) boolean?) (tags pure) (desc . "Semantically equivalent to <=."))
 ((name . "fx>=?") (signature lambda ((fixnum? i) ...) boolean?) (tags pure) (desc . "Semantically equivalent to >=."))
 ((name . "fxzero?") (signature lambda ((fixnum? i)) boolean?) (tags pure) (desc . "Semantically equivalent to zero?. "))
 ((name . "fxpositive?") (signature lambda ((fixnum? i)) boolean?) (tags pure) (desc . "Semantically equivalent to positive?. "))
 ((name . "fxnegative?") (signature lambda ((fixnum? i)) boolean?) (tags pure) (desc . "Semantically equivalent to negative?. "))
 ((name . "fxodd?") (signature lambda ((fixnum? i)) boolean?) (tags pure) (desc . "Semantically equivalent to odd?."))
 ((name . "fxeven?") (signature lambda ((fixnum? i)) boolean?) (tags pure) (desc . "Semantically equivalent to even?."))
 ((name . "fxmax")
  (signature lambda ((fixnum? i) (fixnum? j) ...) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to max."))
 ((name . "fxmin")
  (signature lambda ((fixnum? i) (fixnum? j) ...) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to min."))
 ((name . "fx+")
  (signature lambda ((fixnum? i) (fixnum? j)) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to +, but accepts exactly two arguments."))
 ((name . "fx-")
  (signature lambda ((fixnum? i) (fixnum? j)) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to -, but accepts exactly two arguments."))
 ((name . "fxneg") 
  (signature lambda ((fixnum? i)) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to -, but accepts exactly one argument."))
 ((name . "fx*")
  (signature lambda ((fixnum? i) (fixnum? j)) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to *, but accepts exactly two arguments."))
 ((name . "fxquotient")
  (signature lambda ((fixnum? i) (fixnum? j)) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to quotient."))
 ((name . "fxremainder")
  (signature lambda ((fixnum? i) (fixnum? j)) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to remainder."))
 ((name . "fxabs")
  (signature lambda ((fixnum? i)) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to abs. In accordance with the fixnum rule, has undefined results when applied to fx-least."))
 ((name . "fxsquare") (signature lambda ((fixnum? i)) fixnum?) (tags pure) (desc . "Semantically equivalent to square."))
 ((name . "fxsqrt") (signature lambda ((fixnum? i)) fixnum?) (tags pure) (desc . "Semantically equivalent to exact-integer-sqrt (not sqrt)."))
 ((name . "fx+/carry")
  (signature
   lambda
   ((fixnum? i) (fixnum? j) (fixnum? k))
   (values fixnum? fixnum?))
  (tags pure)
  (desc . "Returns the two fixnum results of the following computation:
(let*-values (((s) (+ i j k))
       ((q r) (balanced/ s (expt 2 fx-width))))
  (values r q))"))
 ((name . "fx-/carry")
  (signature
   lambda
   ((fixnum? i) (fixnum? j) (fixnum? k))
   (values fixnum? fixnum?))
  (tags pure)
  (desc . "Returns the two fixnum results of the following computation:
(let*-values (((d) (- i j k))
       ((q r) (balanced/ d (expt 2 fx-width))))
  (values r q))"))
 ((name . "fx*/carry")
  (signature
   lambda
   ((fixnum? i) (fixnum? j) (fixnum? k))
   (values fixnum? fixnum?))
  (tags pure)
  (desc . "Returns the two fixnum results of the following computation:
(let*-values (((s) (+ (* i j) k))
       ((q r) (balanced/ s (expt 2 fx-width))))
  (values r q))"))
 ((name . "fxnot") (signature lambda ((integer? i)) integer?) (tags pure) (desc . "Semantically equivalent to bitwise-not."))
 ((name . "fxand") (signature lambda ((integer? i) ...) integer?) (tags pure) (desc . "Semantically equivalent to bitwise-and."))
 ((name . "fxior") (signature lambda ((integer? i) ...) integer?) (tags pure) (desc . "Semantically equivalent to bitwise-ior."))
 ((name . "fxxor") (signature lambda ((integer? i) ...) integer?) (tags pure) (desc . "Semantically equivalent to bitwise-xor."))
 ((name . "fxarithmetic-shift")
  (signature lambda ((fixnum? i) (integer? count)) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to arithmetic-shift, except that it is an error for the absolute value of count to exceed w-1."))
 ((name . "fxarithmetic-shift-left")
  (signature lambda ((fixnum? i) (integer? count)) fixnum?)
  (tags pure)
  (desc . "The same as fxarithmetic-shift except that a negative value of count is an error. This is provided for additional efficiency."))
 ((name . "fxarithmetic-shift-right")
  (signature lambda ((fixnum? i) (integer? count)) fixnum?)
  (tags pure)
  (desc . "The same as fxarithmetic-shift except that a non-negative value of count specifies the number of bits to shift right, and a negative value is an error. This is provided for additional efficiency."))
 ((name . "fxbit-count") (signature lambda ((fixnum? i)) integer?) (tags pure) (desc . "Semantically equivalent to SRFI 151 bit-count."))
 ((name . "fxlength") (signature lambda ((fixnum? i)) integer?) (tags pure) (desc . "Semantically equivalent to integer-length."))
 ((name . "fxif")
  (signature lambda ((fixnum? mask) (fixnum? i) (fixnum? j)) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to bitwise-if. It can be implemented as (fxior (fxand mask i) (fxand (fxnot mask) j)))."))
 ((name . "fxbit-set?")
  (signature lambda ((integer? index) (fixnum? i)) boolean?)
  (tags pure)
  (desc . "Semantically equivalent to SRFI 151 bit-set?, except that it is an error for index to be larger than or equal to fx-width."))
 ((name . "fxcopy-bit")
  (signature lambda ((integer? index) (fixnum? i) (boolean? boolean)) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to SRFI 151 copy-bit, except that it is an error for index to be larger than or equal to fx-width."))
 ((name . "fxfirst-set-bit")
  (signature lambda ((fixnum? i)) integer?)
  (tags pure)
  (desc . "Semantically equivalent to first-set-bit."))
 ((name . "fxbit-field")
  (signature lambda ((fixnum? i) (integer? start) (integer? end)) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to bit-field."))
 ((name . "fxbit-field-rotate")
  (signature
   lambda
   ((fixnum? i) (integer? count) (integer? start) (integer? end))
   fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to SRFI 151 bit-field-rotate."))
 ((name . "bit-field-reverse")
  (signature lambda ((fixnum? i) (integer? start) (integer? end)) fixnum?)
  (tags pure)
  (desc . "Semantically equivalent to bit-field-reverse.")))
