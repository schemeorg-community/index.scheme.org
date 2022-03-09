(
 
 (fx-width
   (value integer?))
 
 (fx-greatest
   (value fixnum?))
 
 (fx-least
   (value fixnum?))
 
 (fixnum?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (integer?))
 
 (fx=?
   (lambda ((fixnum? i) ...) boolean?)
   (pure))
 
 (fx<?
   (lambda ((fixnum? i) ...) boolean?)
   (pure))
 
 (fx>?
   (lambda ((fixnum? i) ...) boolean?)
   (pure))
 
 (fx<=?
   (lambda ((fixnum? i) ...) boolean?)
   (pure))
 
 (fx>=?
   (lambda ((fixnum? i) ...) boolean?)
   (pure))
 
 (fxzero?
   (lambda ((fixnum? i)) boolean?)
   (pure))
 
 (fxpositive?
   (lambda ((fixnum? i)) boolean?)
   (pure))
 
 (fxnegative?
   (lambda ((fixnum? i)) boolean?)
   (pure))
 
 (fxodd?
   (lambda ((fixnum? i)) boolean?)
   (pure))
 
 (fxeven?
   (lambda ((fixnum? i)) boolean?)
   (pure))
 
 (fxmax
   (lambda ((fixnum? i) (fixnum? j) ...) fixnum?)
   (pure))
 
 (fxmin
   (lambda ((fixnum? i) (fixnum? j) ...) fixnum?)
   (pure))
 
 (fx+
   (lambda ((fixnum? i) (fixnum? j)) fixnum?)
   (pure))
 
 (fx-
   (lambda ((fixnum? i) (fixnum? j)) fixnum?)
   (pure))
 
 (fxneg
   (lambda ((fixnum? i)) fixnum?)
   (pure))
 
 (fx*
   (lambda ((fixnum? i) (fixnum? j)) fixnum?)
   (pure))
 
 (fxquotient
   (lambda ((fixnum? i) (fixnum? j)) fixnum?)
   (pure))
 
 (fxremainder
   (lambda ((fixnum? i) (fixnum? j)) fixnum?)
   (pure))
 
 (fxabs
   (lambda ((fixnum? i)) fixnum?)
   (pure))
 
 (fxsquare
   (lambda ((fixnum? i)) fixnum?)
   (pure))
 
 (fxsqrt
   (lambda ((fixnum? i)) fixnum?)
   (pure))
 
 (fx+/carry
   (lambda ((fixnum? i) (fixnum? j) (fixnum? k)) (values fixnum? fixnum?))
   (pure))
 
 (fx-/carry
   (lambda ((fixnum? i) (fixnum? j) (fixnum? k)) (values fixnum? fixnum?))
   (pure))
 
 (fx*/carry
   (lambda ((fixnum? i) (fixnum? j) (fixnum? k)) (values fixnum? fixnum?))
   (pure))
 
 (fxnot
   (lambda ((integer? i)) integer?)
   (pure))
 
 (fxand
   (lambda ((integer? i) ...) integer?)
   (pure))
 
 (fxior
   (lambda ((integer? i) ...) integer?)
   (pure))
 
 (fxxor
   (lambda ((integer? i) ...) integer?)
   (pure))
 
 (fxarithmetic-shift
   (lambda ((fixnum? i) (integer? count)) fixnum?)
   (pure))
 
 (arithmetic-shift-left
   (lambda ((fixnum? i) (integer? count)) fixnum?)
   (pure))
 
 (arithmetic-shift-right
   (lambda ((fixnum? i) (integer? count)) fixnum?)
   (pure))
 
 (fxbit-count
   (lambda ((fixnum? i)) integer?)
   (pure))
 
 (fxlength
   (lambda ((fixnum? i)) integer?)
   (pure))
 
 (fxif
   (lambda ((fixnum? mask) (fixnum? i) (fixnum? j)) fixnum?)           
   (pure))
 
 (fxbit-set?
   (lambda ((integer? index) (fixnum? i)) boolean?)
   (pure))
 
 (fxcopy-bit
   (lambda ((integer? index) (fixnum? i) (boolean? boolean)) fixnum?)
   (pure))
 
 (fxfirst-set-bit
   (lambda ((fixnum? i)) integer?)
   (pure))
 
 (fxbit-field
   (lambda ((fixnum? i) (integer? start) (integer? end)) fixnum?)
   (pure))
 
 (fxbit-field-rotate
   (lambda ((fixnum? i) (integer? count) (integer? start) (integer? end)) fixnum?)
   (pure))
 
 (bit-field-reverse
   (lambda ((fixnum? i) (integer? start) (integer? end)) fixnum?)
   (pure))
 )
