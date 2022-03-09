(
 
 (bitwise-not
   (lambda ((integer? i)) integer?)
   (pure))
 
 (bitwise-and
   (lambda ((integer? i) ...) integer?)
   (pure))
 
 (bitwise-ior
   (lambda ((integer? i) ...) integer?)
   (pure))
 
 (bitwise-xor
   (lambda ((integer? i) ...) integer?)
   (pure))
 
 (bitwise-eqv
   (lambda ((integer? i) ...) integer?)
   (pure))
 
 (bitwise-nand
   (lambda ((integer? i) (integer? j)) integer?)
   (pure))
 
 (bitwise-nor
   (lambda ((integer? i) (integer? j)) integer?)
   (pure))
 
 (bitwise-andc1
   (lambda ((integer? i) (integer? j)) integer?)
   (pure))
 
 (bitwise-andc2
   (lambda ((integer? i) (integer? j)) integer?)
   (pure))
 
 (bitwise-orc1
   (lambda ((integer? i) (integer? j)) integer?)
   (pure))
 
 (bitwise-orc2
   (lambda ((integer? i) (integer? j)) integer?)
   (pure))
 
 (arithmetic-shift
   (lambda ((integer? i) (integer? count)) integer?)
   (pure))
 
 (bit-count
   (lambda ((integer? i)) integer?)
   (pure))
 
 (integer-length
   (lambda ((integer? i)) integer?)
   (pure))
 
 (bitwise-if
   (lambda ((integer? mask) (integer? i) (integer? j)) integer?)           
   (pure))
 
 (bit-set?
   (lambda ((integer? index) (integer? i)) boolean?)
   (pure))
 
 (copy-bit
   (lambda ((integer? index) (integer? i) (boolean? boolean)) integer?)
   (pure))
 
 (bit-swap
   (lambda ((integer? index1) (integer? index2) (integer? i)) integer?)
   (pure))
 
 (any-bit-set?
   (lambda ((integer? test-bits) (integer? i)) boolean?)
   (pure))
 
 (every-bit-set?
   (lambda ((integer? test-bits) (integer? i)) boolean?)
   (pure))
 
 (first-set-bit
   (lambda ((integer? i)) integer?)
   (pure))
 
 (bit-field
   (lambda ((integer? i) (integer? start) (integer? end)) integer?)
   (pure))
 
 (bit-field-any?
   (lambda ((integer? i) (integer? start) (integer? end)) boolean?)
   (pure))
 
 (bit-field-every?
   (lambda ((integer? i) (integer? start) (integer? end)) boolean?)
   (pure))
 
 (bit-field-clear
   (lambda ((integer? i) (integer? start) (integer? end)) integer?)
   (pure))
 
 (bit-field-set
   (lambda ((integer? i) (integer? start) (integer? end)) integer?)
   (pure))
 
 (bit-field-replace
   (lambda ((integer? dest) (integer? source) (integer? start) (integer? end)) integer?)
   (pure))
 
 (bit-field-replace-same
   (lambda ((integer? dest) (integer? source) (integer? start) (integer? end)) integer?)
   (pure))
 
 (bit-field-rotate
   (lambda ((integer? i) (integer? count) (integer? start) (integer? end)) integer?)
   (pure))
 
 (bit-field-reverse
   (lambda ((integer? i) (integer? start) (integer? end)) integer?)
   (pure))
 
 (bits->list
   (lambda ((integer? i)) list?)
   (pure))
 
 (bits->list
   (lambda ((integer? i) (integer? len)) list?)
   (pure))
 
 (bits->vector
   (lambda ((integer? i)) vector?)
   (pure))
 
 (bits->vector
   (lambda ((integer? i) (integer? len)) vector?)
   (pure))
 
 (list->bits
   (lambda ((list? list)) integer?)
   (pure))

 (vector->bits
   (lambda ((vector? vector)) integer?)
   (pure))
 
 (bits
   (lambda ((boolean? bool) ...) integer?)
   (pure))
 
 (bitwise-fold
   (lambda ((procedure? proc) seed (integer? i)) *)
   (pure)
   ((proc (lambda ((boolean? bit) state) *))))
 
 (bitwise-for-each
   (lambda ((procedure? proc) (integer? i)) undefined)
   ()
   ((proc (lambda ((boolean? bit)) undefined))))
 
 (bitwise-unfold
   (lambda ((procedure? stop?) (procedure? mapper) (procedure? successor) seed) integer?)
   (pure)
   ((stop? (lambda (seed) boolean?))
    (mapper (lambda (seed) boolean?))
    (successor (lambda (seed) *))))
 
 (make-bitwise-generator
   (lambda ((integer? i)) procedure?)
   ()
   ((return (lambda () boolean?))))
 )
