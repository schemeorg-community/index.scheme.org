(define-library 
  (srfi 180)
  (export json-number-of-character-limit
          json-nesting-depth-limit
          json-null?
          json-error?
          json-error-reason
          json-fold
          json-generator
          json-read
          json-lines-read
          json-sequence-read
          json-accumulator
          json-write)

  (import (scheme base)
          (scheme inexact)
          (scheme case-lambda)
          (scheme char)
          (scheme write)
          (only (srfi 60) arithmetic-shift bitwise-ior))

  (include "180-impl.scm"))
