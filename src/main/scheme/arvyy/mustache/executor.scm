(define-library
  (arvyy mustache executor)
  (import (scheme base)
          (arvyy mustache parser))
  (export execute)
  (include "executor-impl.scm"))
