(define-library
  (arvyy mustache parser)
  (import (scheme base)
          (scheme write)
          (scheme cxr)
          (arvyy mustache tokenizer)
          (srfi 1))
  (export
    parse
    interp? interp-ref interp-escape?
    section? section-ref section-invert? section-content section-raw-content
    partial? partial-name partial-indent
    new-line? new-line-content)
  (include "parser-impl.scm"))
