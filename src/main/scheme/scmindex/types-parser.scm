(define-library
  (scmindex types-parser)
  (import (scheme base)
          (scheme cxr)
          (scheme file)
          (scheme read)
          (scheme write)
          (only (srfi 1) lset-adjoin lset-difference alist-cons alist-delete delete-duplicates)
          )
  
  (export read-specs 
          make-type-maps
          flatten-type
          func->json
          
          make-func
          func?
          func-lib
          func-name
          func-param-names
          func-signature
          func-param-signatures
          func-tags
          func-param-types
          func-return-types
          func-supertypes
          
          )
  (include "types-parser-impl.scm"))
