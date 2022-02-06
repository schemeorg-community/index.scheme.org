(define-library
  (arvyy mustache)
  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (arvyy mustache lookup)
          (arvyy mustache collection)
          (prefix (arvyy mustache executor) executor-)
          (arvyy mustache parser)
          (arvyy mustache tokenizer)
          (srfi 1))
  (export 
          execute
          compile
          current-lookup
          current-collection
          current-writer
          
          compose-lookups
          alist-lookup
          
          collection
          compose-collections
          vector-collection
          list-collection
          stream-collection)
  (include "mustache-impl.scm"))
