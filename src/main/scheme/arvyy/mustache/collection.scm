(define-library
  (arvyy mustache collection)
  (import (scheme base)
          (srfi 41))
  (export 
    collection
    collection-pred-proc
    collection-empty?-proc
    collection-for-each-proc
    
    compose-collections
    vector-collection
    stream-collection
    list-collection)
  (begin

    (define-record-type <collection>
                        (collection pred-proc empty?-proc for-each-proc)
                        collection?
                        (pred-proc collection-pred-proc)
                        (empty?-proc collection-empty?-proc)
                        (for-each-proc collection-for-each-proc))

    (define vector-collection
      (collection vector? 
                  (lambda (v) (= 0 (vector-length v)))
                  vector-for-each))

    (define list-collection
      (collection list?
                  null?
                  for-each))
    
    (define stream-collection
      (collection stream?
                  stream-null?
                  stream-for-each))

    (define (compose-collections . collections)
      (define (find-collection object)
        (let loop ((collections collections))
         (cond
           ((null? collections)
            #f)
           (((collection-pred-proc (car collections)) object)
            (car collections))
           (else (loop (cdr collections))))))

      (collection
        ;; predicate
        (lambda (object)
          (cond
            ((find-collection object) #t)
            (else #f)))
        ;; empty proc
        (lambda (object)
          (cond
            ((find-collection object) => (lambda (c) ((collection-empty?-proc c) object)))
            (else (error "Collection not found"))))
        ;; for-each proc
        (lambda (proc object)
          (cond
            ((find-collection object) => (lambda (c) ((collection-for-each-proc c) proc object)))
            (else (error "Collection not found"))))))))
