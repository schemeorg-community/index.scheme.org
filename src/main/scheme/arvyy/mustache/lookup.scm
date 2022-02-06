(define-library
  (arvyy mustache lookup)
  (import (scheme base))
  (export
    compose-lookups
    alist-lookup)
  (begin

    (define (compose-lookups . lookups)
      (lambda (obj name found not-found)
        (let loop ((lookups lookups))
         (if (null? lookups)
             (not-found)
             (let ((l (car lookups)))
              (l obj name found (lambda () 
                                  (loop (cdr lookups)))))))))

    (define (alist-lookup obj name found not-found)
      (define key (string->symbol name))
      (define alist? (and (list? obj)
                          (or (null? obj)
                              (pair? (car obj)))))
      (if alist?
          (cond
            ((assoc key obj) => (lambda (pair) (found (cdr pair))))
            (else (not-found)))
          (not-found)))))
