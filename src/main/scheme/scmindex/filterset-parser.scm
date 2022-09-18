#|

|#
(define-library
  (scmindex filterset-parser)
  (import (scheme base)
          (scheme file)
          (scheme read)
          (arvyy slf4j)
          (scmindex domain)
          (scmindex util))
  (export read-filters)
  (begin

    (define logger (get-logger "filterset-parser"))
    
    (define (read-filters index-file)
      (log-info logger "Reading filtersets from index file {}" index-file)
      (with-input-from-file 
        index-file
        (lambda ()
          (define lst (read))
          (define names
            (map (lambda (entry)
                   (define code (cdr (assoc 'code entry)))
                   (define name (cdr (assoc 'name entry)))
                   (cons code name))
                 lst))
          (define filters
            (map
              (lambda (entry)
                (define code (cdr (assoc 'code entry)))
                (define file (cdr (assoc 'file entry)))
                (log-info logger "Reading filterset from data file {} for filter {}" code file)
                (with-input-from-file
                  file
                  (lambda ()
                    (read-filter code
                                 (read)))))
              lst))
          (values 
            names
            (apply append filters)))))

    (define (read-filter filtername input)
      (map
        (lambda (entry)
          (define source (write* (car entry)))
          (define target
            (cond
              ((equal? #t (cdr entry)) source)
              (else (write* (cdr entry)))))
          (make-filter-entry filtername source target))
        input))
    
))
