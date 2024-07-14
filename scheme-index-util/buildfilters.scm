(import (chicken file)
        (chicken process-context)
        json
        matchable)

(define (main)
  (define index-file-name (list-ref (command-line-arguments) 0))
  (define output-file-name (list-ref (command-line-arguments) 1))
  (process-filters-index index-file-name output-file-name))

(define (make-json-sink out)
  (define empty #t)
  (display "[" out)
  (lambda (obj)
    (cond
      ((eq? obj 'eof)
       (display "]\n" out))
      (else
        (begin
          (unless empty
            (display "\n, " out))
          (when empty
            (set! empty #f))
          (json-write obj out))))))

(define (->string obj)
  (let ((out (open-output-string)))
    (write obj out)
    (get-output-string out)))

(define (process-filters-index index-file-name output-file-name)
  (define (process-filter filter json-sink)
    (define code
      (cond
        ((assoc 'code filter) => cdr)
        (else (error "Missing code"))))
    (define name
      (cond
        ((assoc 'name filter) => cdr)
        (else (error "Missing name"))))
    (define file
      (cond
        ((assoc 'file filter) => cdr)
        (else (error "Missing file"))))
    (define libs
      (let ((content (with-input-from-file file read)))
        (map
          (lambda (e)
            (->string (car e)))
          content)))
    (define json-entry
      `#((code . ,code)
         (name . ,name)
         (libs . ,libs)))
    (json-sink json-entry))

  (define filters (with-input-from-file index-file-name read))

  (with-output-to-file output-file-name
    (lambda ()
      (let ((json-sink (make-json-sink (current-output-port))))
        (for-each
          (lambda (filter)
            (process-filter filter json-sink))
          filters)
        (json-sink 'eof)))))

(main)
