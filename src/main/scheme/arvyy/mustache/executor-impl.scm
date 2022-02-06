(define (html-escape writer value)
  (define str-value
    (let ((out (open-output-string)))
     (writer value out)
     (get-output-string out)))
  (define out (open-output-string))
  (string-for-each
      (lambda (char)
        (case char
          ((#\&) (write-string "&amp;" out))
          ((#\<) (write-string "&lt;" out))
          ((#\>) (write-string "&gt;" out))
          ((#\") (write-string "&quot;" out))
          (else (write-char char out))))
      str-value)
  (get-output-string out))

(define (lookup-in-stack-single name objs-stack lookup)
  (let loop ((objs objs-stack))
   (if (null? objs)
       (values objs #f)
       (lookup (car objs) 
               name
               (lambda (value) (values objs value))
               (lambda () (loop (cdr objs)))))))

(define (lookup-in-stack name-lst objs-stack lookup)
  (define-values (objs value)
                 (lookup-in-stack-single (car name-lst) objs-stack lookup))
  (cond
    ((not value) #f)
    ((null? (cdr name-lst)) value)
    (else (lookup-in-stack (cdr name-lst)
                           (list value)
                           lookup))))

(define (execute template objs-stack partials out lookup collection? collection-empty? collection-for-each writer)
  (define (execute-h template indent objs-stack)
    (for-each
      (lambda (fragment)
        (cond
          ((string? fragment)
           (write-string fragment out))
          ((new-line? fragment)
           (begin
             (write-string (new-line-content fragment) out)
             (write-string (make-string indent #\space) out)))
          ((interp? fragment)
           (let* ((name (interp-ref fragment))
                  (value (if (equal? '(".") name)
                             (car objs-stack)
                             (lookup-in-stack name
                                              objs-stack
                                              lookup))))
             (if (interp-escape? fragment)
                 (write-string (html-escape writer value) out)
                 (writer value out))))

          ((section? fragment)
           (let* ((name (section-ref fragment))
                  (value (if (equal? '(".") name)
                             (car objs-stack)
                             (lookup-in-stack (section-ref fragment)
                                              objs-stack
                                              lookup)))
                  (inner-template (section-content fragment)))
             
             (cond
               ((not value)
                (when (section-invert? fragment)
                  (execute-h inner-template indent objs-stack)))
               ((not (collection? value))
                (unless (section-invert? fragment)
                  (execute-h inner-template indent (cons value objs-stack))))
               (else
                 (if (section-invert? fragment)
                     (when (collection-empty? value)
                       (execute-h inner-template indent objs-stack))
                     (collection-for-each
                       (lambda (el)
                         (execute-h inner-template indent (cons el objs-stack)))
                       value))))))
          
          ((partial? fragment)
           (let ()
            (define partial-tpl
              (cond
                ((assoc (partial-name fragment) partials) => cdr)
                (else #f)))
            (when partial-tpl
              (execute-h partial-tpl 
                         (+ indent (partial-indent fragment))
                         objs-stack) )))
          
          (else (error "Unknown fragment"))))
      template))
  (execute-h template 0 objs-stack))
