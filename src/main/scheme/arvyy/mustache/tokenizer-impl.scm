(define-record-type <token-ws>
                    (token-ws count)
                    token-ws?
                    (count token-ws-count))

(define-record-type <token-nl>
                    (token-nl chars)
                    token-nl?
                    (chars token-nl-chars))

(define-record-type <token-comment>
                    (token-comment)
                    token-comment?)

(define-record-type <token-str>
                    (token-str content)
                    token-str?
                    (content token-str-content))

(define-record-type <token-delimchanger>
                    (token-delimchager open close)
                    token-delimchager?
                    (open token-delimchager-open)
                    (close token-delimchager-close))

(define-record-type <token-interp>
                    (token-interp tag escape?)
                    token-interp?
                    (tag token-interp-tag)
                    (escape? token-interp-escape?))

(define-record-type <token-section-open>
                    (token-section-open tag inverted?)
                    token-section-open?
                    (tag token-section-open-tag)
                    (inverted? token-section-open-inverted?))

(define-record-type <token-section-close>
                    (token-section-close tag)
                    token-section-close?
                    (tag token-section-close-tag))

(define-record-type <token-partial>
                    (token-partial tag)
                    token-partial?
                    (tag token-partial-tag))

(define (read-tokens str)
  (let loop ((in (string->list str))
             (ws-count 0)
             (str-value '())
             (open-delim '(#\{ #\{))
             (close-delim '(#\} #\}))
             (result/inv '()))
    
    (define (resolve-ws/str)
      (cond
        ;; unflushed ws and str info
        ((and (not (null? str-value))
              (> ws-count 0))
         (append (list (token-ws ws-count)
                       (token-str (list->string (reverse str-value))))
                 result/inv))
        
        ;; unflushed str info
        ((not (null? str-value))
         (cons (token-str (list->string (reverse str-value)))
               result/inv))
        
        ;; unflushed ws info
        ((> ws-count 0)
         (cons (token-ws ws-count)
               result/inv))
        
        ;; no unflushed info
        (else result/inv)))
    
    ;; handle when in is null; ie final function return
    (define (return)
      (define final-result/inv (resolve-ws/str))
      (reverse final-result/inv))
    
    ;; handle after tag read
    (define (continue-after-tag in token)
      (loop
        in
        0
        '()
        open-delim
        close-delim
        (cons token (resolve-ws/str))))
    
    (define (process-interp in)
      (define-values (in* tag)
                     (read-tag in close-delim))
      (continue-after-tag in* (token-interp tag #t)))
    
    (define (process-triple-mustache in)
      (define-values (in* tag)
                     (read-tag in '(#\} #\} #\})))
      (continue-after-tag in* (token-interp tag #f)))
    
    (define (process-ampersand in)
      (define-values (in* tag)
                     (read-tag in close-delim))
      (continue-after-tag in* (token-interp tag #f)))
    
    (define (process-inverted in)
      (define-values (in* tag)
                     (read-tag in close-delim))
      (continue-after-tag in* (token-section-open tag #t)))
    
    (define (process-section in)
      (define-values (in* tag)
                     (read-tag in close-delim))
      (continue-after-tag in* (token-section-open tag #f)))
    
    (define (process-close in)
      (define-values (in* tag)
                     (read-tag in close-delim))
      (continue-after-tag in* (token-section-close tag)))
    
    (define (process-partial in)
      (define-values (in* tag)
                     (read-tag in close-delim))
      (continue-after-tag in* (token-partial tag)))
    
    (define (process-comment in)
      (let loop* ((in in))
       (cond
         ((null? in) (error "Unexpected EOF"))
         ((match-follows in close-delim) => (lambda (in*)
                                              (continue-after-tag in* (token-comment))))
         (else (loop* (cdr in))))))
    
    (define (process-delim-change in)
      (let*-values (((in new-open) (read-tag in #f))
                    ((in new-close) (read-tag in (cons #\= close-delim))))
                   (loop in
                         0
                         '()
                         (string->list new-open)
                         (string->list new-close)
                         (cons (token-delimchager new-open new-close) 
                               (resolve-ws/str)))))
    
    (define (process-open-delim in*)
      (cond
        ((match-follows in* '(#\&)) => process-ampersand)
        ((match-follows in* '(#\^)) => process-inverted)
        ((match-follows in* '(#\#)) => process-section)
        ((match-follows in* '(#\/)) => process-close)
        ((match-follows in* '(#\>)) => process-partial)
        ((match-follows in* '(#\=)) => process-delim-change)
        ((match-follows in* '(#\!)) => process-comment)
        (else (process-interp in*))))
    
    (define (process-space in*)
      (loop in*
            (+ 1 ws-count)
            str-value
            open-delim
            close-delim
            result/inv))
    
    (define (process-eol in* chars)
      (loop in*
            0
            '()
            open-delim
            close-delim
            (cons (token-nl chars)
                  (resolve-ws/str))))
    
    (define (process-nl in*)
      (process-eol in* '(#\newline)))
    
    (define (process-crnl in*)
      (process-eol in* '(#\return #\newline)))
    
    (define (process-char)
      (loop (cdr in)
            0
            (append (list (car in))
                    (make-list ws-count #\space)
                    str-value)
            open-delim
            close-delim
            result/inv))
    
    ;; loop handler
    (cond
      ((null? in) (return))
      ((match-follows in '(#\{ #\{ #\{)) => process-triple-mustache)
      ((match-follows in open-delim) => process-open-delim)
      ((match-follows in '(#\space)) => process-space)
      ((match-follows in '(#\newline)) => process-nl)
      ((match-follows in '(#\return #\newline)) => process-crnl)
      (else (process-char)))))

(define (match-follows in chars)
  (let loop ((in* in)
             (chars* chars))
    (cond
      ((null? chars*) in*)
      ((null? in*) #f)
      ((char=? (car in*) (car chars*)) 
       (loop (cdr in*)
             (cdr chars*)))
      (else #f))))

(define (skip-spaces in)
  (cond
    ((null? in) '())
    ((char=? (car in) #\space) (skip-spaces (cdr in)))
    (else in)))

(define (read-tag in close-delim)
  (define-values
    (tag in*)
    (let loop ((in (skip-spaces in))
               (result '()))
      (define (return)
        (values (list->string (reverse result))
                in))
      (cond
        ((null? in) (error "Unexpected EOF"))
        ((char=? (car in) #\space) (return))
        ((and close-delim (match-follows in close-delim)) 
         (return))
        (else (loop (cdr in)
                    (cons (car in) result))))))
  (cond
    ((not close-delim) (values in* tag))
    ((match-follows (skip-spaces in*) close-delim) => (lambda (in**)
                                                        (values in** tag)))
    (else (error "Bad tag"))))
