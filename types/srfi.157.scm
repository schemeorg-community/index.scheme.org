(((name . "with-continuation-mark")
  (signature syntax-rules () ((_ key value expression)))
  (desc . "The <key> expression is evaluated to obtain a key, the <value> expression is evaluated to obtain a value, the key is mapped to the value as a continuation mark in the current continuation's initial continuation (if the frame already has a mark for the key, the mark is replaced), and, finally, the <expression> is evaluated. The continuation for evaluating <expression> is the continuation of the with-continuation-mark expression (so the result of the <expression> is the result of the with-continuation-mark expression, and the <expression> is in tail context if the with-continuation-mark expression is)."))
 ((name . "current-continuation-marks")
  (signature lambda () continuation-marks?)
  (desc . "Returns an object called a set of continuations marks, which at some point in the future can be asked (by the continuation-mark-set->list, continuation-mark-set->list* and continuation-mark-set-first procedures) to deliver the set of continuation marks of the continuation of the call to current-continuation-marks for a given key."))
 ((name . "continuation-marks?")
  (signature lambda (obj) boolean?)
  (tags pure)
  (desc . "Returns #t if obj is a set of continuation marks, and #f otherwise. Note that sets of continuation marks are not necessarily disjoint from other Scheme types such as lists."))
 ((name . "continuation-mark-set->list")
  (signature lambda ((continuation-marks? marks) ) list?)
  (desc . "Returns a newly allocated list containing the marks for the key in the continuation mark set marks."))
 ((name . "continuation-mark-set->list*")
  (signature case-lambda 
             (((continuation-marks? marks) list) list?)
             (((continuation-marks? marks) list default) list?))
  (subsigs
    (return (list vector?)))
  (desc . "Returns a newly allocated list containing vectors of marks in the continuation mark set marks. The length of each vector in the result list is the same as the length of the key list, and a value in a particular vector position is the value for the corresponding key in list. Values for multiple keys appear in a single vector only when the marks are for the same continuation frame in the continuation mark set marks. The object default is used for vector elements to indicate the lack of a value."))
 ((name . "continuation-mark-set-first")
  (signature case-lambda 
             (((continuation-marks? marks) key) *)
             (((continuation-marks? marks) key default) *))
  (desc . "Returns the first element of the list that would be returned by (continuation-mark-set->list marks key), or default if the result would be the empty list.
Semantically equivalent to, but may be more efficient than:
  (let ((lst (continuation-mark-set->list marks key))
    (if (not (null? lst))
        (car lst)
        default)))"))
 ((name . "call-with-immediate-continuation-mark")
  (signature case-lambda 
             ((key (procedure? proc)) *)
             ((key (procedure? proc) default) *))
  (subsigs
    (proc (lambda (mark-value) *)))
  (desc . "Tail-calls proc with the value associated with key in the first frame of the current continuation (i.e., a value that would be replaced in the set of current continuation marks if the call to call-with-immediate-continuation-mark were replaced with a with-continuation-mark form using key as the key expression). If no such value exists in the first frame, default is passed to proc.
Semantically equivalent to, but may be more efficient than:
  (let ((secret-key (vector #f)))
    (with-continuation-mark secret-key #f
      (let ((marks
            (continuation-mark-set->list* (current-continuation-marks)
                                          (list key secret-key)
                                          default))
        (proc (vector-ref (car marks) 0)))))")))
