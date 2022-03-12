(
 (cons
   (lambda (a d) pair?)
   (pure))
 
 (list
   (lambda (object ...) list?)
   (pure))
 
 (xcons
   (lambda (d a) pair?)
   (pure))
 
 (cons*
   (lambda (elt1 elt2 ...) *))
 
 (make-list
   (lambda ((integer? n)) list?))
 
 (make-list
   (lambda ((integer? n) fill) list?)
   (pure))
 
 (list-tabulate
   (lambda ((integer? n) (procedure? init-proc)) list?)
   (pure)
   ((init-proc (lambda ((integer? i)) *))))
 
 (list-copy
   (lambda ((list? flist)) list?)
   (pure))
 
 (list-copy
   (lambda ((dotted-list? flist)) dotted-list?)
   (pure))
 
 (circular-list
   (lambda (elt1 elt2 ...) circular-list?)
   (pure))
 
 (iota
   (lambda ((integer? count)) list?)
   (pure))
 
 (iota
   (lambda ((integer? count) (number? start)) list?)
   (pure))
 
 (iota
   (lambda ((integer? count) (number? start) (number? step)) list?)
   (pure))
 
 (proper-list?
   (lambda (obj) boolean?)
   (pure predicate)
   ())
 
 (circular-list?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (pair?))
 
 (dotted-list?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (pair?))
 
 (pair?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (null?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (null-list?
   (lambda ((list? lst)) boolean?)
   (pure predicate))
 
 (null-list?
   (lambda ((circular-list? lst)) boolean?)
   (pure predicate))
 
 (not-pair?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (list=
   (lambda ((procedure? elt=) (list? list1) ...) boolean?)
   (pure)
   ((elt= (lambda (a b) *))))
 
 (caar
   (lambda ((pair? pair)) *)
   (pure))

 (cadr
  (lambda ((pair? pair)) *)
  (pure))
 
 (car
   (lambda ((pair? pair)) *)
   (pure))
 
 (cdar
   (lambda ((pair? pair)) *)
   (pure))

 (cddr
   (lambda ((pair? pair)) *)
   (pure))

 (cdr
   (lambda ((pair? pair)) *)
   (pure))
 
 (caaaar
   (lambda ((pair? pair)) *)
   (pure))

 (caaadr
   (lambda ((pair? pair)) *)
   (pure))

 (caaar
   (lambda ((pair? pair)) *)
   (pure))

 (caadar
   (lambda ((pair? pair)) *)
   (pure))

 (caaddr
   (lambda ((pair? pair)) *)
   (pure))

 (caadr
   (lambda ((pair? pair)) *)
   (pure))

 (cadaar
   (lambda ((pair? pair)) *)
   (pure))

 (cadadr
   (lambda ((pair? pair)) *)
   (pure))

 (cadar
   (lambda ((pair? pair)) *)
   (pure))

 (caddar
   (lambda ((pair? pair)) *)
   (pure))

 (cadddr
   (lambda ((pair? pair)) *)
   (pure))

 (caddr
   (lambda ((pair? pair)) *)
   (pure))

 (cdaaar
   (lambda ((pair? pair)) *)
   (pure))

 (cdaadr
   (lambda ((pair? pair)) *)
   (pure))

 (cdaar
   (lambda ((pair? pair)) *)
   (pure))

 (cdadar
   (lambda ((pair? pair)) *)
   (pure))

 (cdaddr
   (lambda ((pair? pair)) *)
   (pure))

 (cdadr
   (lambda ((pair? pair)) *)
   (pure))

 (cddaar
   (lambda ((pair? pair)) *)
   (pure))

 (cddadr
   (lambda ((pair? pair)) *)
   (pure))

 (cddar
   (lambda ((pair? pair)) *)
   (pure))

 (cdddar
   (lambda ((pair? pair)) *)
   (pure))

 (cddddr
   (lambda ((pair? pair)) *)
   (pure))

 (cdddr
   (lambda ((pair? pair)) *)
   (pure))
 
 (list-ref
   (lambda ((list? clist) (integer? i)) *)
   (pure))
 
 (list-ref
   (lambda ((circular-list? clist) (integer? i)) *)
   (pure))
 
 (first
   (lambda ((pair? pair)) *)
   (pure))
 
 (second
   (lambda ((pair? pair)) *)
   (pure))
 
 (third
   (lambda ((pair? pair)) *)
   (pure))
 
 (fourth
   (lambda ((pair? pair)) *)
   (pure))
 
 (fifth
   (lambda ((pair? pair)) *)
   (pure))
 
 (sixth
   (lambda ((pair? pair)) *)
   (pure))
 
 (seventh
   (lambda ((pair? pair)) *)
   (pure))
 
 (eighth
   (lambda ((pair? pair)) *)
   (pure))
 
 (ninth
   (lambda ((pair? pair)) *)
   (pure))
 
 (tenth
   (lambda ((pair? pair)) *)
   (pure))
 
 (car+cdr
   (lambda ((pair? pair)) (values * *))
   (pure))
 
 (take
   (lambda ((list? x) (integer? i)) list?)
   (pure))
 
 (take
   (lambda ((circular-list? x) (integer? i)) list?)
   (pure))
 
 (take
   (lambda ((dotted-list? x) (integer? i)) list?)
   (pure))
 
 (drop
   (lambda ((list? x) (integer? i)) list?)
   (pure))
 
 (drop
   (lambda ((circular-list? x) (integer? i)) circular-list?)
   (pure))
 
 (drop
   (lambda ((dotted-list? x) (integer? i)) *)
   (pure))
 
 (take-right
   (lambda ((list? flist) (integer? i)) list?)
   (pure))
 
 (take-right
   (lambda ((dotted-list? flist) (integer? i)) *)
   (pure))
 
 (drop-right
   (lambda ((list? flist) (integer? i)) list?)
   (pure))
 
 (drop-right
   (lambda ((dotted-list? flist) (integer? i)) list?)
   (pure))
 
 (take!
   (lambda ((list? x) (integer? i)) list?))
 
 (take!
   (lambda ((circular-list? x) (integer? i)) list?))
 
 (take!
   (lambda ((dotted-list? x) (integer? i)) list?))
 
 (drop-right!
   (lambda ((list? flist) (integer? i)) list?))
 
 (drop-right!
   (lambda ((dotted-list? flist) (integer? i)) list?))
 
 (split-at
   (lambda ((list? x) (integer? i)) (values list? list?))
   (pure))
 
 (split-at
   (lambda ((circular-list? x) (integer? i)) (values list? circular-list?))
   (pure))
 
 (split-at
   (lambda ((dotted-list? x) (integer? i)) (values list? *))
   (pure))
 
 (split-at!
   (lambda ((list? x) (integer? i)) (values list? list?)))
 
 (split-at!
   (lambda ((circular-list? x) (integer? i)) (values list? circular-list?)))
 
 (split-at!
   (lambda ((dotted-list? x) (integer? i)) (values list? *)))
 
 (last
   (lambda ((list? pair)) *)
   (pure))
 
 (last-pair
   (lambda ((list? pair)) pair?)
   (pure))
 
 (length
   (lambda ((list? list)) integer?)
   (pure))
 
 (length+
   (lambda ((list? clist)) integer?)
   (pure))
 
 (length+
   (lambda ((circular-list? clist)) boolean?)
   (pure))
 
 (append
   (lambda ((list? list) ...)  list?)
   (pure))

 (append
   (lambda ((list? list) ... obj)  *)
   (pure))
 
 (append!
   (lambda ((list? list) ...)  list?))

 (append!
   (lambda ((list? list) ... obj)  *))
 
 (concatenate
   (lambda ((list? list-of-lists)) *)
   (pure))
 
 (concatenate!
   (lambda ((list? list-of-lists)) *))
 
 (reverse
   (lambda ((list? list)) list?)
   (pure))
 
 (reverse!
   (lambda ((list? list)) list?))
 
 (append-reverse
   (lambda ((list? rev-head) (list? tail)) list?)
   (pure))
 
 (append-reverse
   (lambda ((list? rev-head) tail) *)
   (pure))
 
 (append-reverse!
   (lambda ((list? rev-head) (list? tail)) list?))
 
 (append-reverse!
   (lambda ((list? rev-head) tail) *))
 
 (zip
   (lambda ((list? clist1) (list? clist2) ...) list?)
   (pure))
 
 (unzip1
   (lambda ((list? list)) list?)
   (pure))
 
 (unzip2
   (lambda ((list? list)) (values list? list?))
   (pure))
 
 (unzip3
   (lambda ((list? list)) (values list? list? list?))
   (pure))
 
 (unzip4
   (lambda ((list? list)) (values list? list? list? list?))
   (pure))
 
 (unzip5
   (lambda ((list? list)) (values list? list? list? list? list?))
   (pure))
 
 (count
   (lambda ((procedure? pred) (list? clist1) (list? clist2) ...) integer?)
   (pure)
   ((pred (lambda (obj ...) *))))
 
 (fold
   (lambda ((procedure? kons) knil (list? clist1) (list? clist2) ...) *)
   (pure)
   ((kons (lambda (obj1 obj2 ... fold-state) *))))
 
 (fold-right
   (lambda ((procedure? kons) knil (list? clist1) (list? clist2) ...) *)
   (pure)
   ((kons (lambda (obj1 obj2 ... fold-state) *))))
 
 (pair-fold
   (lambda ((procedure? kons) knil (list? clist1) (list? clist2) ...) *)
   (pure)
   ((kons (lambda ((pair? pair1) (pair? pair2) ... fold-state) *))))
 
 (pair-fold-right
   (lambda ((procedure? kons) knil (list? clist1) (list? clist2) ...) *)
   (pure)
   ((kons (lambda ((pair? pair1) (pair? pair2) ... fold-state) *))))
 
 (reduce
   (lambda ((procedure? f) ridentity (list? list))*)
   (pure)
   ((f (lambda (obj fold-state) *))))
 
 (reduce-right
   (lambda ((procedure? f) ridentity (list? list))*)
   (pure)
   ((f (lambda (obj fold-state) *))))
 
 (unfold
   (lambda ((procedure? p) (procedure? f) (procedure? g) seed) list?)
   (pure)
   ((p (lambda (seed) boolean?))
    (f (lambda (seed) *))
    (g (lambda (seed) *))))
 
 (unfold
   (lambda ((procedure? p) (procedure? f) (procedure? g) seed (list? tail-gen)) *)
   (pure)
   ((p (lambda (seed) boolean?))
    (f (lambda (seed) *))
    (g (lambda (seed) *))
    (tail-gen (lambda () *))))
 
 (unfold-right
   (lambda ((procedure? p) (procedure? f) (procedure? g) seed) list?)
   (pure)
   ((p (lambda (seed) boolean?))
    (f (lambda (seed) *))
    (g (lambda (seed) *))))
 
 (unfold-right
   (lambda ((procedure? p) (procedure? f) (procedure? g) seed (list? tail-gen)) *)
   (pure)
   ((p (lambda (seed) boolean?))
    (f (lambda (seed) *))
    (g (lambda (seed) *))
    (tail-gen (lambda () *))))
 
 (map
   (lambda ((procedure? proc) (list? list1) (list? list2) ...) list?)
   (pure)
   ((proc (lambda (obj1 obj2 ...) *))))
 
 (for-each
   (lambda ((procedure? proc) (list? list1) (list? list2) ...) undefined)
   ()
   ((proc (lambda (obj1 obj2 ...) undefined))))
 
 (append-map
   (lambda ((procedure? proc) (list? list1) (list? list2) ...) list?)
   (pure)
   ((proc (lambda (obj1 obj2 ...) list?))))
 
 (append-map!
   (lambda ((procedure? proc) (list? list1) (list? list2) ...) list?)
   ()
   ((proc (lambda (obj1 objs2 ...) list?))))
 
 (map!
   (lambda ((procedure? proc) (list? list1) (list? list2) ...) list?)
   ()
   ((proc (lambda (obj1 obj2 ...) *))))
 
 (map-in-order
   (lambda ((procedure? proc) (list? list1) (list? list2) ...) list?)
   ()
   ((proc (lambda (obj1 obj2 ...) *))))
 
 (pair-for-each
   (lambda ((procedure? proc) (list? list1) (list? list2) ...) undefined)
   ()
   ((proc (lambda ((pair? obj1) (pair? obj2) ...) undefined))))
 
 (filter-map
   (lambda ((procedure? proc) (list? list1) (list? list2) ...) list?)
   (pure)
   ((proc (lambda (obj1 obj2 ...) *))))
 
 (filter
   (lambda ((procedure? pred) (list? list)) list?)
   (pure)
   ((pred (lambda (obj) *))))
 
 (filter!
   (lambda ((procedure? pred) (list? list)) list?)
   ()
   ((pred (lambda (obj) *))))
 
 (partition
   (lambda ((procedure? pred) (list? list)) (values list? list?))
   (pure)
   ((pred (lambda (obj) *))))
 
 (partition!
   (lambda ((procedure? pred) (list? list)) (values list? list?))
   ()
   ((pred (lambda (obj) *))))
 
 (remove
   (lambda ((procedure? pred) (list? list)) list?)
   (pure)
   ((pred (lambda (obj) *))))
 
 (remove!
   (lambda ((procedure? pred) (list? list)) list?)
   ()
   ((pred (lambda (obj) *))))
 
 (find
   (lambda ((procedure? pred) (list? list)) *)
   (pure)
   ((pred (lambda (obj) *))))
 
 (find-tail
   (lambda ((procedure? pred) (list? list)) (or pair? #f))
   (pure)
   ((pred (lambda (obj) *))))
 
 (take-while
   (lambda ((procedure? pred) (list? list)) list?)
   (pure)
   ((pred (lambda (obj) *))))
 
 (take-while!
   (lambda ((procedure? pred) (list? list)) list?)
   ()
   ((pred (lambda (obj) *))))
 
 (drop-while
   (lambda ((procedure? pred) (list? list)) list?)
   (pure)
   ((pred (lambda (obj) *))))
 
 (drop-while
   (lambda ((procedure? pred) (circular-list? list)) circular-list?)
   (pure)
   ((pred (lambda (obj) *))))
 
 (span
   (lambda ((procedure? pred) (list? list)) (values list? list?))
   (pure)
   ((pred (lambda (obj) *))))
 
 (span
   (lambda ((procedure? pred) (circular-list? list)) (values list? circular-list?))
   (pure)
   ((pred (lambda (obj) *))))
 
 (span!
   (lambda ((procedure? pred) (list? list)) (values list? list?))
   ()
   ((pred (lambda (obj) *))))
 
 (break
   (lambda ((procedure? pred) (list? list)) (values list? list?))
   (pure)
   ((pred (lambda (obj) *))))
 
 (break
   (lambda ((procedure? pred) (circular-list? list)) (values list? circular-list?))
   (pure)
   ((pred (lambda (obj) *))))
 
 (break!
   (lambda ((procedure? pred) (list? list)) (values list? list?))
   ()
   ((pred (lambda (obj) *))))
 
 (any
   (lambda ((procedure? pred) (list? clist1) (list? clist2) ...) *)
   (pure)
   ((pred (lambda (obj1 obj2 ...) *))))
 
 (every
   (lambda ((procedure? pred) (list? clist1) (list? clist2) ...) *)
   (pure)
   ((pred (lambda (obj1 obj2 ...) *))))
 
 (list-index
   (lambda ((procedure? pred) (list? clist1) (list? clist2) ...) (or integer? #f))
   (pure)
   ((pred (lambda (obj1 obj2 ...) *))))
 
 (member
   (lambda (obj (list? list)) (or #f list?))
   (pure))

 (member
   (lambda (obj (list? list) (procedure? =)) (or #f list?))
   (pure)
   ((= (lambda (obj1 obj2) *))))

 (memq
   (lambda (obj (list? list)) (or #f list?))
   (pure))

 (memv
   (lambda (obj (list? list)) (or 3f list?))
   (pure))
 
 (delete
   (lambda (obj (list? list)) list?)
   (pure))
 
 (delete
   (lambda (obj (list? list) (procedure? =)) list?)
   (pure)
   ((= (lambda (obj1 obj2) *))))
 
 (delete!
   (lambda (obj (list? list)) list?)
   ())
 
 (delete!
   (lambda (obj (list? list) (procedure? =)) list?)
   ()
   ((= (lambda (obj1 obj2) *))))
 
 (delete-duplicates
   (lambda ((list? list)) list?)
   (pure))
 
 (delete-duplicates
   (lambda ((list? list) (procedure? =)) list?)
   (pure)
   ((= (lambda (obj1 obj2) *))))
 
 (delete-duplicates!
   (lambda ((list? list)) list?)
   ())
 
 (delete-duplicates!
   (lambda ((list? list) (procedure? =)) list?)
   ()
   ((= (lambda (obj1 obj2) *))))
 
 (assoc
   (lambda (obj (list? alist)) (or list? #f))
   (pure))

 (assoc
   (lambda (obj (list? alist) (procedure? =)) (or pair? #f))
   (pure)
   ((= (lambda (a b) *))))

 (assq
   (lambda (obj (list? alist)) (or pair? #f))
   (pure))

 (assv
   (lambda (obj (list? alist)) (or pair? #f))
   (pure))
 
 (alist-cons
   (lambda (key datum (list? alist)) list?)
   (pure))
 
 (alist-copy
   (lambda ((list? alist)) list?)
   (pure))
 
 (alist-delete
   (lambda (key (list? alist)) list?)
   (pure))
 
 (alist-delete
   (lambda (key (list? alist) (procedure? =)) list?)
   (pure)
   ((= (lambda (a b) *))))
 
 (alist-delete!
   (lambda (key (list? alist)) list?))
 
 (alist-delete!
   (lambda (key (list? alist) (procedure? =)) list?)
   ()
   ((= (lambda (a b) *))))
 
 (lset<=
   (lambda ((procedure? =) (list? list1) ...) boolean?)
   (pure)
   ((= (lambda (a b) *))))
 
 (lset=
   (lambda ((procedure? =) (list? list1) (list? list2) ...) boolean?)
   (pure)
   ((= (lambda (a b) *))))
 
 (lset-adjoin
   (lambda ((procedure? =) (list? list) elt1 ...) list?)
   (pure)
   ((= (lambda (a b) *))))
 
 (lset-union
   (lambda ((procedure? =) (list? list1) ...) list?)
   (pure)
   ((= (lambda (a b) *))))
 
 (lset-union!
   (lambda ((procedure? =) (list? list1) ...) list?)
   ()
   ((= (lambda (a b) *))))
 
 (lset-intersection
   (lambda ((procedure? =) (list? list1) (list? list2) ...) list?)
   (pure)
   ((= (lambda (a b) *))))
 
 (lset-intersection!
   (lambda ((procedure? =) (list? list1) (list? list2) ...) list?)
   ()
   ((= (lambda (a b) *))))
 
 (lset-difference
   (lambda ((procedure? =) (list? list1) (list? list2) ...) list?)
   (pure)
   ((= (lambda (a b) *))))
 
 (lset-difference!
   (lambda ((procedure? =) (list? list1) (list? list2) ...) list?)
   ()
   ((= (lambda (a b) *))))

 (lset-xor
   (lambda ((procedure? =) (list? list1) ...) list?)
   (pure)
   ((= (lambda (a b) *))))
 
 (lset-xor!
   (lambda ((procedure? =) (list? list1) ...) list?)
   ()
   ((= (lambda (a b) *))))
 
 (lset-diff+intersection
   (lambda ((procedure? =) (list? list1) (list? list2) ...) (values list? list?))
   (pure)
   ((= (lambda (a b) *))))

 (lset-diff+intersection!
   (lambda ((procedure? =) (list? list1) (list? list2) ...) (values list? list?))
   ()
   ((= (lambda (a b) *))))
 
 (set-car!
   (lambda ((pair? pair) object) undefined))
 
 (set-cdr!
   (lambda ((pair? pair) object) undefined))
 
 )
