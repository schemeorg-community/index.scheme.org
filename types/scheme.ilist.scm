(
 (ipair
   (lambda (a d) ipair?)
   (pure))
 
 (ilist
   (lambda (object ...) ilist?)
   (pure))
 
 (xipair
   (lambda (d a) ipair?)
   (pure))
 
 (ipair*
   (lambda (elt1 elt2 ...) *))
 
 (make-ilist
   (lambda ((integer? n)) ilist?))
 
 (make-ilist
   (lambda ((integer? n) fill) ilist?)
   (pure))
 
 (ilist-tabulate
   (lambda ((integer? n) (procedure? init-proc)) ilist?)
   (pure)
   ((init-proc (lambda ((integer? i)) *))))
 
 (ilist-copy
   (lambda ((ilist? dilist)) ilist?)
   (pure))
 
 (ilist-copy
   (lambda ((dotted-ilist? dilist)) dotted-ilist?)
   (pure))
 
 (iiota
   (lambda ((integer? count)) ilist?)
   (pure))
 
 (iiota
   (lambda ((integer? count) (number? start)) ilist?)
   (pure))
 
 (iiota
   (lambda ((integer? count) (number? start) (number? step)) ilist?)
   (pure))
 
 (proper-ilist?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (ilist?))
 
 (ilist?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (ilist?))
 
 (dotted-ilist?
   (lambda (obj) boolean?)
   (pure predicate)
   ()
   (ipair?))
 
 (ipair?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (null-ilist?
   (lambda ((ilist? lst)) boolean?)
   (pure predicate))
 
 (not-ipair?
   (lambda (obj) boolean?)
   (pure predicate))
 
 (ilist=
   (lambda ((procedure? elt=) (ilist? ilist1) ...) boolean?)
   (pure)
   ((elt= (lambda (a b) *))))
 
 (icaar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icadr
  (lambda ((ipair? ipair)) *)
  (pure))
 
 (icar
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (icdar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icddr
   (lambda ((ipair? ipair)) *)
   (pure))

 (icdr
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (icaaaar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icaaadr
   (lambda ((ipair? ipair)) *)
   (pure))

 (icaaar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icaadar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icaaddr
   (lambda ((ipair? ipair)) *)
   (pure))

 (icaadr
   (lambda ((ipair? ipair)) *)
   (pure))

 (icadaar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icadadr
   (lambda ((ipair? ipair)) *)
   (pure))

 (icadar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icaddar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icadddr
   (lambda ((ipair? ipair)) *)
   (pure))

 (icaddr
   (lambda ((ipair? ipair)) *)
   (pure))

 (icdaaar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icdaadr
   (lambda ((ipair? ipair)) *)
   (pure))

 (icdaar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icdadar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icdaddr
   (lambda ((ipair? ipair)) *)
   (pure))

 (icdadr
   (lambda ((ipair? ipair)) *)
   (pure))

 (icddaar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icddadr
   (lambda ((ipair? ipair)) *)
   (pure))

 (icddar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icdddar
   (lambda ((ipair? ipair)) *)
   (pure))

 (icddddr
   (lambda ((ipair? ipair)) *)
   (pure))

 (icdddr
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (ilist-ref
   (lambda ((ilist? ilist) (integer? i)) *)
   (pure))
 
 (ifirst
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (isecond
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (ithird
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (ifourth
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (ififth
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (isixth
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (iseventh
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (ieighth
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (ininth
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (itenth
   (lambda ((ipair? ipair)) *)
   (pure))
 
 (icar+cdr
   (lambda ((ipair? ipair)) (values * *))
   (pure))
 
 (itake
   (lambda ((ilist? x) (integer? i)) ilist?)
   (pure))
 
 (itake
   (lambda ((dotted-ilist? x) (integer? i)) ilist?)
   (pure))
 
 (idrop
   (lambda ((ilist? x) (integer? i)) ilist?)
   (pure))
 
 (idrop
   (lambda ((dotted-ilist? x) (integer? i)) *)
   (pure))
 
 (itake-right
   (lambda ((ilist? dilist) (integer? i)) ilist?)
   (pure))
 
 (itake-right
   (lambda ((dotted-ilist? dilist) (integer? i)) *)
   (pure))
 
 (idrop-right
   (lambda ((ilist? dilist) (integer? i)) ilist?)
   (pure))
 
 (idrop-right
   (lambda ((dotted-ilist? dilist) (integer? i)) ilist?)
   (pure))
 
 (isplit-at
   (lambda ((ilist? x) (integer? i)) (values ilist? ilist?))
   (pure))
 
 (isplit-at
   (lambda ((dotted-ilist? x) (integer? i)) (values ilist? *))
   (pure))
 
 (ilast
   (lambda ((ilist? ipair)) *)
   (pure))
 
 (ilast-ipair
   (lambda ((ilist? ipair)) ipair?)
   (pure))
 
 (ilength
   (lambda ((ilist? ilist)) integer?)
   (pure))
 
 (iappend
   (lambda ((ilist? ilist) ...)  ilist?)
   (pure))

 (iappend
   (lambda ((ilist? ilist) ... obj)  *)
   (pure))
 
 (iconcatenate
   (lambda ((ilist? ilist-of-ilists)) *)
   (pure))
 
 (ireverse
   (lambda ((ilist? ilist)) ilist?)
   (pure))
 
 (iappend-reverse
   (lambda ((ilist? rev-head) (ilist? tail)) ilist?)
   (pure))
 
 (iappend-reverse
   (lambda ((ilist? rev-head) tail) *)
   (pure))
 
 (izip
   (lambda ((ilist? ilist1) (ilist? ilist2) ...) ilist?)
   (pure))
 
 (iunzip1
   (lambda ((ilist? ilist)) ilist?)
   (pure))
 
 (iunzip2
   (lambda ((ilist? ilist)) (values ilist? ilist?))
   (pure))
 
 (iunzip3
   (lambda ((ilist? ilist)) (values ilist? ilist? ilist?))
   (pure))
 
 (iunzip4
   (lambda ((ilist? ilist)) (values ilist? ilist? ilist? ilist?))
   (pure))
 
 (iunzip5
   (lambda ((ilist? ilist)) (values ilist? ilist? ilist? ilist? ilist?))
   (pure))
 
 (icount
   (lambda ((procedure? pred) (ilist? ilist1) (ilist? ilist2) ...) integer?)
   (pure)
   ((pred (lambda (obj ...) *))))
 
 (ifold
   (lambda ((procedure? kons) knil (ilist? ilist1) (ilist? ilist2) ...) *)
   (pure)
   ((kons (lambda (obj1 obj2 ... fold-state) *))))
 
 (ifold-right
   (lambda ((procedure? kons) knil (ilist? ilist1) (ilist? ilist2) ...) *)
   (pure)
   ((kons (lambda (obj1 obj2 ... fold-state) *))))
 
 (ipair-fold
   (lambda ((procedure? kons) knil (ilist? ilist1) (ilist? ilist2) ...) *)
   (pure)
   ((kons (lambda ((ipair? ipair1) (ipair? ipair2) ... fold-state) *))))
 
 (ipair-fold-right
   (lambda ((procedure? kons) knil (ilist? ilist1) (ilist? ilist2) ...) *)
   (pure)
   ((kons (lambda ((ipair? ipair1) (ipair? ipair2) ... fold-state) *))))
 
 (ireduce
   (lambda ((procedure? f) ridentity (ilist? ilist))*)
   (pure)
   ((f (lambda ((obj fold-state)) *))))
 
 (ireduce-right
   (lambda ((procedure? f) ridentity (ilist? ilist))*)
   (pure)
   ((f (lambda ((obj fold-state)) *))))
 
 (iunfold
   (lambda ((procedure? p) (procedure? f) (procedure? g) seed) ilist?)
   (pure)
   ((p (lambda (seed) boolean?))
    (f (lambda (seed) *))
    (g (lambda (seed) *))))
 
 (iunfold
   (lambda ((procedure? p) (procedure? f) (procedure? g) seed (ilist? tail-gen)) *)
   (pure)
   ((p (lambda (seed) boolean?))
    (f (lambda (seed) *))
    (g (lambda (seed) *))
    (tail-gen (lambda () *))))
 
 (iunfold-right
   (lambda ((procedure? p) (procedure? f) (procedure? g) seed) ilist?)
   (pure)
   ((p (lambda (seed) boolean?))
    (f (lambda (seed) *))
    (g (lambda (seed) *))))
 
 (iunfold-right
   (lambda ((procedure? p) (procedure? f) (procedure? g) seed (ilist? tail-gen)) *)
   (pure)
   ((p (lambda (seed) boolean?))
    (f (lambda (seed) *))
    (g (lambda (seed) *))
    (tail-gen (lambda () *))))
 
 (imap
   (lambda ((procedure? proc) (ilist? ilist1) (ilist? ilist2) ...) ilist?)
   (pure)
   ((proc (lambda (obj1 obj2 ...) *))))
 
 (ifor-each
   (lambda ((procedure? proc) (ilist? ilist1) (ilist? ilist2) ...) undefined)
   ()
   ((proc (lambda (obj1 obj2 ...) undefined))))
 
 (iappend-map
   (lambda ((procedure? proc) (ilist? ilist1) (ilist? ilist2) ...) ilist?)
   (pure)
   ((proc (lambda (obj1 obj2 ...) ilist?))))
 
 (imap-in-order
   (lambda ((procedure? proc) (ilist? ilist1) (ilist? ilist2) ...) ilist?)
   ()
   ((proc (lambda (obj1 obj2 ...) *))))
 
 (ipair-for-each
   (lambda ((procedure? proc) (ilist? ilist1) (ilist? ilist2) ...) undefined)
   ()
   ((proc (lambda ((ipair? obj1) (ipair? obj2) ...) undefined))))
 
 (ifilter-map
   (lambda ((procedure? proc) (ilist? ilist1) (ilist? ilist2) ...) ilist?)
   (pure)
   ((proc (lambda (obj1 obj2 ...) *))))
 
 (ifilter
   (lambda ((procedure? pred) (ilist? ilist)) ilist?)
   (pure)
   ((pred (lambda (obj) *))))
 
 (ipartition
   (lambda ((procedure? pred) (ilist? ilist)) (values ilist? ilist?))
   (pure)
   ((pred (lambda (obj) *))))
 
 (iremove
   (lambda ((procedure? pred) (ilist? ilist)) ilist?)
   (pure)
   ((pred (lambda (obj) *))))
 
 (ifind
   (lambda ((procedure? pred) (ilist? ilist)) *)
   (pure)
   ((pred (lambda (obj) *))))
 
 (ifind-tail
   (lambda ((procedure? pred) (ilist? ilist)) (or ipair? #f))
   (pure)
   ((pred (lambda (obj) *))))
 
 (itake-while
   (lambda ((procedure? pred) (ilist? ilist)) ilist?)
   (pure)
   ((pred (lambda (obj) *))))
 
 (idrop-while
   (lambda ((procedure? pred) (ilist? ilist)) ilist?)
   (pure)
   ((pred (lambda (obj) *))))
 
 (ispan
   (lambda ((procedure? pred) (ilist? ilist)) (values ilist? ilist?))
   (pure)
   ((pred (lambda (obj) *))))
 
 (ibreak
   (lambda ((procedure? pred) (ilist? ilist)) (values ilist? ilist?))
   (pure)
   ((pred (lambda (obj) *))))
 
 (iany
   (lambda ((procedure? pred) (ilist? ilist1) (ilist? ilist2) ...) *)
   (pure)
   ((pred (lambda (obj1 obj2 ...) *))))
 
 (ievery
   (lambda ((procedure? pred) (ilist? ilist1) (ilist? ilist2) ...) *)
   (pure)
   ((pred (lambda (obj1 obj2 ...) *))))
 
 (ilist-index
   (lambda ((procedure? pred) (ilist? ilist1) (ilist? ilist2) ...) (or integer? #f))
   (pure)
   ((pred (lambda (obj1 obj2 ...) *))))
 
 (imember
   (lambda (obj (ilist? ilist)) (or #f ilist?))
   (pure))

 (imember
   (lambda (obj (ilist? ilist) (procedure? =)) (or #f ilist?))
   (pure)
   ((= (lambda (obj1 obj2) *))))

 (imemq
   (lambda (obj (ilist? ilist)) (or #f ilist?))
   (pure))

 (imemv
   (lambda (obj (ilist? ilist)) (or #f ilist?))
   (pure))
 
 (idelete
   (lambda (obj (ilist? ilist)) ilist?)
   (pure))
 
 (idelete
   (lambda (obj (ilist? ilist) (procedure? =)) ilist?)
   (pure)
   ((= (lambda (obj1 obj2) *))))
 
 (idelete-duplicates
   (lambda ((ilist? ilist)) ilist?)
   (pure))
 
 (idelete-duplicates
   (lambda ((ilist? ilist) (procedure? =)) ilist?)
   (pure)
   ((= (lambda (obj1 obj2) *))))
 
 (iassoc
   (lambda (obj (ilist? ialist)) (or ilist? #f))
   (pure))

 (iassoc
   (lambda (obj (ilist? ialist) (procedure? =)) (or ipair? #f))
   (pure)
   ((= (lambda (a b) *))))

 (iassq
   (lambda (obj (ilist? ialist)) (or ipair? #f))
   (pure))

 (iassv
   (lambda (obj (ilist? ialist)) (or ipair? #f))
   (pure))
 
 (ialist-cons
   (lambda (key datum (ialist? ialist)) ialist?)
   (pure))
 
 (ialist-delete
   (lambda (key (ialist? ialist)) ialist?)
   (pure))
 
 (ialist-delete
   (lambda (key (ialist? ialist) (procedure? =)) ialist?)
   (pure)
   ((= (lambda (a b) *))))
 
 (replace-icar
   (lambda ((ipair? ipair) object) ipair?))
 
 (replace-icdr
   (lambda ((ipair? ipair) object) ipair?))
 
 (pair->ipair
   (lambda ((pair? pair)) ipair?)
   (pure))
 
 (ipair->pair
   (lambda ((ipair? ipair)) pair?)
   (pure))

 (list->ilist
   (lambda ((list? flist)) ilist?)
   (pure))

 (list->ilist
   (lambda ((dotted-list? flist)) dotted-ilist?)
   (pure))

 (ilist->list
   (lambda ((ilist? flist)) list?)
   (pure))

 (ilist->list
   (lambda ((dotted-ilist? flist)) dotted-list?)
   (pure))
 
 (tree->itree
   (lambda ((pair? pair)) ipair?)
   (pure))
 
 (tree->itree
   (lambda (object) *)
   (pure))
 
 (itree->tree
   (lambda ((ipair? ipair)) pair?)
   (pure))
 
 (itree->tree
   (lambda (object) *)
   (pure))
 
 (gtree->itree
   (lambda ((pair? pair)) ipair?)
   (pure))
 
 (gtree->itree
   (lambda (object) *)
   (pure))
 
 (gitree->tree
   (lambda ((ipair? ipair)) pair?)
   (pure))
 
 (gitree->tree
   (lambda (object) *)
   (pure))
 
 (iapply
   (lambda ((procedure? proc) arg1 ... (ilist? args)) *)
   (pure))
 
 (ipair-comparator
   (value comparator?))
 
 (ilist-comparator
   (value comparator?))
 
 (make-ilist-comparator
   (lambda ((comparator? element-comparator)) comparator?)
   (pure))
 
 (make-improper-ilist-comparator
   (lambda ((comparator? element-comparator)) comparator?)
   (pure))
 
 (make-ipair-comparator
   (lambda ((comparator? car-comparator) (comparator? cdr-comparator)) comparator?)
   (pure))
 
 (make-icar-comparator
   (lambda ((comparator? comparator)) comparator?)
   (pure))
 
 (make-icdr-comparator
   (lambda ((comparator? comparator)) comparator?)
   (pure))
 
 )
