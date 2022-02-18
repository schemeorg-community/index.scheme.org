(
 
 (list-sorted?
   (lambda ((procedure? <) (list? lis)) boolean?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-sorted?
   (lambda ((procedure? <) (vector? v)) boolean?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-sorted?
   (lambda ((procedure? <) (vector? v) (integer? start)) boolean?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-sorted?
   (lambda ((procedure? <) (vector? v) (integer? start) (integer? end)) boolean?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (list-sort
   (lambda ((procedure? <) (list? lis)) list?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (list-stable-sort
   (lambda ((procedure? <) (list? lis)) list?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (list-sort!
   (lambda ((procedure? <) (list? lis)) list?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (list-stable-sort!
   (lambda ((procedure? <) (list? lis)) list?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-sort
   (lambda ((procedure? <) (vector? v)) boolean?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-sort
   (lambda ((procedure? <) (vector? v) (integer? start)) boolean?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-sort
   (lambda ((procedure? <) (vector? v) (integer? start) (integer? end)) boolean?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-stable-sort
   (lambda ((procedure? <) (vector? v)) boolean?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-stable-sort
   (lambda ((procedure? <) (vector? v) (integer? start)) boolean?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-stable-sort
   (lambda ((procedure? <) (vector? v) (integer? start) (integer? end)) boolean?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-sort!
   (lambda ((procedure? <) (vector? v)) boolean?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-sort!
   (lambda ((procedure? <) (vector? v) (integer? start)) boolean?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-sort!
   (lambda ((procedure? <) (vector? v) (integer? start) (integer? end)) boolean?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-stable-sort!
   (lambda ((procedure? <) (vector? v)) boolean?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-stable-sort!
   (lambda ((procedure? <) (vector? v) (integer? start)) boolean?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-stable-sort!
   (lambda ((procedure? <) (vector? v) (integer? start) (integer? end)) boolean?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (list-merge
   (lambda ((procedure? <) (list? lis1) (list? lis2)) list?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (list-merge!
   (lambda ((procedure? <) (list? lis1) (list? lis2)) list?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-merge
   (lambda ((procedure? <) (vector? v1) (vector? v2)) vector?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-merge
   (lambda ((procedure? <) (vector? v1) (vector? v2) (integer? start1)) vector?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-merge
   (lambda ((procedure? <) (vector? v1) (vector? v2) (integer? start1) (integer? end1)) vector?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-merge
   (lambda ((procedure? <) (vector? v1) (vector? v2) (integer? start1) (integer? end1) (integer? start2)) vector?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-merge
   (lambda ((procedure? <) (vector? v1) (vector? v2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) vector?)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-merge!
   (lambda ((procedure? <) (vector? v1) (vector? v2)) vector?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-merge!
   (lambda ((procedure? <) (vector? v1) (vector? v2) (integer? start1)) vector?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-merge!
   (lambda ((procedure? <) (vector? v1) (vector? v2) (integer? start1) (integer? end1)) vector?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-merge!
   (lambda ((procedure? <) (vector? v1) (vector? v2) (integer? start1) (integer? end1) (integer? start2)) vector?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-merge!
   (lambda ((procedure? <) (vector? v1) (vector? v2) (integer? start1) (integer? end1) (integer? start2) (integer? end2)) vector?)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (list-delete-neighbor-dups
   (lambda ((procedure? =) (list? lis)) list?)
   (pure)
   ((= (lambda (obj1 obj2) boolean?))))
 
 (list-delete-neighbor-dups!
   (lambda ((procedure? =) (list? lis)) list?)
   ()
   ((= (lambda (obj1 obj2) boolean?))))
 
 (vector-delete-neighbor-dups
   (lambda ((procedure? =) (vector? v)) vector?)
   (pure)
   ((= (lambda (obj1 obj2) boolean?))))
 
 (vector-delete-neighbor-dups
   (lambda ((procedure? =) (vector? v) (integer? start)) vector?)
   (pure)
   ((= (lambda (obj1 obj2) boolean?))))
 
 (vector-delete-neighbor-dups
   (lambda ((procedure? =) (vector? v) (integer? start) (integer? end)) vector?)
   (pure)
   ((= (lambda (obj1 obj2) boolean?))))
 
 (vector-delete-neighbor-dups!
   (lambda ((procedure? =) (vector? v)) vector?)
   ()
   ((= (lambda (obj1 obj2) boolean?))))
 
 (vector-delete-neighbor-dups!
   (lambda ((procedure? =) (vector? v) (integer? start)) vector?)
   ()
   ((= (lambda (obj1 obj2) boolean?))))
 
 (vector-delete-neighbor-dups!
   (lambda ((procedure? =) (vector? v) (integer? start) (integer? end)) vector?)
   ()
   ((= (lambda (obj1 obj2) boolean?))))
 
 (vector-find-median
   (lambda ((procedure? <) (vector? v) knil) *)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-find-median
   (lambda ((procedure? <) (vector? v) knil (procedure? mean)) *)
   (pure)
   ((< (lambda (obj1 obj2) boolean?))
    (mean (lambda (obj1 obj2) *))))
 
 (vector-find-median!
   (lambda ((procedure? <) (vector? v) knil) *)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-find-median!
   (lambda ((procedure? <) (vector? v) knil (procedure? mean)) *)
   ()
   ((< (lambda (obj1 obj2) boolean?))
    (mean (lambda (obj1 obj2) *))))
 
 (vector-select!
   (lambda ((procedure? <) (vector? v) (integer? k)) *)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-select!
   (lambda ((procedure? <) (vector? v) (integer? k) (integer? start)) *)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-select!
   (lambda ((procedure? <) (vector? v) (integer? k) (integer? start) (integer? end)) *)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-separate!
   (lambda ((procedure? <) (vector? v) (integer? k)) undefined)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-separate!
   (lambda ((procedure? <) (vector? v) (integer? k) (integer? start)) undefined)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 (vector-separate!
   (lambda ((procedure? <) (vector? v) (integer? k) (integer? start) (integer? end)) undefined)
   ()
   ((< (lambda (obj1 obj2) boolean?))))
 
 )
