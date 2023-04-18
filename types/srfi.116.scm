(((name . "iq")
  (signature syntax-rules () ((_ datum ...) ilist?))
  (desc . "iq is partly analogous to quote, taking an arbitrary number of literals and constructing an ilist from them, with any pairs in the literals converted to ipairs. It is useful for providing constant ipair-based objects. Note that pairs within literal vectors or other implementation-dependent literals will not be converted. Unfortunately, there is no ilist analogue of ', so we save keystrokes by using iq rather than iquote and omitting the top-level parentheses."))
 ((name . "ipair")
  (signature lambda (a d) ipair?)
  (tags pure)
  (desc . "The primitive constructor. Returns a newly allocated ipair whose icar is a and whose icdr is d. The ipair is guaranteed to be different (in the sense of eqv?) from every existing object."))
 ((name . "ilist") 
  (signature lambda (object ...) ilist?)
  (tags pure)
  (desc . "Returns a newly allocated ilist of its arguments."))
 ((name . "xipair") 
  (signature lambda (d a) ipair?)
  (tags pure)
  (desc . "(lambda (d a) (ipair a d))
Of utility only as a value to be conveniently passed to higher-order procedures."))
 ((name . "ipair*") 
  (signature lambda (elt1 elt2 ...) *)
  (tags pure)
  (desc . "Like ilist, but the last argument provides the tail of the constructed ilist"))
 ((name . "make-ilist")
  (signature case-lambda (((integer? n)) ilist?) (((integer? n) fill) ilist?))
  (tags pure)
  (desc . "Returns an n-element ilist, whose elements are all the value fill. If the fill argument is not given, the elements of the ilist may be arbitrary values."))
 ((name . "ilist-tabulate")
  (signature lambda ((integer? n) (procedure? init-proc)) ilist?)
  (subsigs (init-proc (lambda ((integer? i)) *)))
  (tags pure)
  (desc . "Returns an n-element ilist. Element i of the ilist, where 0 <= i < n, is produced by (init-proc i). No guarantee is made about the dynamic order in which init-proc is applied to these indices."))
 ((name . "ilist-copy")
  (signature
   case-lambda
   (((ilist? dilist)) ilist?)
   (((dotted-ilist? dilist)) dotted-ilist?))
  (tags pure)
  (desc . "Copies the spine of the argument, including the ilist tail."))
 ((name . "iiota")
  (signature
   case-lambda
   (((integer? count)) ilist?)
   (((integer? count) (number? start)) ilist?)
   (((integer? count) (number? start) (number? step)) ilist?))
  (tags pure)
  (desc . "Returns an ilist containing the elements
(start start+step ... start+(count-1)*step)

The start and step parameters default to 0 and 1, respectively. This procedure takes its name from the APL primitive."))
 ((group
    ((name . "proper-ilist?")
     (signature lambda (obj) boolean?)
     (tags pure predicate))
    ((name . "ilist?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "These identifiers are bound either to the same procedure, or to procedures of equivalent behavior. In either case, true is returned iff x is a proper ilist — a ()-terminated ilist.
More carefully: The empty list is a proper ilist. An ipair whose icdr is a proper ilist is also a proper ilist. Everything else is a dotted ilist. This includes non-ipair, non-() values (e.g. symbols, numbers, mutable pairs), which are considered to be dotted ilists of length 0."))
 ((name . "dotted-ilist?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "True if x is a finite, non-nil-terminated ilist. That is, there exists an n >= 0 such that icdrn(x) is neither an ipair nor (). This includes non-ipair, non-() values (e.g. symbols, numbers), which are considered to be dotted ilists of length 0."))
 ((name . "ipair?") 
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if object is an ipair; otherwise, #f."))
 ((name . "null-ilist?")
  (signature lambda ((ilist? lst)) boolean?)
  (tags pure predicate)
  (desc . "Ilist is a proper ilist. This procedure returns true if the argument is the empty list (), and false otherwise. It is an error to pass this procedure a value which is not a proper ilist. This procedure is recommended as the termination condition for ilist-processing procedures that are not defined on dotted ilists."))
 ((name . "not-ipair?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "(lambda (x) (not (ipair? x)))
Provided as a procedure as it can be useful as the termination condition for ilist-processing procedures that wish to handle all ilists, both proper and dotted."))
 ((name . "ilist=")
  (signature lambda ((procedure? elt=) (ilist? ilist1) ...) boolean?)
  (subsigs (elt= (lambda (a b) *)))
  (tags pure)
  (desc . "Determines ilist equality, given an element-equality procedure. Proper ilist A equals proper ilist B if they are of the same length, and their corresponding elements are equal, as determined by elt=. If the element-comparison procedure's first argument is from ilisti, then its second argument is from ilisti+1, i.e. it is always called as (elt= a b) for a an element of ilist A, and b an element of ilist B.
In the n-ary case, every ilisti is compared to ilisti+1 (as opposed, for example, to comparing ilist1 to ilisti, for i>1). If there are no ilist arguments at all, ilist= simply returns true.
It is an error to apply ilist= to anything except proper ilists. It cannot reasonably be extended to dotted ilists, as it provides no way to specify an equality procedure for comparing the ilist terminators.
Note that the dynamic order in which the elt= procedure is applied to pairs of elements is not specified. For example, if ilist= is applied to three ilists, A, B, and C, it may first completely compare A to B, then compare B to C, or it may compare the first elements of A and B, then the first elements of B and C, then the second elements of A and B, and so forth.
The equality procedure must be consistent with eq?. That is, it must be the case that
(eq? x y) => (elt= x y).
Note that this implies that two ilists which are eq? are always ilist=, as well; implementations may exploit this fact to \"short-cut\" the element-by-element comparisons."))
 ((group
    ((name . "icar")
     (signature lambda ((ipair? ipair)) *)
     (tags pure))
    ((name . "icdr")
     (signature lambda ((ipair? ipair)) *)
     (tags pure)))
  (desc . "These procedures return the contents of the icar and icdr field of their argument, respectively. Note that it is an error to apply them to the empty ilist."))
 ((group
    ((name . "icaar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icadr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icdar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icddr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icaaaar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icaaadr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icaaar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icaadar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icaaddr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icaadr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icadaar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icadadr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icadar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icaddar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icadddr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icaddr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icdaaar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icdaadr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icdaar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icdadar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icdaddr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icdadr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icddaar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icddadr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icddar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icdddar") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icddddr") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "icdddr") (signature lambda ((ipair? ipair)) *) (tags pure)))
  (desc . "These procedures are compositions of icar and icdr, where for example icaddr could be defined by
(define icaddr (lambda (x) (icar (icdr (icdr x))))).

Arbitrary compositions, up to four deep, are provided. There are twenty-eight of these procedures in all."))
 ((name . "ilist-ref")
  (signature lambda ((ilist? ilist) (integer? i)) *)
  (tags pure)
  (desc . "Returns the ith element of ilist. (This is the same as the icar of (idrop ilist i).) It is an error if i >= n, where n is the length of ilist."))
 ((group
    ((name . "ifirst") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "isecond") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "ithird") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "ifourth") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "ififth") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "isixth") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "iseventh") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "ieighth") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "ininth") (signature lambda ((ipair? ipair)) *) (tags pure))
    ((name . "itenth") (signature lambda ((ipair? ipair)) *) (tags pure)))
  (desc . "Synonyms for car, cadr, caddr, ..."))
 ((name . "icar+icdr")
  (signature lambda ((ipair? ipair)) (values * *))
  (tags pure)
  (desc . "The fundamental ipair deconstructor:
(lambda (p) (values (icar p) (icdr p)))

This can, of course, be implemented more efficiently by a compiler."))
 ((name . "itake")
  (signature
   case-lambda
   (((ilist? x) (integer? i)) ilist?)
   (((dotted-ilist? x) (integer? i)) ilist?))
  (tags pure)
  (desc . "itake returns the first i elements of ilist x."))
 ((group
    ((name . "idrop")
     (signature
       case-lambda
       (((ilist? x) (integer? i)) ilist?)
       (((dotted-ilist? x) (integer? i)) *))
     (tags pure))
    ((name . "ilist-tail")
     (signature
       case-lambda
       (((ilist? x) (integer? i)) ilist?)
       (((dotted-ilist? x) (integer? i)) *))
     (tags pure)))
  (desc . "idrop / ilist-tail returns all but the first i elements of ilist x."))
 ((name . "itake-right")
  (signature
   case-lambda
   (((ilist? dilist) (integer? i)) ilist?)
   (((dotted-ilist? dilist) (integer? i)) *))
  (tags pure)
  (desc . "itake-right returns the last i elements of dilist."))
 ((name . "idrop-right")
  (signature
   case-lambda
   (((ilist? dilist) (integer? i)) ilist?)
   (((dotted-ilist? dilist) (integer? i)) ilist?))
  (tags pure)
  (desc . "idrop-right returns all but the last i elements of dilist."))
 ((name . "isplit-at")
  (signature
   case-lambda
   (((ilist? x) (integer? i)) (values ilist? ilist?))
   (((dotted-ilist? x) (integer? i)) (values ilist? *)))
  (tags pure)
  (desc . "isplit-at splits the ilist x at index i, returning an ilist of the first i elements, and the remaining tail. It is equivalent to
(values (itake x i) (idrop x i))"))
 ((name . "ilast") 
  (signature lambda ((ilist? ipair)) *)
  (tags pure)
  (desc . "Returns the last element of the non-empty, possibly dotted, ilist ipair."))
 ((name . "last-ipair")
  (signature lambda ((ilist? ipair)) ipair?)
  (tags pure)
  (desc . "last-ipair returns the last ipair in the non-empty ilist pair."))
 ((name . "ilength") 
  (signature lambda ((ilist? ilist)) integer?)
  (tags pure)
  (desc . "Returns the length of its argument. It is an error to pass a value to ilength which is not a proper ilist (()-terminated).
The length of a proper ilist is a non-negative integer n such that icdr applied n times to the ilist produces the empty list."))
 ((name . "iappend")
  (signature
   case-lambda
   (((ilist? ilist) ...) ilist?)
   (((ilist? ilist) ... obj) *))
  (tags pure)
  (desc . "Returns an ilist consisting of the elements of ilist1 followed by the elements of the other ilist parameters.
The resulting ilist is always newly allocated, except that it shares structure with the final ilisti argument. This last argument may be any value at all; an improper ilist results if it is not a proper ilist. All other arguments must be proper ilists."))
 ((name . "iconcatenate")
  (signature lambda ((ilist? ilist-of-ilists)) *)
  (tags pure)
  (desc . "Appends the elements of its argument together. That is, iconcatenate returns
(iapply iappend ilist-of-ilists)

or, equivalently,
(ireduce-right iappend '() ilist-of-ilists)

Note that some Scheme implementations do not support passing more than a certain number (e.g., 64) of arguments to an n-ary procedure. In these implementations, the (iapply iappend ...) idiom would fail when applied to long lists, but iconcatenate would continue to function properly.
As with iappend, the last element of the input list may be any value at all."))
 ((name . "ireverse") 
  (signature lambda ((ilist? ilist)) ilist?)
  (tags pure)
  (desc . "Returns a newly allocated ilist consisting of the elements of ilist in reverse order."))
 ((name . "iappend-reverse")
  (signature
   case-lambda
   (((ilist? rev-head) (ilist? tail)) ilist?)
   (((ilist? rev-head) tail) *))
  (tags pure)
  (desc . "iappend-reverse returns (iappend (ireverse rev-head) tail). It is provided because it is a common operation — a common list-processing style calls for this exact operation to transfer values accumulated in reverse order onto the front of another ilist, and because the implementation is significantly more efficient than the simple composition it replaces. (But note that this pattern of iterative computation followed by a reverse can frequently be rewritten as a recursion, dispensing with the reverse and iappend-reverse steps, and shifting temporary, intermediate storage from the heap to the stack, which is typically a win for reasons of cache locality and eager storage reclamation.)"))
 ((name . "izip")
  (signature lambda ((ilist? ilist1) (ilist? ilist2) ...) ilist?)
  (tags pure)
  (desc . "(lambda ilists (iapply imap ilist ilists))
If izip is passed n ilists, it returns an ilist as long as the shortest of these ilists, each element of which is an n-element ilist comprised of the corresponding elements from the parameter ilists."))
 ((group
    ((name . "iunzip1") (signature lambda ((ilist? ilist)) ilist?) (tags pure))
    ((name . "iunzip2")
     (signature lambda ((ilist? ilist)) (values ilist? ilist?))
     (tags pure))
    ((name . "iunzip3")
     (signature lambda ((ilist? ilist)) (values ilist? ilist? ilist?))
     (tags pure))
    ((name . "iunzip4")
     (signature lambda ((ilist? ilist)) (values ilist? ilist? ilist? ilist?))
     (tags pure))
    ((name . "iunzip5")
     (signature
       lambda
       ((ilist? ilist))
       (values ilist? ilist? ilist? ilist? ilist?))
     (tags pure)))
  (desc . "iunzip1 takes an ilist of ilists, where every ilist must contain at least one element, and returns an ilist containing the initial element of each such ilist. That is, it returns (imap icar ilists). iunzip2 takes an ilist of ilists, where every ilist must contain at least two elements, and returns two values: an ilist of the first elements, and an ilist of the second elements. iunzip3 does the same for the first three elements of the ilists, and so forth."))
 ((name . "icount")
  (signature
   lambda
   ((procedure? pred) (ilist? ilist1) (ilist? ilist2) ...)
   integer?)
  (subsigs (pred (lambda (obj ...) *)))
  (tags pure)
  (desc . "pred is a procedure taking as many arguments as there are ilists and returning a single value. It is applied element-wise to the elements of the ilists, and a count is tallied of the number of elements that produce a true value. This count is returned. count is \"iterative\" in that it is guaranteed to apply pred to the ilist elements in a left-to-right order. The counting stops when the shortest ilist expires."))
 ((name . "ifold")
  (signature
   lambda
   ((procedure? kons) knil (ilist? ilist1) (ilist? ilist2) ...)
   *)
  (subsigs (kons (lambda (obj1 obj2 ... fold-state) *)))
  (tags pure)
  (desc . "The fundamental ilist iterator.
First, consider the single ilist-parameter case. If ilist1 = (e1 e2 ... en), then this procedure returns
(kons en ... (kons e2 (kons e1 knil)) ... )

If n ilist arguments are provided, then the kons function must take n+1 parameters: one element from each ilist, and the \"seed\" or fold state, which is initially knil. The fold operation terminates when the shortest ilist runs out of values"))
 ((name . "ifold-right")
  (signature
   lambda
   ((procedure? kons) knil (ilist? ilist1) (ilist? ilist2) ...)
   *)
  (subsigs (kons (lambda (obj1 obj2 ... fold-state) *)))
  (tags pure)
  (desc . "The fundamental ilist recursion operator.
First, consider the single ilist-parameter case. If ilist1 = (e1 e2 ... en), then this procedure returns
(kons e1 (kons e2 ... (kons en knil)))

If n ilist arguments are provided, then the kons procedure must take n+1 parameters: one element from each ilist, and the \"seed\" or fold state, which is initially knil. The fold operation terminates when the shortest ilist runs out of values"))
 ((name . "ipair-fold")
  (signature
   lambda
   ((procedure? kons) knil (ilist? ilist1) (ilist? ilist2) ...)
   *)
  (subsigs (kons (lambda ((ipair? ipair1) (ipair? ipair2) ... fold-state) *)))
  (tags pure)
  (desc . "Analogous to fold, but kons is applied to successive sub-ilists of the ilists, rather than successive elements — that is, kons is applied to the ipairs making up the lists."))
 ((name . "ipair-fold-right")
  (signature
   lambda
   ((procedure? kons) knil (ilist? ilist1) (ilist? ilist2) ...)
   *)
  (subsigs (kons (lambda ((ipair? ipair1) (ipair? ipair2) ... fold-state) *)))
  (tags pure)
  (desc . "Holds the same relationship with ifold-right that ipair-fold holds with ifold."))
 ((name . "ireduce")
  (signature lambda ((procedure? f) ridentity (ilist? ilist)) *)
  (subsigs (f (lambda (obj fold-state) *)))
  (tags pure)
  (desc . "ireduce is a variant of ifold.
ridentity should be a \"right identity\" of the procedure f - that is, for any value x acceptable to f,
(f x ridentity) = x

ireduce has the following definition:
If ilist = (), return ridentity;
Otherwise, return (ifold f (icar ilist) (icdr ilist)).
...in other words, we compute (ifold f ridentity ilist).

Note that ridentity is used only in the empty-list case. You typically use ireduce when applying f is expensive and you'd like to avoid the extra application incurred when ifold applies f to the head of ilist and the identity value, redundantly producing the same value passed in to f. For example, if f involves searching a file directory or performing a database query, this can be significant. In general, however, ifold is useful in many contexts where ireduce is not (consider the examples given in the ifold definition — only one of the five folds uses a function with a right identity. The other four may not be performed with ireduce)."))
 ((name . "ireduce-right")
  (signature lambda ((procedure? f) ridentity (ilist? ilist)) *)
  (subsigs (f (lambda (obj fold-state) *)))
  (tags pure)
  (desc . "ireduce-right is the fold-right variant of ireduce. It obeys the following definition:
(ireduce-right f ridentity '()) = ridentity
(ireduce-right f ridentity (iq e1)) = (f e1 ridentity) = e1
(ireduce-right f ridentity (iq e1 e2 ...)) =
    (f e1 (ireduce f ridentity (e2 ...)))

...in other words, we compute (ifold-right f ridentity ilist)."))
 ((name . "iunfold")
  (signature
   case-lambda
   (((procedure? p) (procedure? f) (procedure? g) seed) ilist?)
   (((procedure? p) (procedure? f) (procedure? g) seed (ilist? tail-gen)) *))
  (subsigs
   (p (lambda (seed) boolean?))
   (f (lambda (seed) *))
   (g (lambda (seed) *))
   (tail-gen (lambda () *)))
  (tags pure)
  (desc . "iunfold is best described by its basic recursion:
(iunfold p f g seed) =
    (if (p seed) (tail-gen seed)
        (ipair (f seed)
              (iunfold p f g (g seed))))

p: Determines when to stop unfolding. 
f: Maps each seed value to the corresponding ilist element. 
g: Maps each seed value to next seed value. 
seed: The \"state\" value for the unfold. 
tail-gen: Creates the tail of the ilist; defaults to (lambda (x) '()) 

In other words, we use g to generate a sequence of seed values
seed, g(seed), g^2(seed), g^3(seed), ...
These seed values are mapped to ilist elements by f, producing the elements of the result ilist in a left-to-right order. P says when to stop."))
 ((name . "iunfold-right")
  (signature
   case-lambda
   (((procedure? p) (procedure? f) (procedure? g) seed) ilist?)
   (((procedure? p) (procedure? f) (procedure? g) seed (ilist? tail-gen)) *))
  (subsigs
   (p (lambda (seed) boolean?))
   (f (lambda (seed) *))
   (g (lambda (seed) *))
   (tail-gen (lambda () *)))
  (tags pure)
  (desc . "iunfold-right constructs an ilist with the following loop:
(let lp ((seed seed) (lis tail))
  (if (p seed) lis
      (lp (g seed)
          (ipair (f seed) lis))))

p: Determines when to stop unfolding. 
f: Maps each seed value to the corresponding ilist element. 
g: Maps each seed value to next seed value. 
seed: The \"state\" value for the unfold. 
tail: ilist terminator; defaults to '(). 

In other words, we use g to generate a sequence of seed values
seed, g(seed), g2(seed), g3(seed), ...
These seed values are mapped to ilist elements by f, producing the elements of the result ilist in a right-to-left order. P says when to stop."))
 ((name . "imap")
  (signature
   lambda
   ((procedure? proc) (ilist? ilist1) (ilist? ilist2) ...)
   ilist?)
  (subsigs (proc (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "proc is a procedure taking as many arguments as there are ilist arguments and returning a single value. imap applies proc element-wise to the elements of the ilists and returns an ilist of the results, in order. The dynamic order in which proc is applied to the elements of the ilists is unspecified."))
 ((name . "ifor-each")
  (signature
   lambda
   ((procedure? proc) (ilist? ilist1) (ilist? ilist2) ...)
   undefined)
  (subsigs (proc (lambda (obj1 obj2 ...) undefined)))
  (desc . "The arguments to ifor-each are like the arguments to imap, but ifor-each calls proc for its side effects rather than for its values. Unlike imap, ifor-each is guaranteed to call proc on the elements of the ilists in order from the first element(s) to the last, and the value returned by ifor-each is unspecified."))
 ((name . "iappend-map")
  (signature
   lambda
   ((procedure? proc) (ilist? ilist1) (ilist? ilist2) ...)
   ilist?)
  (subsigs (proc (lambda (obj1 obj2 ...) ilist?)))
  (tags pure)
  (desc . "Equivalent to
(iapply iappend (imap f ilist1 ilist2 ...))
and
(iapply iappend (imap f ilist1 ilist2 ...))
Map f over the elements of the ilists, just as in the imap function. However, the results of the applications are appended together (using iappend) to make the final result.
The dynamic order in which the various applications of f are made is not specified."))
 ((name . "imap-in-order")
  (signature
   lambda
   ((procedure? proc) (ilist? ilist1) (ilist? ilist2) ...)
   ilist?)
  (subsigs (proc (lambda (obj1 obj2 ...) *)))
  (desc . "A variant of the imap procedure that guarantees to apply f across the elements of the ilisti arguments in a left-to-right order. This is useful for mapping procedures that both have side effects and return useful values."))
 ((name . "ipair-for-each")
  (signature
   lambda
   ((procedure? proc) (ilist? ilist1) (ilist? ilist2) ...)
   undefined)
  (subsigs (proc (lambda ((ipair? obj1) (ipair? obj2) ...) undefined)))
  (desc . "Like ifor-each, but f is applied to successive sub-ilists of the argument ilists. That is, f is applied to the cells of the ilists, rather than the ilists' elements. These applications occur in left-to-right order."))
 ((name . "ifilter-map")
  (signature
   lambda
   ((procedure? proc) (ilist? ilist1) (ilist? ilist2) ...)
   ilist?)
  (subsigs (proc (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Like imap, but only true values are saved."))
 ((name . "ifilter")
  (signature lambda ((procedure? pred) (ilist? ilist)) ilist?)
  (subsigs (pred (lambda (obj) *)))
  (tags pure)
  (desc . "Return all the elements of ilist that satisfy predicate pred. The ilist is not disordered — elements that appear in the result ilist occur in the same order as they occur in the argument ilist. The returned ilist may share a common tail with the argument ilist. The dynamic order in which the various applications of pred are made is not specified."))
 ((name . "ipartition")
  (signature lambda ((procedure? pred) (ilist? ilist)) (values ilist? ilist?))
  (subsigs (pred (lambda (obj) *)))
  (tags pure)
  (desc . "Partitions the elements of ilist with predicate pred, and returns two values: the ilist of in-elements and the ilist of out-elements. The ilist is not disordered — elements occur in the result ilists in the same order as they occur in the argument ilist. The dynamic order in which the various applications of pred are made is not specified. One of the returned ilists may share a common tail with the argument ilist."))
 ((name . "iremove")
  (signature lambda ((procedure? pred) (ilist? ilist)) ilist?)
  (subsigs (pred (lambda (obj) *)))
  (tags pure)
  (desc . "Returns ilist without the elements that satisfy predicate pred:
(lambda (pred ilist) (ifilter (lambda (x) (not (pred x))) ilist))
The ilist is not disordered — elements that appear in the result ilist occur in the same order as they occur in the argument ilist. The returned ilist may share a common tail with the argument ilist. The dynamic order in which the various applications of pred are made is not specified."))
 ((name . "ifind")
  (signature lambda ((procedure? pred) (ilist? ilist)) *)
  (subsigs (pred (lambda (obj) *)))
  (tags pure)
  (desc . "Return the first element of ilist that satisfies predicate pred; false if no element does.
(ifind even? (iq 3 1 4 1 5 9)) => 4
Note that ifind has an ambiguity in its lookup semantics — if ifind returns #f, you cannot tell (in general) if it found a #f element that satisfied pred, or if it did not find any element at all. In many situations, this ambiguity cannot arise — either the ilist being searched is known not to contain any #f elements, or the ilist is guaranteed to have an element satisfying pred. However, in cases where this ambiguity can arise, you should use ifind-tail instead of ifind — ifind-tail has no such ambiguity"))
 ((name . "ifind-tail")
  (signature lambda ((procedure? pred) (ilist? ilist)) (or ipair? #f))
  (subsigs (pred (lambda (obj) *)))
  (tags pure)
  (desc . "Return the first ipair of ilist whose icar satisfies pred. If no ipair does, return false.
ifind-tail can be viewed as a general-predicate variant of the imember function.
Ifind-tail is essentially idrop-while, where the sense of the predicate is inverted: Ifind-tail searches until it finds an element satisfying the predicate; idrop-while searches until it finds an element that doesn't satisfy the predicate."))
 ((name . "itake-while")
  (signature lambda ((procedure? pred) (ilist? ilist)) ilist?)
  (subsigs (pred (lambda (obj) *)))
  (tags pure)
  (desc . "Returns the longest initial prefix of ilist whose elements all satisfy the predicate pred."))
 ((name . "idrop-while")
  (signature lambda ((procedure? pred) (ilist? ilist)) ilist?)
  (subsigs (pred (lambda (obj) *)))
  (tags pure)
  (desc . "idrops the longest initial prefix of ilist whose elements all satisfy the predicate pred, and returns the rest of the ilist."))
 ((group
    ((name . "ispan")
     (signature lambda ((procedure? pred) (ilist? ilist)) (values ilist? ilist?))
     (subsigs (pred (lambda (obj) *)))
     (tags pure))
    ((name . "ibreak")
     (signature lambda ((procedure? pred) (ilist? ilist)) (values ilist? ilist?))
     (subsigs (pred (lambda (obj) *)))
     (tags pure)))
  (desc . "ispan splits the ilist into the longest initial prefix whose elements all satisfy pred, and the remaining tail. ibreak inverts the sense of the predicate: the tail commences with the first element of the input ilist that satisfies the predicate.
In other words: ispan finds the initial span of elements satisfying pred, and ibreak breaks the ilist at the first element satisfying pred. "))
 ((name . "iany")
  (signature lambda ((procedure? pred) (ilist? ilist1) (ilist? ilist2) ...) *)
  (subsigs (pred (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Applies the predicate across the ilists, returning true if the predicate returns true on any application.
If there are n ilist arguments ilist1 ... ilistn, then pred must be a procedure taking n arguments and returning a boolean result.
iany applies pred to the first elements of the ilisti parameters. If this application returns a true value, iany immediately returns that value. Otherwise, it iterates, applying pred to the second elements of the ilisti parameters, then the third, and so forth. The iteration stops when a true value is produced or one of the ilists runs out of values; in the latter case, iany returns #f. The application of pred to the last element of the ilists is a tail call.
Note the difference between ifind and iany — ifind returns the element that satisfied the predicate; iany returns the true value that the predicate produced.
Like ievery, iany's name does not end with a question mark — this is to indicate that it does not return a simple boolean (#t or #f), but a general value."))
 ((name . "ievery")
  (signature lambda ((procedure? pred) (ilist? ilist1) (ilist? ilist2) ...) *)
  (subsigs (pred (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Applies the predicate across the ilists, returning true if the predicate returns true on every application.
If there are n ilist arguments ilist1 ... ilistn, then pred must be a procedure taking n arguments and returning a boolean result.
ievery applies pred to the first elements of the ilisti parameters. If this application returns false, ievery immediately returns false. Otherwise, it iterates, applying pred to the second elements of the ilisti parameters, then the third, and so forth. The iteration stops when a false value is produced or one of the ilists runs out of values. In the latter case, ievery returns the true value produced by its final application of pred. The application of pred to the last element of the ilists is a tail call.
If one of the ilisti has no elements, ievery simply returns #t.
Like iany, ievery's name does not end with a question mark — this is to indicate that it does not return a simple boolean (#t or #f), but a general value."))
 ((name . "ilist-index")
  (signature
   lambda
   ((procedure? pred) (ilist? ilist1) (ilist? ilist2) ...)
   (or integer? #f))
  (subsigs (pred (lambda (obj1 obj2 ...) *)))
  (tags pure)
  (desc . "Return the index of the leftmost element that satisfies pred.
If there are n ilist arguments ilist1 ... ilistn, then pred must be a function taking n arguments and returning a boolean result.
ilist-index applies pred to the first elements of the ilisti parameters. If this application returns true, ilist-index immediately returns zero. Otherwise, it iterates, applying pred to the second elements of the ilisti parameters, then the third, and so forth. When it finds a tuple of ilist elements that cause pred to return true, it stops and returns the zero-based index of that position in the ilists.
The iteration stops when one of the ilists runs out of values; in this case, ilist-index returns #f."))
 ((group
    ((name . "imember")
     (signature
       case-lambda
       ((obj (ilist? ilist)) (or #f ilist?))
       ((obj (ilist? ilist) (procedure? =)) (or #f ilist?)))
     (subsigs (= (lambda (obj1 obj2) *)))
     (tags pure))
    ((name . "imemq")
     (signature lambda (obj (ilist? ilist)) (or #f ilist?))
     (tags pure))
    ((name . "imemv")
     (signature lambda (obj (ilist? ilist)) (or #f ilist?))
     (tags pure)))
  (desc . "These procedures return the first sub-ilist of ilist whose icar is x, where the sub-ilists of ilist are the non-empty ilists returned by (idrop ilist i) for i less than the length of ilist. If x does not occur in ilist, then #f is returned. imemq uses eq? to compare x with the elements of ilist, while imemv uses eqv?, and imember uses equal?.
 The comparison procedure is used to compare the elements ei of ilist to the key x in this way:
 (= x ei) ; ilist is (E1 ... En)
 That is, the first argument is always x, and the second argument is one of the ilist elements."))
 ((name . "idelete")
  (signature
   case-lambda
   ((obj (ilist? ilist)) ilist?)
   ((obj (ilist? ilist) (procedure? =)) ilist?))
  (subsigs (= (lambda (obj1 obj2) *)))
  (tags pure)
  (desc . "idelete uses the comparison procedure =, which defaults to equal?, to find all elements of ilist that are equal to x, and deletes them from ilist. The dynamic order in which the various applications of = are made is not specified.
The ilist is not disordered — elements that appear in the result ilist occur in the same order as they occur in the argument ilist. The result may share a common tail with the argument ilist.
The comparison procedure is used in this way: (= x ei). That is, x is always the first argument, and an ilist element is always the second argument. The comparison procedure will be used to compare each element of ilist exactly once; the order in which it is applied to the various ei is not specified."))
 ((name . "idelete-duplicates")
  (signature
   case-lambda
   (((ilist? ilist)) ilist?)
   (((ilist? ilist) (procedure? =)) ilist?))
  (subsigs (= (lambda (obj1 obj2) *)))
  (tags pure)
  (desc . "idelete-duplicates removes duplicate elements from the ilist argument. If there are multiple equal elements in the argument ilist, the result ilist only contains the first or leftmost of these elements in the result. The order of these surviving elements is the same as in the original ilist — idelete-duplicates does not disorder the ilist (hence it is useful for \"cleaning up\" immutable association lists).
The = parameter is used to compare the elements of the ilist; it defaults to equal?. If x comes before y in ilist, then the comparison is performed (= x y). The comparison procedure will be used to compare each pair of elements in ilist no more than once; the order in which it is applied to the various pairs is not specified.
Implementations of idelete-duplicates are allowed to share common tails between argument and result ilists — for example, if the ilist argument contains only unique elements, it may simply return exactly this ilist.
Be aware that, in general, idelete-duplicates runs in time O(n2) for n-element ilists. Uniquifying long ilists can be accomplished in O(n lg n) time by sorting the ilist to bring equal elements together, then using a linear-time algorithm to remove equal elements. Alternatively, one can use algorithms based on element-marking, with linear-time results."))
 ((group
    ((name . "iassoc")
     (signature
       case-lambda
       ((obj (ilist? ialist)) (or ilist? #f))
       ((obj (ilist? ialist) (procedure? =)) (or ipair? #f)))
     (subsigs (= (lambda (a b) *)))
     (tags pure))
    ((name . "iassq")
     (signature lambda (obj (ilist? ialist)) (or ipair? #f))
     (tags pure))
    ((name . "iassv")
     (signature lambda (obj (ilist? ialist)) (or ipair? #f))
     (tags pure)))
  (desc . "ialist must be an immutable association list — an ilist of ipairs. These procedures find the first ipair in ialist whose icar field is key, and returns that ipair. If no ipair in ialist has key as its icar, then #f is returned. iassq uses eq? to compare key with the icar fields of the ipairs in ialist, while iassv uses eqv? and iassoc uses equal?.
 The comparison procedure is used to compare the elements ei of ilist to the key parameter in this way:
(= key (icar ei)) ; ilist is (E1 ... En)
That is, the first argument is always key, and the second argument is one of the ilist elements."))
 ((name . "ialist-cons")
  (signature lambda (key datum (ilist? ialist)) ilist?)
  (tags pure)
  (desc . "(lambda (key datum ialist) (ipair (ipair key datum) ialist))
Construct a new ialist entry mapping key -> datum onto ialist."))
 ((name . "ialist-delete")
  (signature
   case-lambda
   ((key (ilist? ialist)) ilist?)
   ((key (ilist? ialist) (procedure? =)) ilist?))
  (subsigs (= (lambda (a b) *)))
  (tags pure)
  (desc . "ialist-delete deletes all associations from ialist with the given key, using key-comparison procedure =, which defaults to equal?. The dynamic order in which the various applications of = are made is not specified.
Return values may share common tails with the ialist argument. The ialist is not disordered — elements that appear in the result ialist occur in the same order as they occur in the argument ialist.
The comparison procedure is used to compare the element keys ki of ialist's entries to the key parameter in this way: (= key ki)"))
 ((name . "replace-icar") 
  (signature lambda ((ipair? ipair) object) ipair?)
  (desc . "This procedure returns an ipair with object in the icar field and the icdr of ipair in the icdr field."))
 ((name . "replace-icdr") 
  (signature lambda ((ipair? ipair) object) ipair?)
  (desc . "This procedure returns an ipair with object in the icdr field and the icar of ipair in the icar field."))
 ((group
    ((name . "pair->ipair") (signature lambda ((pair? pair)) ipair?) (tags pure))
    ((name . "ipair->pair") (signature lambda ((ipair? ipair)) pair?) (tags pure)))
  (desc . "These procedures, which are inverses, return an ipair and a pair respectively that have the same (i)car and (i)cdr fields as the argument."))
 ((group
    ((name . "list->ilist")
     (signature
       case-lambda
       (((list? flist)) ilist?)
       (((dotted-list? flist)) dotted-ilist?))
     (tags pure))
    ((name . "ilist->list")
     (signature
       case-lambda
       (((ilist? flist)) list?)
       (((dotted-ilist? flist)) dotted-list?))
     (tags pure)))
  (desc . "These procedures return an ilist and a list respectively that have the same elements as the argument. The tails of dotted (i)lists are preserved in the result, which makes the procedures not inverses when the tail of a dotted ilist is a list or vice versa. The empty list is converted to itself.
It is an error to apply list->ilist to a circular list."))
 ((group
    ((name . "tree->itree")
     (signature case-lambda (((pair? pair)) ipair?) ((object) *))
     (tags pure))
    ((name . "itree->tree")
     (signature case-lambda (((ipair? ipair)) pair?) ((object) *))
     (tags pure)))
  (desc . "These procedures walk a tree of pairs or ipairs respectively and make a deep copy of it, returning an isomorphic tree containing ipairs or pairs respectively. The result may share structure with the argument. If the argument is not of the expected type, it is returned.
These procedures are not inverses in the general case. For example, a pair of ipairs would be converted by tree->itree to an ipair of ipairs, which if converted by itree->tree would produce a pair of pairs."))
 ((group
    ((name . "gtree->itree")
     (signature case-lambda (((pair? pair)) ipair?) ((object) *))
     (tags pure))
    ((name . "gtree->tree")
     (signature case-lambda (((ipair? ipair)) pair?) ((object) *))
     (tags pure)))
  (desc . "These procedures walk a generalized tree consisting of pairs, ipairs, or a combination of both, and make a deep copy of it, returning an isomorphic tree containing only ipairs or pairs respectively. The result may share structure with the argument. If the argument is neither a pair nor an ipair, it is returned."))
 ((name . "iapply")
  (signature lambda ((procedure? proc) arg1 ... (ilist? args)) *)
  (tags pure)
  (desc . "The iapply procedure is an analogue of apply whose last argument is an ilist rather than a list. It is equivalent to (apply procedure object ... (ilist->list ilist)), but may be implemented more efficiently."))
 ((name . "ipair-comparator") 
  (signature value comparator?)
  (desc . "The ipair-comparator object is a SRFI-114 comparator suitable for comparing ipairs. Note that it is not a procedure. It compares pairs using default-comparator on their cars. If the cars are not equal, that value is returned. If they are equal, default-comparator is used on their cdrs and that value is returned."))
 ((name . "ilist-comparator") 
  (signature value comparator?)
  (desc . "The ilist-comparator object is a SRFI-114 comparator suitable for comparing ilists. Note that it is not a procedure. It compares ilists lexicographically, as follows:
* The empty ilist compares equal to itself.
* The empty ilist compares less than any non-empty ilist.
* Two non-empty ilists are compared by comparing their icars. If the icars are not equal when compared using default-comparator, then the result is the result of that comparison. Otherwise, the icdrs are compared using ilist-comparator."))
 ((name . "make-ilist-comparator")
  (signature lambda ((comparator? element-comparator)) comparator?)
  (tags pure)
  (desc . "The make-ilist-comparator procedure returns a comparator suitable for comparing ilists using element-comparator to compare the elements."))
 ((name . "make-improper-ilist-comparator")
  (signature lambda ((comparator? element-comparator)) comparator?)
  (tags pure)
  (desc . "The make-improper-ilist-comparator procedure returns a comparator that compares arbitrary objects as follows: the empty list precedes all ipairs, which precede all other objects. Ipairs are compared as if with (make-ipair-comparator comparator comparator). All other objects are compared using comparator."))
 ((name . "make-ipair-comparator")
  (signature
   lambda
   ((comparator? car-comparator) (comparator? cdr-comparator))
   comparator?)
  (tags pure)
  (desc . "Returns a comparator that compares ipairs first on their icars using icar-comparator. If the icars are equal, it compares the icdrs using icdr-comparator."))
 ((name . "make-icar-comparator")
  (signature lambda ((comparator? comparator)) comparator?)
  (tags pure)
  (desc . "The make-icar-comparator procedure returns a comparator that compares ipairs on their icars alone using comparator."))
 ((name . "make-icdr-comparator")
  (signature lambda ((comparator? comparator)) comparator?)
  (tags pure)
  (desc . "The make-icdr-comparator procedure returns a comparator that compares ipairs on their icdrs alone using comparator.")))
