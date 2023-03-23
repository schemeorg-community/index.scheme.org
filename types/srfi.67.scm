(((name . "boolean-compare")
  (signature lambda ((boolean? bool1) (boolean? bool2)) integer?)
  (tags pure)
  (desc . "Compares two booleans, ordered by #f < #t."))
 ((group
    ((name . "char-compare")
     (signature lambda ((char? char1) (char? char2)) integer?)
     (tags pure))
    ((name . "char-compare-ci")
     (signature lambda ((char? char1) (char? char2)) integer?)
     (tags pure)))
  (desc . "Compare characters as char<=? and char-ci<=? respectively. The suffix -ci means \"case insensitive.\""))
 ((group
    ((name . "string-compare")
     (signature lambda ((string? string1) (string? string2)) integer?)
     (tags pure))
    ((name . "string-compare-ci")
     (signature lambda ((string? string1) (string? string2)) integer?)
     (tags pure)))
  (desc . "Compare strings as string<= and string-ci<=?. The suffix -ci means \"case insensitive.\""))
 ((name . "symbol-compare")
  (signature lambda ((symbol? symbol1) (symbol? symbol2)) integer?)
  (tags pure)
  (desc . "Compares symbols as string<= on the names returned by symbol->string."))
 ((group
    ((name . "integer-compare")
     (signature lambda ((integer? integer1) (integer? integer2)) integer?)
     (tags pure))
    ((name . "rational-compare")
     (signature lambda ((rational? rational1) (rational? rational2)) integer?)
     (tags pure))
    ((name . "real-compare")
     (signature lambda ((real? real1) (real? real2)) integer?)
     (tags pure))
    ((name . "complex-compare")
     (signature lambda ((complex? complex1) (complex? complex2)) integer?)
     (tags pure))
    ((name . "number-compare")
     (signature lambda ((number? number1) (number? number2)) integer?)
     (tags pure)))
  (desc . "Compare two numbers. It is an error if an argument is not of the type specified by the name of the procedure.
        Complex numbers are ordered lexicographically on pairs (re, im). For objects representing real numbers sign(x - y) is computed. The ordering for values satisfying real? or complex? but not representing a real or complex number should be consistent with procedures = and < of R5RS , and apart from that it is unspecified.
        Numerical compare procedures are compatible with the R5RS numerical tower in the following sense: If S is a subtype of the numerical type T and x, y can be represented both in S and in T, then compare-S and compare-T compute the same result. "))
 ((group
    ((name . "vector-compare")
     (signature
       case-lambda
       (((vector? x) (vector? y)) integer?)
       (((procedure? compare) (vector? x) (vector? y)) integer?)
       ((x y (procedure? size) (procedure? ref)) integer?)
       (((procedure? compare) x y (procedure? size) (procedure? ref)) integer?))
     (subsigs
       (compare (lambda (el1 el2) integer?))
       (size (lambda (seq) integer?))
       (ref (lambda (seq index) *)))
     (tags pure))
    ((name . "vector-compare-as-list")
     (signature
       case-lambda
       (((vector? x) (vector? y)) integer?)
       (((procedure? compare) (vector? x) (vector? y)) integer?)
       ((x y (procedure? size) (procedure? ref)) integer?)
       (((procedure? compare) x y (procedure? size) (procedure? ref)) integer?))
     (subsigs
       (compare (lambda (el1 el2) integer?))
       (size (lambda (seq) integer?))
       (ref (lambda (seq index) *)))
     (tags pure))
    ((name . "list-compare")
     (signature
       case-lambda
       (((list? x) (list? y)) integer?)
       (((procedure? compare) (list? x) (list? y)) integer?)
       ((x y (procedure? empty?) (procedure? head) (procedure? tail)) integer?)
       (((procedure? compare)
         x
         y
         (procedure? empty?)
         (procedure? head)
         (procedure? tail))
        integer?))
     (subsigs
       (compare (lambda (el1 el2) integer?))
       (empty? (lambda (seq) boolean?))
       (head (lambda (seq) *))
       (tail (lambda (seq) *)))
     (tags pure))
    ((name . "list-compare-as-vector")
     (signature
       case-lambda
       (((list? x) (list? y)) integer?)
       (((procedure? compare) (list? x) (list? y)) integer?)
       ((x y (procedure? empty?) (procedure? head) (procedure? tail)) integer?)
       (((procedure? compare)
         x
         y
         (procedure? empty?)
         (procedure? head)
         (procedure? tail))
        integer?))
     (subsigs
       (compare (lambda (el1 el2) integer?))
       (empty? (lambda (seq) boolean?))
       (head (lambda (seq) *))
       (tail (lambda (seq) *)))
     (tags pure)))
  (desc . "Compare two sequences x and y, using compare for comparing elements. The result is an exact integer in { - 1, 0, 1}. If compare is not supplied, default-compare is used.
The procedure named access-compare-as-order accesses the objects like access and compares them with respect to the order given by order. The names type-compare are abbreviations for type-compare-as-type."))
 ((group
    ((name . "pair-compare-car")
     (signature lambda ((procedure? compare)) procedure?)
     (subsigs
       (compare (lambda (x y) integer?))
       (return (lambda ((pair? p1) (pair? p2)) integer?)))
     (tags pure))
    ((name . "pair-compare-cdr")
     (signature lambda ((procedure? compare)) procedure?)
     (subsigs
       (compare (lambda (x y) integer?))
       (return (lambda ((pair? p1) (pair? p2)) integer?)))
     (tags pure)))
  (desc . "Construct a compare procedure on pairs which only uses the car (only the cdr, respectively), and ignores the other."))
 ((name . "pair-compare")
  (signature
   case-lambda
   (((procedure? compare-car)
     (procedure? compare-cdr)
     (pair? pair1)
     (pair? pair2))
    integer?)
   ((obj1 obj2) integer?)
   (((procedure? compare) obj1 obj2) integer?))
  (subsigs (compare (lambda (x y) integer?)))
  (tags pure)
  (desc . " The 4-ary form compares two pairs pair1 pair2 by comparing their cars using compare-car, and if the cars are equal the cdrs are compared using compare-cdr.
The 3-ary form compares two objects by type using the ordering of types
null < pair < neither-null-nor-pair.

Two objects of type neither-null-nor-pair are compared using compare. Two pairs are compared by using compare on the cars, and if the cars are equal by recursing on the cdrs.
The 2-ary form uses default-compare for compare."))
 ((name . "default-compare")
  (signature lambda (obj1 obj2) integer?)
  (tags pure)
  (desc . "compares its arguments by type using the ordering
null < pair < boolean < char < string < symbol < number < vector < other

Two objects of the same type type are compared as type-compare would, if there is such a procedure. The type null consists of the empty list '(). The effect of comparing two other objects or of comparing cyclic structures (made from lists or vectors) is unspecified. (Implementations are encouraged to add comparisons for other built-in types, e.g. records, regexps, etc.) "))
 ((name . "refine-compare")
  (signature syntax-rules () ((_ compare-expression ...) integer?))
  (subsigs (compare-expression (value integer?)))
  (desc . "The arguments <compare-expression> ...are evaluated from left to right until a non-zero value is found (which then is the value) or until there are no more arguments to evaluate (in which case the value is 0). It is allowed that there are no arguments at all."))
 ((name . "select-compare")
  (signature
   syntax-rules
   (else)
   ((_ x y (type? c1 ...) ...) integer?)
   ((_ x y (type? c1 ...) ... (else c1 ...)) integer?))
  (subsigs (c (value integer?)) (type? (value procedure?)))
  (desc . "Select-compare is a conditional for defining hierarchical extensions and refinements of compare procedures (refer to Section 5). It compares the values of <x1> and <x2> by trying the type tests in order, and applies an implict refine-compare on the consequences upon a match.
In more detail, evaluation proceeds as follows: First <x1> and <x2> are evaluated in unspecified order, resulting in values x1 and x2, respectively. Then the clauses are evaluated one by one, from left to right.
For clause (<type?> <c1> ...), first <type?> is evaluated resulting in a predicate procedure type? and then the expressions (type? x1) and (type? x2) are evaluated and interpreted as booleans. If both booleans are true then the overall value is (refine-compare <c1> ...). If only the first is true the result is -1, if only the second is true the result is 1, and if neither is true the next clause is considered. An else clause is treated as if both tests where true. If there are no clauses left, the result is 0.
Select-compare evaluates <x1> and <x2> exactly once, even in the absence of any clauses. Moreover, each <type?> is evaluated at most once and the resulting procedure type? is called at most twice."))
 ((name . "cond-compare")
  (signature
   syntax-rules
   (else)
   ((_ x y ((t1 t2) c1 ...) ...) integer?)
   ((_ x y ((t1 t2) c1 ...) ... (else c1 ...)) integer?))
  (subsigs
   (t (value boolean?))
   (c (value integer?))
   (type? (value procedure?)))
  (desc . "Cond-compare is another conditional for defining hierarchical extensions and refinements of compare procedures (refer to Section 5).
Evaluation proceeds as follows: The clauses are evaluated one by one, from left to right. For clause ((<t1> <t2>) <c1> ...), first <t1> and <t2> are evaluated and the results are interpreted as boolean values. If both booleans are true then the overall value is (refine-compare <c1> ...). If only the first is true the result is -1, if only the second is true the result is 1, and if neither is true the next clause is considered. An else clause is treated as if both booleans where true. If there are no clauses left (or there are no clauses to begin with), the result is 0.
Cond-compare evaluates each expression at most once."))
 ((name . "if3")
  (signature syntax-rules () ((_ c less equal greater)))
  (subsigs (c (value integer?)))
  (desc . "If3 is the 3-way conditional for comparisons. First <c> is evaluated, resulting in value c. The value c must be an exact integer in { - 1, 0, 1}, otherwise an error is signalled. If c = - 1 then the value of the if3-expression is obtained by evaluating <less>. If c = 0 then <equal> is evaluated. If c = 1 then <greater> is evaluated."))
 ((group
    ((name . "if=?")
     (signature syntax-rules () ((_ c consequent)) ((_ c consequent alternate)))
     (subsigs (c (value integer?))))
    ((name . "if<?")
     (signature syntax-rules () ((_ c consequent)) ((_ c consequent alternate)))
     (subsigs (c (value integer?))))
    ((name . "if>?")
     (signature syntax-rules () ((_ c consequent)) ((_ c consequent alternate)))
     (subsigs (c (value integer?))))
    ((name . "if<=?")
     (signature syntax-rules () ((_ c consequent)) ((_ c consequent alternate)))
     (subsigs (c (value integer?))))
    ((name . "if>=?")
     (signature syntax-rules () ((_ c consequent)) ((_ c consequent alternate)))
     (subsigs (c (value integer?))))
    ((name . "if-not=?")
     (signature syntax-rules () ((_ c consequent)) ((_ c consequent alternate)))
     (subsigs (c (value integer?)))))
  (desc . "These six macros are 2-way conditionals for comparisons. First <c> is evaluated, resulting in value c. The value c must be an exact integer in { - 1, 0, 1}, otherwise an error is signalled. Then, depending on the value of c and the name of the macro, either <consequence> or <alternate> is evaluated, and the resulting value is the value of the conditional expression.
The branch is chosen according to the following table:
         <consequent> <alternate>
if=?     c = 0        c: { - 1, 1}
if<?     c = -1       c: {0, 1}
if>?     c = 1        c: { - 1, 0}
if<=?    c: { - 1, 0} c = 1
if>=?    c: {0, 1}    c = -1
if-not=? c: { - 1, 1} c = 0

Note:   The macros if<=? etc. are the preferred way of 2-way branching based on the result of a comparison."))
 ((group
    ((name . "=?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y) boolean?)
       (((procedure? compare) x y) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . "<?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y) boolean?)
       (((procedure? compare) x y) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . ">?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y) boolean?)
       (((procedure? compare) x y) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . "<=?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y) boolean?)
       (((procedure? compare) x y) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . ">=?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y) boolean?)
       (((procedure? compare) x y) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . "not=?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y) boolean?)
       (((procedure? compare) x y) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure)))
  (desc . "If the values x and y are given, test if x and y are in the relation specified by the name of the procedure rel?, with respect to compare procedure compare; otherwise construct a predicate procedure.
In the forms (rel? [ compare ] x y), the result is a boolean (either #t or #f) depending on (compare x y) and the test rel? as specified for if<? etc. If compare is not supplied, default-compare is used.
In the form (rel? [ compare ]), the predicate procedure (lambda (x y) (rel? compare x y)) is constructed. Again, if compare is not supplied, default-compare is used."))
 ((group
    ((name . "</<?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y z) boolean?)
       (((procedure? compare) x y z) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . "</<=?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y z) boolean?)
       (((procedure? compare) x y z) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . "<=/<?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y z) boolean?)
       (((procedure? compare) x y z) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . "<=/<=?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y z) boolean?)
       (((procedure? compare) x y z) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . ">/>?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y z) boolean?)
       (((procedure? compare) x y z) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . ">/>=?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y z) boolean?)
       (((procedure? compare) x y z) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . ">=/>?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y z) boolean?)
       (((procedure? compare) x y z) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . ">=/>=?")
     (signature
       case-lambda
       (() procedure?)
       (((procedure? compare)) procedure?)
       ((x y z) boolean?)
       (((procedure? compare) x y z) boolean?))
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure)))
  (desc . "Test if x, y, and z form a chain with the two relations specified by the name of the procedure rel1/rel2?, with respect to the compare procedure compare.
If compare is not provided, default-compare is used. If x y z are not provided, a predicate procedure of three arguments is constructed. The order in which the values are compared is unspecified, but each value is compared at least once."))
 ((group
    ((name . "chain=?")
     (signature lambda ((procedure? compare) x1 ...) boolean?)
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . "chain<?")
     (signature lambda ((procedure? compare) x1 ...) boolean?)
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . "chain>?")
     (signature lambda ((procedure? compare) x1 ...) boolean?)
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . "chain<=?")
     (signature lambda ((procedure? compare) x1 ...) boolean?)
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . "chain>=?")
     (signature lambda ((procedure? compare) x1 ...) boolean?)
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure)))
  (desc . "Test if the values x1 ...(zero or more values) form a chain with respect to the relation specified by the name of the procedure, and with respect to the compare procedure compare. The result is a boolean (either #t or #f.) The order in which the values are compared is unspecified, but each value is compared at least once (even if there is just one.)
A sequence of values x1, ..., xn forms a chain with respect to the relation rel? if (rel? compare xi xj) for all 1 <= i < j <= n. In particular, this is the case for n: {0,1}.
Since the relations = , <, >, <=, and >= are transitive, it is sufficient to test (rel? compare xi xi+1) for 1 < i < n."))
 ((name . "pairwise-not=?")
  (signature lambda ((procedure? compare) x1 ...) boolean?)
  (subsigs (compare (lambda (x y) integer?)))
  (tags pure)
  (desc . "Tests if the values x1 ...(zero or more values) are pairwise unequal with respect to the compare procedure compare. The result is a boolean (either #t or #f). The order in which the values are compared is unspecified, but each value is compared at least once (even if there is just one).
The values x1, ..., xn are pairwise unequal if (not=? compare xi xj) for all i != j. In particular, this is the case for n: {0,1}.
Since compare defines a total ordering on the values, the property can be checked in time O(n log n), and implementations are required to do this. (For example by first sorting and then comparing adjacent elements)."))
 ((group
    ((name . "min-compare")
     (signature lambda ((procedure? compare) x1 x2 ...) *)
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure))
    ((name . "max-compare")
     (signature lambda ((procedure? compare) x1 x2 ...) *)
     (subsigs (compare (lambda (x y) integer?)))
     (tags pure)))
  (desc . "A minimum or maximum of the values x1 x2 ...(one or more values) with respect to the compare procedure compare.
The result is the first value that is minimal (maximal, respectively). The order in which the values are compared is unspecified, but each value is compared at least once (even if there is just one value). "))
 ((name . "kth-largest")
  (signature lambda ((procedure? compare) (integer? k) x1 x2 ...) *)
  (subsigs (compare (lambda (x y) integer?)))
  (tags pure)
  (desc . "The k-th largest element of values x0 x1 ...(one or more values) with respect to the compare procedure compare.
More precisely, (kth-largest compare k x0 ... xn-1) returns the (modulo k n)-th element of the unique sequence obtained by stably sorting (x0 ··· xn-1). (Recall that a sorting algorithm is stable if it does not permute items with equal key, i.e. equivalent w.r.t. compare).
The argument k is an exact integer, and n >= 1. The order in which the values xi are compared is unspecified, but each value is compared at least once (even if there is just one value)."))
 ((group
    ((name . "compare-by<")
     (signature
       case-lambda
       (((procedure? lt-pred)) procedure?)
       (((procedure? lt-pred) x y) integer?))
     (subsigs (lt-pred (lambda (x y) boolean?)))
     (tags pure))
    ((name . "compare-by>")
     (signature
       case-lambda
       (((procedure? gt-pred)) procedure?)
       (((procedure? gt-pred) x y) integer?))
     (subsigs (gt-pred (lambda (x y) boolean?)))
     (tags pure))
    ((name . "compare-by<=")
     (signature
       case-lambda
       (((procedure? le-pred)) procedure?)
       (((procedure? le-pred) x y) integer?))
     (subsigs (le-pred (lambda (x y) boolean?)))
     (tags pure))
    ((name . "compare-by>=")
     (signature
       case-lambda
       (((procedure? ge-pred)) procedure?)
       (((procedure? ge-pred) x y) integer?))
     (subsigs (ge-pred (lambda (x y) boolean?)))
     (tags pure))
    ((name . "compare-by=/<")
     (signature
       case-lambda
       (((procedure? eq-pred) (procedure? lt-pred)) procedure?)
       (((procedure? eq-pred) (procedure? lt-pred) x y) integer?))
     (subsigs (eq-pred (lambda (x y) boolean?)) (lt-pred (lambda (x y) boolean?)))
     (tags pure))
    ((name . "compare-by=/>")
     (signature lambda ((procedure? eq-pred) (procedure? gt-pred)) procedure?)
     (subsigs
       (eq-pred (lambda (x y) boolean?))
       (gt-pred (lambda (x y) boolean?))
       (return (lambda (x y) integer?)))
     (tags pure))
    ((name . "compare-by=/<")
     (signature lambda ((procedure? eq-pred) (procedure? gt-pred) x y) integer?)
     (subsigs (eq-pred (lambda (x y) boolean?)) (gt-pred (lambda (x y) boolean?)))
     (tags pure)))
  (desc . "If optional arguments x and y are present then these are compared with respect to the total order defined by the predicate(s) given; the result is in { -1, 0, 1 }. If x and y are not present then a procedure comparing its two arguments using the predicate(s) given is constructed and returned.
The predicate procedures mean the following: (lt-pred x y) tests if x < y, le-pred tests for <=, gt-pred for >, ge-pred for >=, and eq-pred tests if x and y are equivalent. The result returned by a predicate procedure is interpreted as a Scheme truth value (i.e. #f is false and non-#f is true).
The purpose of the procedures compare-bypredicate(s) is to define a compare procedure from an order predicate, and possibly an additional equivalence predicate. If an equivalence predicate eq-pred is given, it is called before the order predicate because the equivalence may be coarser than the total ordering, and it may also be cheaper."))
 ((name . "debug-compare")
  (signature lambda ((procedure? compare)) procedure?)
  (subsigs
   (compare (lambda (x y) integer?))
   (return (lambda (x y) integer?)))
  (desc . "Constructs a compare procedure equivalent to compare but with debugging code wrapped around the calls to compare. The debugging code signals an error if it detects a violation of the axioms of a compare function. For this it is assumed that compare has no side-effects.
More specifically, (debug-compare compare) evaluates to a compare procedure compare1 which checks reflexivity, antisymmetry, and transitivity of compare based on the arguments on which compare1 is called:
The procedure compare1 checks reflexivity on any value passed to compare, antisymmetry on any pair of values on which compare is called, and transitivity on triples where two of the arguments are from the current call to compare1 and the third is a pseudo-random selection from the two arguments of the previous call to compare1.")))
