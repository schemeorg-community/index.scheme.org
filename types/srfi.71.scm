(
 ((group
    ((name . "let")
     (signature
       syntax-rules
       (values)
       ((_ (binding-spec ...) body))
       ((_ name (binding-spec ...) body)))
     (subsigs
       (binding-spec
         (pattern
           (var1 var2 ... expression)
           ((values var ...) expression)
           ((values var ... . var-rest) expression)))))
    ((name . "let*")
     (signature syntax-rules (values) ((_ (binding-spec ...) body)))
     (subsigs
       (binding-spec
         (pattern
           (var1 var2 ... expression)
           ((values var ...) expression)
           ((values var ... . var-rest) expression)))))
    ((name . "letrec")
     (signature syntax-rules (values) ((_ (binding-spec ...) body)))
     (subsigs
       (binding-spec
         (pattern
           (var1 var2 ... expression)
           ((values var ...) expression)
           ((values var ... . var-rest) expression))))))
  (desc . "The syntax of Scheme (R5RS, Section 7.1.3.) is extended by replacing the existing production:
<binding spec> --> (<variable> <expression>)
by the three new productions
<binding spec> --> ((values <variable>*) <expression>)
<binding spec> --> ((values <variable>* . <variable>) <expression>)
<binding spec> --> (<variable>+ <expression>)

The form (<variable>+ <expression>) is just an abbreviation for ((values <variable>+) <expression>), and it includes the original <binding spec> of R5RS.
The first two forms are evaluated as follows: The variables are bound and the expression is evaluated according to the enclosing construct (either let, let*, or letrec.) However, the expression may deliver any number of values to its continuation, which stores these values into the variables specified, possibly allocating a rest list in case of the . <variable> form.
The number of values delivered by the expression must match the number of values expected by the binding specification. Otherwise an error is raised, as call-with-values would. This implies in particular, that each binding of a named let involves exactly one value, because this binding can also be an argument to a lambda-expression."))
 ((group
    ((name . "uncons") (signature lambda ((pair? pair)) (values * *)) (tags pure))
    ((name . "uncons-2")
     (signature lambda ((list? lst)) (values * * *))
     (tags pure))
    ((name . "uncons-3")
     (signature lambda ((list? lst)) (values * * * *))
     (tags pure))
    ((name . "uncons-4")
     (signature lambda ((list? lst)) (values * * * * *))
     (tags pure))
    ((name . "uncons-cons")
     (signature lambda ((list? alist)) (values * * *))
     (tags pure))
    ((name . "unlist")
     (signature lambda ((list? lst)) (values * ...))
     (tags pure))
    ((name . "unvector")
     (signature lambda ((vector? vec)) (values * ...))
     (tags pure)))
  (desc . "The following procedures, specified in terms of standard procedures, are added to the set of standard procedures:
(define (uncons pair)
  (values (car pair) (cdr pair)))

(define (uncons-2 list)
  (values (car list) (cadr list) (cddr list)))

(define (uncons-3 list)
  (values (car list) (cadr list) (caddr list) (cdddr list)))

(define (uncons-4 list)
  (values (car list) (cadr list) (caddr list) (cadddr list) (cddddr list)))

(define (uncons-cons alist)
  (values (caar alist) (cdar alist) (cdr alist)))

(define (unlist list)
  (apply values list))

(define (unvector vector)
  (apply values (vector->list vector)))

These procedures decompose the standard concrete data structures (pair, list, vector) and deliver the components as values. It is an error if the argument cannot be decomposed as expected. Note that the procedures are not necessarily implemented by the definition given above.
The preferred way of decomposing a list into the first two elements and the rest list is (let ((x1 x2 x3+ (uncons-2 x))) body), and similar for three or four elements and a rest. This is not equivalent to (let (((values x1 x2 . x3+) (unlist x))) body) because the latter binds x3+ to a newly allocated copy of (cddr x)."))
 ((group
    ((name . "values->list") (signature syntax-rules () ((_ expression) list?)))
    ((name . "values->vector")
     (signature syntax-rules () ((_ expression) vector?))))
  (desc . "These operation receive all values (if any) delivered by their argument expression and return a newly allocated list (vector, resp.) of these values. Note that values->list is not the same as list (the procedure returning the list of its arguments).")))
