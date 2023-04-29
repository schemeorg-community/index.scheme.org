(((name . "rest-values")
  (signature 
    case-lambda
    (((list? rest-list)) list?)
    ((caller (list? rest-list)) list?)
    (((list? rest-list) args-number-limit) list?)
    ((caller (list? rest-list) args-number-limit default ...) list?))
  (tags pure)
  (desc . "1. When the <args-number-limit> is + or a positive integer, each <default> should be a list that contains default value(s), or a pair whose car is a default value and cdr is a predicate procedure.
2. When the <args-number-limit> is - or a negative integer, each <default> is any scheme expression.
3. When the <args-number-limit> is a boolean, each <default> is the same as 1.

1. (first mode of operation)
Rest-values checks whether each element of the <rest-list> is a member of the corresponding <default> list, or satisfies the predicate procedure of the corresponding <default> pair, and then returns the checked element(s). If the element doesn't pass, rest-values signals an error. when there are no more elements in the <rest-list>, then rest-values additionally returns the car values of the remaining <default>s. On the other hand, when the number of elements of the <rest-list> are more than the number of the <default>s, the supernumerary elements are additionally returned if the <args-number-limit> is +, or its value is not less than the number of elements of the <rest-list>.

2. (second mode of operation)
This is the same as the first except that rest-values does not check each element of the <rest-list>, and it uses - instead of +, and an absolute value instead of a simple value as the value of <args-number-limit>.

3. (third mode of operation)
Rest-values checks whether any element of the <default> list is a member of the <rest-list>, or any element of the <rest-list> satisfies the predicate procedure of the <default> pair, and then returns the checked element. If the <default> doesn't pass, rest-values returns the car value of the <default>. when any elements of the <rest-list> are remained after the above processing, rest-values either signals an error if the <args-number-limit> is #t, or returns the remaining elements of the <rest-list> if the <args-number-limit> is #f."))
 ((group
    ((name . "arg-and")
     (signature syntax-rules ()
                ((_ variable expr ...) boolean?)
                ((_ caller variable expr ...) boolean?)))
    ((name . "arg-ands")
     (signature syntax-rules ()
                ((_ (variable expr ...) ...) boolean?)
                ((_ (caller variable expr ...) ...) boolean?)
                ((_ common-caller (variable expr ...) ...) boolean?)
                ((_ common-caller (caller variable expr ...) ...) boolean?)))
    ((name . "err-and")
     (signature syntax-rules ()
                ((_ caller expression ...))))
    ((name . "err-ands")
     (signature syntax-rules ()
                ((_ (caller expression ...) ...))))
    ((name . "arg-or")
     (signature syntax-rules ()
                ((_ variable expr ...) boolean?)
                ((_ caller variable expr ...) boolean?)))
    ((name . "arg-ors")
     (signature syntax-rules ()
                ((_ (variable expr ...) ...) boolean?)
                ((_ (caller variable expr ...) ...) boolean?)
                ((_ common-caller (variable expr ...) ...) boolean?)
                ((_ common-caller (caller variable expr ...) ...) boolean?)))
    ((name . "err-or")
     (signature syntax-rules ()
                ((_ caller expression ...))))
    ((name . "err-ors")
     (signature syntax-rules ()
                ((_ (caller expression ...) ...)))))
  (desc . "Each <variable> should be an argument of a procedure.
The <caller>, <expr>, and <expression> are any scheme expression, but the <expr> should contain the corresponding <variable>.
Arg-and, arg-ands, err-and, and err-ands are the same as \"and\" except that these signal an error in case \"and\" returns a false value.
Arg-or, arg-ors, err-or, and err-ors are the same as \"or\" except that these signal an error in case \"or\" returns a true value.")))
