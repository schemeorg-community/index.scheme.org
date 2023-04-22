(((group
    ((name . "is")
     (signature
       syntax-rules
       (_)
       ((is val1 predicate-or-comparator val2 ...) (or boolean? procedure?)))
     (subsigs (val (pattern _ obj)) (predicate-or-comparator (value procedure?))))
    ((name . "isnt")
     (signature
       syntax-rules
       (_)
       ((isnt val1 predicate-or-comparator val2 ...) (or boolean? procedure?)))
     (subsigs
       (val (pattern _ obj))
       (predicate-or-comparator (value procedure?)))))
  (desc . "Infix relations
This document proposes to augment Scheme with a new syntactic form, is, so that, for example, the expression (is John taller-than? Tom) is expanded to (taller-than? John Tom).
In addition to improved code readability, the introduction of the is form gives an occasion to provide some convenient special behaviour in some particular cases. While some Schemers may find the lack of regularity and predictability of the is form repulsive, we believe that it actually allows us to express some common operations more succinctly.

Short-hand lambda expressions
For example, we decided to treat the _ (underscore) symbol differently than other symbols. (is _ taller-than? John) is expanded to (lambda (_) (taller-than? _ John)), thereby making the functionality of the is form partially overlap with the cut special form defined in the SRFI 26 document.
We chose the underscore symbol, although the cut macro uses the <> symbol, because it has been used as a special non-bindable symbol in various pattern matchers for Scheme (as well as in the Prolog language). It has also traditionally been used to name values that are meant to be ignored, so we believe that our choice should not be in conflict with existing practices.
However, the _ symbol should not be bound to a new transformer, but instead it should be imported from (scheme base) and re-exported, so that it can be renamed by the users who prefer to stick with the <> symbol from SRFI-26.

Multiple instances of underscore
If more than one instance of the underscore symbol appears in the argument position of the is and isnt macros, each occurrence counts as a separate argument (increasing the arity of the resulting lambda accordingly). For example, (is _ < _) is equivalent to (lambda (_1 _2) (< _1 _2)).

Negation
In addition to the is form, this SRFI provides an implementation of the isnt form, which negates the behavior of is. Although we didn't find that form particularly useful, we are certain that it may find its use, and if it were absent from the language, Schemers would come up with their own implementations. As a matter of fact, in our experiments with parroting the English language, we initially used the isn't symbol, which failed to work on some implementations.
Handling fewer arguments

The is and isnt macros could technically be passed fewer than three arguments. In particular, we interpret (isnt x prime?) as (not (prime? x)), and (isnt _ prime?) as (lambda (_) (not (prime? _))). For consistency, we interpret the usages of the is macro similarly, although it may not seem particularly useful. It is illegal to use the is and isnt macros with fewer than two arguments, and such attempts should raise a syntax error.")))
