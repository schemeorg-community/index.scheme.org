(((name . "comparator?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is a comparator, and #f otherwise."))
 ((name . "comparator-ordered?")
  (signature lambda ((comparator? comparator)) boolean?)
  (tags pure)
  (desc . "Returns #t if comparator has a supplied ordering predicate, and #f otherwise."))
 ((name . "comparatory-hashable?")
  (signature lambda ((comparator? comparator)) boolean?)
  (tags pure)
  (desc . "Returns #t if comparator has a supplied hash function, and #f otherwise."))
 ((name . "make-comparator")
  (signature
   lambda
   ((procedure? type-test)
    (procedure? equality)
    ((or procedure? #f) ordering)
    ((or procedure? #f) hash))
   comparator?)
  (subsigs
   (type-test (lambda (obj) boolean?))
   (equality (lambda (obj1 obj2) boolean?))
   (ordering (lambda (obj1 obj2) boolean?))
   (hash (lambda (obj) integer?)))
  (tags pure)
  (desc . "Returns a comparator which bundles the type-test, equality, ordering, and hash procedures provided. However, if ordering or hash is #f, a procedure is provided that signals an error on application. The predicates comparator-ordered? and/or comparator-hashable?, respectively, will return #f in these cases."))
 ((name . "make-pair-comparator")
  (signature
   lambda
   ((comparator? car-comparator) (comparator? cdr-comparator))
   comparator?)
  (tags pure)
  (desc . "This procedure returns comparators whose functions behave as follows.
1 The type test returns #t if its argument is a pair, if the car satisfies the type test predicate of car-comparator, and the cdr satisfies the type test predicate of cdr-comparator.
2 The equality function returns #t if the cars are equal according to car-comparator and the cdrs are equal according to cdr-comparator, and #f otherwise.
3 The ordering function first compares the cars of its pairs using the equality predicate of car-comparator. If they are not equal, then the ordering predicate of car-comparator is applied to the cars and its value is returned. Otherwise, the predicate compares the cdrs using the equality predicate of cdr-comparator. If they are not equal, then the ordering predicate of cdr-comparator is applied to the cdrs and its value is returned.
4 The hash function computes the hash values of the car and the cdr using the hash functions of car-comparator and cdr-comparator respectively and then hashes them together in an implementation-defined way."))
 ((name . "make-list-comparator")
  (signature
   lambda
   ((comparator? element-comparator)
    (procedure? type-test)
    (procedure? empty?)
    (procedure? head)
    (procedure? tail))
   comparator?)
  (subsigs
   (type-test (lambda (obj) boolean?))
   (empty? (lambda (obj) boolean?))
   (head (lambda (obj) *))
   (tail (lambda (obj) *)))
  (tags pure)
  (desc . "This procedure returns comparators whose functions behave as follows:
1 The type test returns #t if its argument satisfies type-test and the elements satisfy the type test predicate of element-comparator.
2 The total order defined by the equality and ordering functions is as follows (known as lexicographic order):
2.1 The empty sequence, as determined by calling empty?, compares equal to itself.
2.2 The empty sequence compares less than any non-empty sequence.
2.3 Two non-empty sequences are compared by calling the head procedure on each. If the heads are not equal when compared using element-comparator, the result is the result of that comparison. Otherwise, the results of calling the tail procedure are compared recursively.
3 The hash function computes the hash values of the elements using the hash function of element-comparator and then hashes them together in an implementation-defined way."))
 ((name . "make-vector-comparator")
  (signature
   lambda
   ((comparator? element-comparator)
    (procedure? type-test)
    (procedure? length)
    (procedure? ref))
   comparator?)
  (subsigs
   (type-test (lambda (obj) boolean?))
   (length (lambda (obj) integer?))
   (ref (lambda (vec (integer? index)) *)))
  (tags pure)
  (desc . "This procedure returns comparators whose functions behave as follows:
1 The type test returns #t if its argument satisfies type-test and the elements satisfy the type test predicate of element-comparator.
2 The equality predicate returns #t if both of the following tests are satisfied in order: the lengths of the vectors are the same in the sense of =, and the elements of the vectors are the same in the sense of the equality predicate of element-comparator.
3 The ordering predicate returns #t if the results of applying length to the first vector is less than the result of applying length to the second vector. If the lengths are equal, then the elements are examined pairwise using the ordering predicate of element-comparator. If any pair of elements returns #t, then that is the result of the list comparator's ordering predicate; otherwise the result is #f
4 The hash function computes the hash values of the elements using the hash function of element-comparator and then hashes them together in an implementation-defined way."))
 ((name . "make-eq-comparator")
  (signature lambda () comparator?)
  (tags pure)
  (desc . "make-eq-comparator, make-eqv-comparator, make-equal-comparator These procedures return comparators whose functions behave as follows:
1 The type test returns #t in all cases.
2 The equality functions are eq?, eqv?, and equal? respectively.
3 The ordering function is implementation-defined, except that it must conform to the rules for ordering functions. It may signal an error instead.
4 The hash function is default-hash."))
 ((name . "make-eqv-comparator")
  (signature lambda () comparator?)
  (tags pure)
  (desc . "make-eq-comparator, make-eqv-comparator, make-equal-comparator These procedures return comparators whose functions behave as follows:
1 The type test returns #t in all cases.
2 The equality functions are eq?, eqv?, and equal? respectively.
3 The ordering function is implementation-defined, except that it must conform to the rules for ordering functions. It may signal an error instead.
4 The hash function is default-hash."))
 ((name . "make-equal-comparator")
  (signature lambda () comparator?)
  (tags pure)
  (desc . "make-eq-comparator, make-eqv-comparator, make-equal-comparator These procedures return comparators whose functions behave as follows:
1 The type test returns #t in all cases.
2 The equality functions are eq?, eqv?, and equal? respectively.
3 The ordering function is implementation-defined, except that it must conform to the rules for ordering functions. It may signal an error instead.
4 The hash function is default-hash."))
 ((name . "boolean-hash") (signature lambda (obj) integer?) (tags pure)
  (desc . "boolean-hash, char-hash, char-ci-hash string-hash, string-ci-hash, symbol-hash, number-hash These are hash functions for some standard Scheme types, suitable for passing to make-comparator. Users may write their own hash functions with the same signature. However, if programmers wish their hash functions to be backward compatible with the reference implementation of SRFI 69, they are advised to write their hash functions to accept a second argument and ignore it. These are suitable hash functions for the specified types. The hash functions char-ci-hash and string-ci-hash treat their argument case-insensitively. Note that while symbol-hash may return the hashed value of applying symbol->string and then string-hash to the symbol, this is not a requirement."))
 ((name . "char-hash") (signature lambda (obj) integer?) (tags pure)
  (desc . "boolean-hash, char-hash, char-ci-hash string-hash, string-ci-hash, symbol-hash, number-hash These are hash functions for some standard Scheme types, suitable for passing to make-comparator. Users may write their own hash functions with the same signature. However, if programmers wish their hash functions to be backward compatible with the reference implementation of SRFI 69, they are advised to write their hash functions to accept a second argument and ignore it. These are suitable hash functions for the specified types. The hash functions char-ci-hash and string-ci-hash treat their argument case-insensitively. Note that while symbol-hash may return the hashed value of applying symbol->string and then string-hash to the symbol, this is not a requirement."))
 ((name . "char-ci-hash") (signature lambda (obj) integer?) (tags pure)
  (desc . "boolean-hash, char-hash, char-ci-hash string-hash, string-ci-hash, symbol-hash, number-hash These are hash functions for some standard Scheme types, suitable for passing to make-comparator. Users may write their own hash functions with the same signature. However, if programmers wish their hash functions to be backward compatible with the reference implementation of SRFI 69, they are advised to write their hash functions to accept a second argument and ignore it. These are suitable hash functions for the specified types. The hash functions char-ci-hash and string-ci-hash treat their argument case-insensitively. Note that while symbol-hash may return the hashed value of applying symbol->string and then string-hash to the symbol, this is not a requirement."))
 ((name . "string-hash") (signature lambda (obj) integer?) (tags pure)
  (desc . "boolean-hash, char-hash, char-ci-hash string-hash, string-ci-hash, symbol-hash, number-hash These are hash functions for some standard Scheme types, suitable for passing to make-comparator. Users may write their own hash functions with the same signature. However, if programmers wish their hash functions to be backward compatible with the reference implementation of SRFI 69, they are advised to write their hash functions to accept a second argument and ignore it. These are suitable hash functions for the specified types. The hash functions char-ci-hash and string-ci-hash treat their argument case-insensitively. Note that while symbol-hash may return the hashed value of applying symbol->string and then string-hash to the symbol, this is not a requirement."))
 ((name . "string-ci-hash") (signature lambda (obj) integer?) (tags pure)
  (desc . "boolean-hash, char-hash, char-ci-hash string-hash, string-ci-hash, symbol-hash, number-hash These are hash functions for some standard Scheme types, suitable for passing to make-comparator. Users may write their own hash functions with the same signature. However, if programmers wish their hash functions to be backward compatible with the reference implementation of SRFI 69, they are advised to write their hash functions to accept a second argument and ignore it. These are suitable hash functions for the specified types. The hash functions char-ci-hash and string-ci-hash treat their argument case-insensitively. Note that while symbol-hash may return the hashed value of applying symbol->string and then string-hash to the symbol, this is not a requirement."))
 ((name . "symbol-hash") (signature lambda (obj) integer?) (tags pure)
  (desc . "boolean-hash, char-hash, char-ci-hash string-hash, string-ci-hash, symbol-hash, number-hash These are hash functions for some standard Scheme types, suitable for passing to make-comparator. Users may write their own hash functions with the same signature. However, if programmers wish their hash functions to be backward compatible with the reference implementation of SRFI 69, they are advised to write their hash functions to accept a second argument and ignore it. These are suitable hash functions for the specified types. The hash functions char-ci-hash and string-ci-hash treat their argument case-insensitively. Note that while symbol-hash may return the hashed value of applying symbol->string and then string-hash to the symbol, this is not a requirement."))
 ((name . "number-hash") (signature lambda (obj) integer?) (tags pure)
  (desc . "boolean-hash, char-hash, char-ci-hash string-hash, string-ci-hash, symbol-hash, number-hash These are hash functions for some standard Scheme types, suitable for passing to make-comparator. Users may write their own hash functions with the same signature. However, if programmers wish their hash functions to be backward compatible with the reference implementation of SRFI 69, they are advised to write their hash functions to accept a second argument and ignore it. These are suitable hash functions for the specified types. The hash functions char-ci-hash and string-ci-hash treat their argument case-insensitively. Note that while symbol-hash may return the hashed value of applying symbol->string and then string-hash to the symbol, this is not a requirement."))
 ((name . "hash-bound") (signature syntax-rules () ((_) integer?)) (tags)
  (desc . "Hash functions should be written so as to return a number between 0 and the largest reasonable number of elements (such as hash buckets) a data structure in the implementation might have. What that value is depends on the implementation. This value provides the current bound as a positive exact integer, typically for use by user-written hash functions. However, they are not required to bound their results in this way."))
 ((name . "hash-salt") (signature syntax-rules () ((_) integer?)) (tags)
  (desc . "A salt is random data in the form of a non-negative exact integer used as an additional input to a hash function in order to defend against dictionary attacks, or (when used in hash tables) against denial-of-service attacks that overcrowd certain hash buckets, increasing the amortized O(1) lookup time to O(n). Salt can also be used to specify which of a family of hash functions should be used for purposes such as cuckoo hashing. This macro provides the current value of the salt, typically for use by user-written hash functions. However, they are not required to make use of the current salt.
The initial value is implementation-dependent, but must be less than the value of (hash-bound), and should be distinct for distinct runs of a program unless otherwise specified by the implementation. Implementations may provide a means to specify the salt value to be used by a particular invocation of a hash function."))
 ((name . "make-default-comparator") (signature lambda () comparator?)
  (desc . "Returns a comparator known as a default comparator that accepts Scheme values and orders them in some implementation-defined way, subject to the following conditions:
1 Given disjoint types a and b, one of three conditions must hold:
1.1 All objects of type a compare less than all objects of type b.
1.2 All objects of type a compare greater than all objects of type b.
1.3 All objects of both type a and type b compare equal to each other. This is not permitted for any of the Scheme types mentioned below.
2 The empty list must be ordered before all pairs.
3 When comparing booleans, it must use the total order #f < #t.
4 When comparing characters, it must use char=? and char<?. Note: In R5RS, this is an implementation-dependent order that is typically the same as Unicode codepoint order; in R6RS and R7RS, it is Unicode codepoint order.
5 When comparing pairs, it must behave the same as a comparator returned by make-pair-comparator with default comparators as arguments.
6 When comparing symbols, it must use an implementation-dependent total order. One possibility is to use the order obtained by applying symbol->string to the symbols and comparing them using the total order implied by string<?.
7 When comparing bytevectors, it must behave the same as a comparator created by the expression (make-vector-comparator (make-comparator bytevector? = < number-hash) bytevector? bytevector-length bytevector-u8-ref).
8 When comparing numbers where either number is complex, since non-real numbers cannot be compared with <, the following least-surprising ordering is defined: If the real parts are < or >, so are the numbers; otherwise, the numbers are ordered by their imaginary parts. This can still produce somewhat surprising results if one real part is exact and the other is inexact.
9 When comparing real numbers, it must use = and <.
10 When comparing strings, it must use string=? and string<?. Note: In R5RS, this is lexicographic order on the implementation-dependent order defined by char<?; in R6RS it is lexicographic order on Unicode codepoint order; in R7RS it is an implementation-defined order.
11 When comparing vectors, it must behave the same as a comparator returned by (make-vector-comparator (make-default-comparator) vector? vector-length vector-ref).
12 When comparing members of types registered with comparator-register-default!, it must behave in the same way as the comparator registered using that function.

Default comparators use default-hash as their hash function."))
 ((name . "default-hash") (signature lambda (obj) integer?)
  (desc . "This is the hash function used by default comparators, which accepts a Scheme value and hashes it in some implementation-defined way, subject to the following conditions:
1 When applied to a pair, it must return the result of hashing together the values returned by default-hash when applied to the car and the cdr.
2 When applied to a boolean, character, string, symbol, or number, it must return the same result as boolean-hash, char-hash, string-hash, symbol-hash, or number-hash respectively.
3 When applied to a list or vector, it must return the result of hashing together the values returned by default-hash when applied to each of the elements."))
 ((name . "comparator-register-default!")
  (signature lambda ((comparator? comparator)) undefined)
  (desc . " Registers comparator for use by default comparators, such that if the objects being compared both satisfy the type test predicate of comparator, it will be employed by default comparators to compare them. Returns an unspecified value. It is an error if any value satisfies both the type test predicate of comparator and any of the following type test predicates: boolean?, char?, null?, pair?, symbol?, bytevector?, number?, string?, vector?, or the type test predicate of a comparator that has already been registered.
This procedure is intended only to extend default comparators into territory that would otherwise be undefined, not to override their existing behavior. In general, the ordering of calls to comparator-register-default! should be irrelevant. However, implementations that support inheritance of record types may wish to ensure that default comparators always check subtypes before supertypes.
This SRFI recommends (but does not require) that libraries which expose comparators do not register them with this procedure, because the default comparator (which is meant mostly for ad hoc programming) is meant to be under the control of the program author rather than the library author. It is the program author's responsibility to ensure that the registered comparators do not conflict with each other."))
 ((name . "comparator-type-test-predicate")
  (signature lambda ((comparator? comparator)) procedure?)
  (subsigs (return (lambda (obj) boolean?)))
  (tags pure))
 ((name . "comparator-equality-predicate")
  (signature lambda ((comparator? comparator)) procedure?)
  (subsigs (return (lambda (obj1 obj2) boolean?)))
  (tags pure))
 ((name . "comparator-ordering-predicate")
  (signature lambda ((comparator? comparator)) (or #f procedure?))
  (subsigs (return (lambda (obj1 obj2) boolean?)))
  (tags pure))
 ((name . "comparator-hash-function")
  (signature lambda ((comparator? comparator)) (or #f procedure?))
  (subsigs (return (lambda (obj) integer?)))
  (tags pure))
 ((name . "comparator-test-type")
  (signature lambda ((comparator? comparator) obj) boolean?)
  (tags pure)
  (desc . "Invokes the type test predicate of comparator on obj and returns what it returns. More convenient than comparator-type-test-predicate, but less efficient when the predicate is called repeatedly."))
 ((name . "comparator-check-type")
  (signature lambda ((comparator? comparator) obj) boolean?)
  (desc . " Invokes the type test predicate of comparator on obj and returns true if it returns true, but signals an error otherwise. More convenient than comparator-type-test-predicate, but less efficient when the predicate is called repeatedly."))
 ((name . "comparator-hash")
  (signature lambda ((comparator? comparator) obj) integer?)
  (tags pure)
  (desc . " Invokes the hash function of comparator on obj and returns what it returns. More convenient than comparator-hash-function, but less efficient when the function is called repeatedly. Note: No invokers are required for the equality and ordering predicates, because =? and <? serve this function. "))
 ((name . "=?")
  (signature
   lambda
   ((comparator? comparator) object1 object2 object3 ...)
   boolean?)
  (tags pure)
  (desc . "=?, <?, >?, <=?, >=? These procedures are analogous to the number, character, and string comparison predicates of Scheme. They allow the convenient use of comparators to handle variable data types.
These procedures apply the equality and ordering predicates of comparator to the objects as follows. If the specified relation returns #t for all objecti and objectj where n is the number of objects and 1 <= i < j <= n, then the procedures return #t, but otherwise #f. Because the relations are transitive, it suffices to compare each object with its successor. The order in which the values are compared is unspecified."))
 ((name . "<?")
  (signature
   lambda
   ((comparator? comparator) object1 object2 object3 ...)
   boolean?)
  (tags pure)
  (desc . "=?, <?, >?, <=?, >=? These procedures are analogous to the number, character, and string comparison predicates of Scheme. They allow the convenient use of comparators to handle variable data types.
These procedures apply the equality and ordering predicates of comparator to the objects as follows. If the specified relation returns #t for all objecti and objectj where n is the number of objects and 1 <= i < j <= n, then the procedures return #t, but otherwise #f. Because the relations are transitive, it suffices to compare each object with its successor. The order in which the values are compared is unspecified."))
 ((name . ">?")
  (signature
   lambda
   ((comparator? comparator) object1 object2 object3 ...)
   boolean?)
  (tags pure)
  (desc . "=?, <?, >?, <=?, >=? These procedures are analogous to the number, character, and string comparison predicates of Scheme. They allow the convenient use of comparators to handle variable data types.
These procedures apply the equality and ordering predicates of comparator to the objects as follows. If the specified relation returns #t for all objecti and objectj where n is the number of objects and 1 <= i < j <= n, then the procedures return #t, but otherwise #f. Because the relations are transitive, it suffices to compare each object with its successor. The order in which the values are compared is unspecified."))
 ((name . "<=?")
  (signature
   lambda
   ((comparator? comparator) object1 object2 object3 ...)
   boolean?)
  (tags pure)
  (desc . "=?, <?, >?, <=?, >=? These procedures are analogous to the number, character, and string comparison predicates of Scheme. They allow the convenient use of comparators to handle variable data types.
These procedures apply the equality and ordering predicates of comparator to the objects as follows. If the specified relation returns #t for all objecti and objectj where n is the number of objects and 1 <= i < j <= n, then the procedures return #t, but otherwise #f. Because the relations are transitive, it suffices to compare each object with its successor. The order in which the values are compared is unspecified."))
 ((name . ">=?")
  (signature
   lambda
   ((comparator? comparator) object1 object2 object3 ...)
   boolean?)
  (tags pure)
  (desc . "=?, <?, >?, <=?, >=? These procedures are analogous to the number, character, and string comparison predicates of Scheme. They allow the convenient use of comparators to handle variable data types.
These procedures apply the equality and ordering predicates of comparator to the objects as follows. If the specified relation returns #t for all objecti and objectj where n is the number of objects and 1 <= i < j <= n, then the procedures return #t, but otherwise #f. Because the relations are transitive, it suffices to compare each object with its successor. The order in which the values are compared is unspecified."))
 ((name . "comparator-if<=>")
  (signature
   syntax-rules
   ()
   ((_ comparator object1 object2 less-than equal-to greater-than))
   ((_ object1 object2 less-than equal-to greater-than)))
  (subsigs (comparator (value comparator?)))
  (desc . "It is an error unless <comparator> evaluates to a comparator and <object1> and <object2> evaluate to objects that the comparator can handle. If the ordering predicate returns true when applied to the values of <object1> and <object2> in that order, then <less-than> is evaluated and its value returned. If the equality predicate returns true when applied in the same way, then <equal-to> is evaluated and its value returned. If neither returns true, <greater-than> is evaluated and its value returned.
If <comparator> is omitted, a default comparator is used.")))
