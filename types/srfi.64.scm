(((name . "test-assert")
  (signature syntax-rules () ((_ expression)) ((_ test-name expression)))
  (subsigs (test-name (value string?)))
  (desc . "This evaluates the expression. The test passes if the result is true; if the result is false, a test failure is reported. The test also fails if an exception is raised, assuming the implementation has a way to catch exceptions. How the failure is reported depends on the test runner environment. The test-name is a string that names the test case. (Though the test-name is a string literal in the examples, it is an expression. It is evaluated only once.) It is used when reporting errors, and also when skipping tests, as described below. It is an error to invoke test-assert if there is no current test runner."))
 ((name . "test-eqv")
  (signature
   syntax-rules
   ()
   ((_ expected test-expr))
   ((_ test-name expected test-expr)))
  (subsigs (test-name (value string?)))
  (desc . "This is equivalent to:
(test-assert [test-name] (eqv? expected test-expr))"))
 ((name . "test-equal")
  (signature
   syntax-rules
   ()
   ((_ expected test-expr))
   ((_ test-name expected test-expr)))
  (subsigs (test-name (value string?)))
  (desc . "This is equivalent to:
(test-assert [test-name] (equal? expected test-expr))"))
 ((name . "test-eq")
  (signature
   syntax-rules
   ()
   ((_ expected test-expr))
   ((_ test-name expected test-expr)))
  (subsigs (test-name (value string?)))
  (desc . "This is equivalent to:
(test-assert [test-name] (eq? expected test-expr))"))
 ((name . "test-approximate")
  (signature
   syntax-rules
   ()
   ((_ expected test-expr error))
   ((_ test-name expected test-expr error)))
  (subsigs (test-name (value string?)))
  (desc . "This is equivalent to (except that each argument is only evaluated once):
(test-assert [test-name]
  (and (>= test-expr (- expected error))
       (<= test-expr (+ expected error))))"))
 ((name . "test-error")
  (signature
   syntax-rules
   ()
   ((_ test-expr))
   ((_ error-type test-expr))
   ((_ test-name error-type test-expr)))
  (subsigs (test-name (value string?)))
  (desc . "Evaluating test-expr is expected to signal an error. The kind of error is indicated by error-type. If the error-type is left out, or it is #t, it means \"some kind of unspecified error should be signaled\". This specification leaves it implementation-defined (or for a future specification) what form test-error may take, though all implementations must allow #t. Some implementations may support SRFI-35's conditions, but these are only standardized for SRFI-36's I/O conditions, which are seldom useful in test suites."))
 ((name . "test-read-eval-string")
  (signature syntax-rules () ((_ string)))
  (subsigs (string (value string?)))
  (desc . "This function parses string (using read) and evaluates the result. The result of evaluation is returned from test-read-eval-string. An error is signalled if there are unread characters after the read is done."))
 ((name . "test-begin")
  (signature syntax-rules () ((_ suite-name)) ((_ suite-name count)))
  (desc . "A test-begin enters a new test group. The suite-name becomes the current test group name, and is added to the end of the test group path. Portable test suites should use a sting literal for suite-name; the effect of expressions or other kinds of literals is unspecified."))
 ((name . "test-end") 
  (signature syntax-rules () ((_)) ((_ suite-name)))
  (desc . "A test-end leaves the current test group. An error is reported if the suite-name does not match the current test group name.
Additionally, if the matching test-begin installed a new test-runner, then the test-end will de-install it, after reporting the accumulated test results in an implementation-defined manner."))
 ((name . "test-group")
  (signature syntax-rules () ((_ suite-name decl-or-expr ...)))
  (desc . "Equivalent to:
(if (not (test-to-skip% suite-name))
  (dynamic-wind
    (lambda () (test-begin suite-name))
    (lambda () decl-or-expr ...)
    (lambda () (test-end suite-name))))

This is usually equivalent to executing the decl-or-exprs within the named test group. However, the entire group is skipped if it matched an active test-skip (see later). Also, the test-end is executed in case of an exception."))
 ((name . "test-group-with-cleanup")
  (signature syntax-rules () ((_ suite-name decl-or-expr ... cleanup-form)))
  (desc . "Execute each of the decl-or-expr forms in order (as in a <body>), and then execute the cleanup-form. The latter should be executed even if one of a decl-or-expr forms raises an exception (assuming the implementation has a way to catch exceptions)."))
 ((name . "test-match-name")
  (signature lambda ((string? name)) procedure?)
  (subsigs (return (lambda ((test-runner? runner)) boolean?)))
  (desc . "The resulting specifier matches if the current test name (as returned by test-runner-test-name) is equals? to name."))
 ((name . "test-match-nth")
  (signature
   case-lambda
   (((integer? n)) procedure?)
   (((integer? n) (integer? count)) procedure?))
  (subsigs (return (lambda ((test-runner? runner)) boolean?)))
  (desc . "This evaluates to a stateful predicate: A counter keeps track of how many times it has been called. The predicate matches the n'th time it is called (where 1 is the first time), and the next (- count 1) times, where count defaults to 1."))
 ((name . "test-match-any")
  (signature lambda ((procedure? specifier) ...) procedure?)
  (subsigs
   (specifier (lambda ((test-runner? runner)) boolean?))
   (return (lambda ((test-runner? runner)) boolean?)))
  (desc . "The resulting specifier matches if any specifier matches. Each specifier is applied, in order, so side-effects from a later specifier happen even if an earlier specifier is true."))
 ((name . "test-match-all")
  (signature lambda ((procedure? specifier) ...) procedure?)
  (subsigs
   (specifier (lambda ((test-runner? runner)) boolean?))
   (return (lambda ((test-runner? runner)) boolean?)))
  (desc . "The resulting specifier matches if each specifier matches. Each specifier is applied, in order, so side-effects from a later specifier happen even if an earlier specifier is false.
 count (i.e. an integer)
Convenience short-hand for: (test-match-nth 1 count).

name (i.e. a string)
Convenience short-hand for (test-match-name name)."))
 ((name . "test-skip")
  (signature lambda (((or procedure? integer? string?) specifier)) undefined)
  (subsigs (specifier (lambda ((test-runner? runner)) boolean?)))
  (desc . "Evaluating test-skip adds the resulting specifier to the set of currently active skip-specifiers. Before each test (or test-group) the set of active skip-specifiers are applied to the active test-runner. If any specifier matches, then the test is skipped.
For convenience, if the specifier is a string that is syntactic sugar for (test-match-name specifier)."))
 ((name . "test-expect-fail")
  (signature lambda (((or procedure? integer? string?) specifier)) undefined)
  (subsigs (specifier (lambda ((test-runner? runner)) boolean?)))
  (desc . "Matching tests (where matching is defined as in test-skip) are expected to fail. This only affects test reporting, not test execution."))
 ((name . "test-runner?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "True iff value is a test-runner object."))
 ((name . "test-runner-current")
  (signature
   case-lambda
   (() test-runner?)
   (((test-runner? runner)) undefined))
  (desc . "Get or set the current test-runner. If an implementation supports parameter objects (as in SRFI-39), then test-runner-current can be a parameter object. Alternatively, test-runner-current may be implemented as a macro or function that uses a fluid or thread-local variable, or a plain global variable."))
 ((name . "test-runner-get") (signature lambda () test-runner?) (desc . "Same as (test-runner-current), buth throws an exception if there is no current test-runner."))
 ((name . "test-runner-simple") (signature lambda () test-runner?) (desc . "Creates a new simple test-runner, that prints errors and a summary on the standard output port."))
 ((name . "test-runner-null")
  (signature lambda () test-runner?)
  (desc . "Creates a new test-runner, that does nothing with the test results. This is mainly meant for extending when writing a custom runner.
Implementations may provide other test-runners, perhaps a (test-runner-gui)."))
 ((name . "test-runner-create") (signature lambda () test-runner?) (desc . "Create a new test-runner. Equivalent to ((test-runner-factory))"))
 ((name . "test-runner-factory")
  (signature case-lambda (() procedure?) (((procedure? factory)) undefined))
  (subsigs (factory (lambda () test-runner?)))
  (desc . "Get or set the current test-runner factory. A factory is a zero-argument function that creates a new test-runner. The default value is test-runner-simple, but implementations may provide a way to override the default. As with test-runner-current, this may be a parameter object, or use a per-thread, fluid, or global variable."))
 ((name . "test-apply")
  (signature
   lambda
   (((or integer? string? procedure?) specifier) ... procedure)
   undefined)
  (subsigs
   (procedure (lambda () *))
   (specifier (lambda ((test-runner? runner)) boolean?)))
  (desc . "Calls procedure with no arguments using the specified runner as the current test-runner. If runner is omitted, then (test-runner-current) is used. (If there is no current runner, one is created as in test-begin.) If one or more specifiers are listed then only tests matching the specifiers are executed. A specifier has the same form as one used for test-skip. A test is executed if it matches any of the specifiers in the test-apply and does not match any active test-skip specifiers."))
 ((name . "test-with-runner")
  (signature syntax-rules () ((_ runner decl-or-expr ...)))
  (subsigs (runner (value test-runner?)))
  (desc . "Executes each decl-or-expr in order in a context where the current test-runner is runner."))
 ((name . "test-result-kind")
  (signature
   case-lambda
   (() (or #f symbol?))
   (((test-runner? runner)) (or #f symbol?)))
  (desc . "Return one of the following result codes from the most recent tests:
'pass The test passed, as expected.
'fail The test failed (and was not expected to).
'xfail The test failed and was expected to.
'xpass The test passed, but was expected to fail.
'skip The test was skipped. 

Returns #f if no tests have been run yet. If we've started on a new test, but don't have a result yet, then the result kind is 'xfail is the test is expected to fail, 'skip is the test is supposed to be skipped, or #f otherwise."))
 ((name . "test-passed?")
  (signature case-lambda (() boolean?) (((test-runner? runner)) boolean?))
  (desc . "True if the value of (test-result-kind [runner]) is one of 'pass or 'xpass. This is a convenient shorthand that might be useful in a test suite to only run certain tests if the previous test passed."))
 ((name . "test-result-ref")
  (signature
   case-lambda
   (((test-runner? runner) (symbol? pname)) *)
   (((test-runner? runner) (symbol? pname) default) *))
  (desc . "Returns the property value associated with the pname property name. If there is no value associated with 'pname return default, or #f if default isn't specified."))
 ((name . "test-result-set!")
  (signature lambda ((test-runner? runner) (symbol? pname) value) undefined)
  (desc . "Sets the property value associated with the pname property name to value. Usually implementation code should call this function, but it may be useful for a custom test-runner to add extra properties."))
 ((name . "test-result-remove")
  (signature lambda ((test-runner? runner) (symbol? pname)) undefined)
  (desc . "Remove the property with the name 'pname."))
 ((name . "test-result-clear")
  (signature lambda ((test-runner? runner)) undefined)
  (desc . "Remove all result properties. The implementation automatically calls test-result-clear at the start of a test-assert and similar procedures."))
 ((name . "test-result-alist")
  (signature lambda ((test-runner? runner)) list?)
  (desc . "Returns an association list of the current result properties. It is unspecified if the result shares state with the test-runner. The result should not be modified, on the other hand the result may be implicitly modified by future test-result-set! or test-result-remove calls. However, a test-result-clear does not modify the returned alist. Thus you can \"archive\" result objects from previous runs."))
 ((group
    ((name . "test-runner-on-test-begin")
     (signature lambda ((test-runner? runner)) procedure?)
     (subsigs (return (lambda ((test-runner? runner)) undefined))))
    ((name . "test-runner-on-test-begin!")
     (signature
       lambda
       ((test-runner? runner) (procedure? on-test-begin))
       undefined)
     (subsigs (on-test-begin (lambda ((test-runner? runner)) undefined)))))
  (desc . "The on-test-begin-function is called at the start of an individual testcase, before the test expression (and expected value) are evaluated. "))
 ((group
    ((name . "test-runner-on-test-end")
     (signature lambda ((test-runner? runner)) procedure?)
     (subsigs (return (lambda ((test-runner? runner)) undefined))))
    ((name . "test-runner-on-test-end!")
     (signature lambda ((test-runner? runner) (procedure? on-test-end)) undefined)
     (subsigs (on-test-end (lambda ((test-runner? runner)) undefined)))))
  (desc . "The on-test-end-function is called at the end of an individual testcase, when the result of the test is available."))
 ((group
    ((name . "test-runner-on-group-begin")
     (signature lambda ((test-runner? runner)) procedure?)
     (subsigs
       (return
         (lambda ((test-runner? runner) (string? suite-name) (integer? count))
           undefined))))
    ((name . "test-runner-on-group-begin!")
     (signature
       lambda
       ((test-runner? runner) (procedure? on-group-begin))
       undefined)
     (subsigs
       (on-group-begin
         (lambda ((test-runner? runner) (string? suite-name) (integer? count))
           undefined)))))
  (desc . "The on-group-begin-function is called by a test-begin, including at the start of a test-group. The suite-name is a Scheme string, and count is an integer or #f."))
 ((group
    ((name . "test-runner-on-group-end")
     (signature lambda ((test-runner? runner)) procedure?)
     (subsigs (return (lambda ((test-runner? runner)) undefined))))
    ((name . "test-runner-on-group-end!")
     (signature
       lambda
       ((test-runner? runner) (procedure? on-group-end))
       undefined)
     (subsigs (on-group-end (lambda ((test-runner? runner)) undefined)))))
  (desc . "The on-group-end-function is called by a test-end, including at the end of a test-group."))
 ((group
    ((name . "test-runner-on-bad-count")
     (signature lambda ((test-runner? runner)) procedure?)
     (subsigs
       (return
         (lambda ((test-runner? runner)
                  (integer? actual-count)
                  (integer? expected-count))
           undefined))))
    ((name . "test-runner-on-bad-count!")
     (signature
       lambda
       ((test-runner? runner) (procedure? on-bad-count))
       undefined)
     (subsigs
       (on-bad-count
         (lambda ((test-runner? runner)
                  (integer? actual-count)
                  (integer? expected-count))
           undefined)))))
  (desc . "Called from test-end (before the on-group-end-function is called) if an expected-count was specified by the matching test-begin and the expected-count does not match the actual-count of tests actually executed or skipped."))
 ((group
    ((name . "test-runner-on-bad-end-name")
     (signature lambda ((test-runner? runner)) procedure?)
     (subsigs
       (return
         (lambda ((test-runner? runner) (string? begin-name) (string? end-name))
           undefined))))
    ((name . "test-runner-on-bad-end-name!")
     (signature
       lambda
       ((test-runner? runner) (procedure? on-bad-end-name))
       procedure?)
     (subsigs
       (on-bad-end-name
         (lambda ((test-runner? runner) (string? begin-name) (string? end-name))
           undefined)))))
  (desc . "Called from test-end (before the on-group-end-function is called) if a suite-name was specified, and it did not that the name in the matching test-begin."))
 ((group
    ((name . "test-runner-on-final")
     (signature lambda ((test-runner? runner)) procedure?)
     (subsigs (return (lambda ((test-runner? runner)) undefined))))
    ((name . "test-runner-on-final!")
     (signature lambda ((test-runner? runner) (procedure? on-final)) undefined)
     (subsigs (on-final (lambda ((test-runner? runner)) undefined)))))
  (desc . "The on-final-function takes one parameter (a test-runner) and typically displays a summary (count) of the tests. The on-final-function is called after called the on-group-end-function correspondiong to the outermost test-end. The default value is test-on-final-simple which writes to the standard output port the number of tests of the various kinds. "))
 ((group
    ((name . "test-on-test-begin-simple")
     (signature lambda ((test-runner? runner)) undefined))
    ((name . "test-on-test-end-simple")
     (signature lambda ((test-runner? runner)) undefined))
    ((name . "test-on-group-begin-simple")
     (signature
       lambda
       ((test-runner? runner) (string? suite-name) (integer? count))
       undefined))
    ((name . "test-on-group-end-simple")
     (signature lambda ((test-runner? runner)) undefined))
    ((name . "test-on-bad-count-simple")
     (signature
       lambda
       ((test-runner? runner) (integer? actual-count) (integer? expected-count))
       undefined))
    ((name . "test-on-bad-end-name-simple")
     (signature
       lambda
       ((test-runner? runner) (string? begin-name) (string? end-name))
       undefined)))
  (desc . "The callback functions used by the default test-runner returned by test-runner-simple."))
 ((name . "test-runner-pass-count")
  (signature lambda ((test-runner? runner)) integer?)
  (desc . "Returns the number of tests that passed, and were expected to pass."))
 ((name . "test-runner-fail-count")
  (signature lambda ((test-runner? runner)) integer?)
  (desc . "Returns the number of tests that failed, but were expected to pass."))
 ((name . "test-runner-xpass-count")
  (signature lambda ((test-runner? runner)) integer?)
  (desc . "Returns the number of tests that passed, but were expected to fail."))
 ((name . "test-runner-xfail-count")
  (signature lambda ((test-runner? runner)) integer?)
  (desc . "Returns the number of tests that failed, and were expected to pass."))
 ((name . "test-runner-skip-count")
  (signature lambda ((test-runner? runner)) integer?)
  (desc . "Returns the number of tests or test groups that were skipped."))
 ((name . "test-runner-test-name")
  (signature lambda ((test-runner? runner)) string?)
  (desc . "Returns the name of the current test or test group, as a string. During execution of test-begin this is the name of the test group; during the execution of an actual test, this is the name of the test-case. If no name was specified, the name is the empty string."))
 ((name . "test-runner-group-path")
  (signature lambda ((test-runner? runner)) list?)
  (desc . "A list of names of groups we're nested in, with the outermost group first."))
 ((name . "test-runner-group-stack")
  (signature lambda ((test-runner? runner)) list?)
  (desc . "A list of names of groups we're nested in, with the outermost group last. (This is more efficient than test-runner-group-path, since it doesn't require any copying.)"))
 ((group
    ((name . "test-runner-aux-value")
     (signature lambda ((test-runner? runner)) *))
    ((name . "test-runner-aux-value!")
     (signature lambda ((test-runner? runner) on-test) *)))
  (desc . "Get or set the aux-value field of a test-runner. This field is not used by this API or the test-runner-simple test-runner, but may be used by custom test-runners to store extra state."))
 ((name . "test-runner-reset")
  (signature lambda ((test-runner? runner)) undefined)
  (desc . "Resets the state of the runner to its initial state.")))
