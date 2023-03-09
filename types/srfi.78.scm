(((name . "check")
  (signature
   syntax-rules
   (=>)
   ((_ expr (=> equal) expected))
   ((_ expr => expected)))
  (subsigs (equal (value procedure?)))
  (desc . "Evaluates <expr> and compares the value to the value of <expected> using the predicate <equal>, which is equal? when omitted. Then a report is printed according to the current mode setting (see below) and the outcome is recorded in a global state to be used in check-report. The precise order of evaluation is that first <equal> and <expected> are evaluated (in unspecified order) and then <expr> is evaluated."))
 ((name . "check-ec")
  (signature
   syntax-rules
   (=>)
   ((_ qualifier ... expr (=> equal) expected (argument ...)))
   ((_ qualifier ... expr => expected (argument ...)))
   ((_ qualifier ... expr (=> equal) expected))
   ((_ qualifier ... expr => expected)))
  (subsigs
   (qualifier
    (pattern
     generator
     (if test)
     (not test)
     (and test ...)
     (or test ...)
     (begin command ... expression)
     (nested qualifier ...)))
   (equal (value procedure?))
   (generator (value generator-macro)))
  (desc . "An eager comprehension for executing a parametric sequence of checks.
Enumerates the sequence of bindings specified by <qualifier>*. For each binding evaluates <equal> and <expected> in unspecified order. Then evalues <expr> and compares the value obtained to the value of <expected> using the value of <equal> as predicate, which is equal? when omitted. The comprehension stops after the first failed check, if there is any. Then a report is printed according to the current mode setting (see below) and the outcome is recorded in a global state to be used in check-report. The entire check-ec counts as a single check.
In case the check fails <argument>* is used for constructing an informative message with the argument values. Use <argument>* to list the relevant free variables of <expr> (see examples) that you want to have printed.
A <qualifier> is any qualifier of an eager comprehension as specified in SRFI 42 "))
 ((name . "check-report") 
  (signature lambda () undefined)
  (desc . "Prints a summary and the first failed check, if there is any, depending on the current mode settings."))
 ((name . "check-set-mode!")
  (signature lambda ((symbol? mode)) undefined)
  (desc . "Sets the current mode to mode, which must be a symbol in '(off summary report-failed report), default is 'report. Note that you can change the mode at any time, and that check, check-ec and check-report use the current value.
The mode symbols have the following meaning:
    off: do not execute any of the checks,
    summary: print only summary in (check-report) and nothing else,
    report-failed: report failed checks when they happen, and in summary,
    report: report every example executed."))
 ((name . "check-reset!") 
  (signature lambda () undefined)
  (desc . "Resets the global state (counters of correct/failed examples) to the state immediately after loading the module for the first time, i.e. no checks have been executed."))
 ((name . "check-passed?")
  (signature lambda ((integer? expected-total-count)) boolean?)
  (desc . "#t if there were no failed checks and expected-total-count correct checks, #f otherwise.
Rationale: This procedure can be used in automatized tests by terminating a test program with the statement (exit (if (check-passed? n) 0 1)).")))
