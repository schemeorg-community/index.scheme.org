(((name . "make-timer")
  (signature case-lambda
             (() timer?)
             (((procedure? error-handler)) timer?))
  (subsigs
    (error-handler (lambda (err) *)))
  (desc . "Creates and starts a timer object. The optional argument error-handler must be a procedure which accepts one argument. If it is given and when a timer task raises an error, then the handler will be invoked and timer will continue if the error-handler wouldn't raise an error. Otherwise whenever an error is raised, timer stops and preserves the error. The error is raised when timer-cancel! procedure is called.
Two timers should run in separate context, means whenever timer A is executing a task, timer B should not be disturbed executing a task by timer A's execution."))
 ((name . "timer?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if given obj is a timer object, otherwise #f."))
 ((name . "timer-cancel!")
  (signature lambda ((timer? timer)) undefined)
  (desc . "Stops the given timer. The procedure raises the preserved error if there is. Once a timer is stopped, it will never be able to start again."))
 ((name . "timer-schedule!")
  (signature case-lambda
             (((timer? timer) (procedure? thunk) ((or timer-delta? integer?) when)) timer-id)
             (((timer? timer) (procedure? thunk) ((or timer-delta? integer?) when) ((or timer-delta? integer?) period)) timer-id))
  (subsigs
    (thunk (lambda () *)))
  (desc . "Schedules the given thunk as the given timer's task. The when argument specifies when the task will be started. It can be either timer delta object or non negative integer. The task is scheduled on the time when the given when passed from the procedure is called. The task is executed on the dynamic environment where the timer is created.
If the optional argument period is given, which must be either timer delta object or an integer, then the given task is scheduled as periodical task. The next task is scheduled by adding when and period. If the period or when is an integer, then it is interpreted as milliseconds.
The procedure returns task id, which is a readable datum such as an integer.
The executing order of the same timing tasks are not defined.
A task should be able to cancel or reschedule other tasks. But it should not be able to cancel or reschedule itself.
If a task is rescheduled whenever it's executed, the timer doesn't stop its execution. It is rescheduled but the current execution will be continued."))
 ((name . "timer-reschedule!")
  (signature case-lambda
             (((timer? timer) (timer-id id) ((or timer-delta? integer?) when)) timer-id)
             (((timer? timer) (timer-id id) ((or timer-delta? integer?) when) ((or timer-delta? integer?) period)) timer-id))
  (desc . "Reschedules the task associated to the given id on the given timer. The when and period arguments are the same as timer-schedule!.
Thus to cancel the periodical task, you can specify 0 as period argument.
The procedure returns given id.
It is an error if the given task id is not associated with the given timer or if the task is already executed and not scheduled anymore."))
 ((name . "timer-task-remove!")
  (signature lambda ((timer? timer) (timer-id id)) boolean?)
  (desc . "Removes the task associated to the given id on the given timer. It returns #t if a task is removed, otherwise #f."))
 ((name . "timer-task-exists?")
  (signature lambda ((timer? timer) (timer-id id)) boolean?)
  (desc . "Returns #t if a task associated to the given id exists, otherwise #f."))
 ((name . "make-timer-delta")
  (signature lambda ((integer? n) (symbol? unit)) timer-delta?)
  (tags pure)
  (desc . "Creates a timer delta object. n must be an integer and unit must be a symbol which represents the time unit. Implementations must support the following units:
h : hour
m : minute
s : second
ms : millisecond
us : microsecond
ns : nanosecond

And may support other unit."))
 ((name . "timer-delta?")
  (signature lambda (obj) boolean?)
  (tags predicate pure)
  (desc . "Returns #t if given obj is a timer delta object, otherwise #f.")))
