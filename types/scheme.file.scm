(
 (call-with-input-file
   (lambda ((string? string) (procedure? proc)) *)
   ()
   ((proc (lambda ((input-port? port)) *))))
 
 (call-with-output-file
   (lambda ((string? string) (procedure? proc)) *)
   ()
   ((proc (lambda ((output-port? port)) *))))
 
 (delete-file
   (lambda ((string? filename)) undefined))
 
 (file-exists?
   (lambda ((string? filename)) boolean?))
 
 (open-binary-input-file
   (lambda ((string? string)) input-port?))
 
 (open-binary-output-file
   (lambda ((string? string)) output-port?))
 
 (open-input-file
   (lambda ((string? string)) input-port?))
 
 (open-output-file
   (lambda ((string? string)) output-port?))
 
 (with-input-from-file
   (lambda ((string? string) (procedure? thunk)) *)
   (parameterized)
   ((thunk (lambda () *))))
 
 (with-output-to-file
   (lambda ((string? string) (procedure? thunk)) *)
   (parameterized)
   ((thunk (lambda () *))))
 )
