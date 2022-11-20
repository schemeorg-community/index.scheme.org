(((name . "load")
  (signature
   case-lambda
   (((string? filename)) undefined)
   (((string? filename) environment-specifier) undefined))
  (desc . "It is an error if filename is not a string. An implementation-dependent operation is used to transform filename into the name of an existing file containing Scheme source code. The load procedure reads expressions and definitions from the file and evaluates them sequentially in the environment specified by environment-specifier . If environment-specifier is omitted, (interaction-environment) is assumed. It is unspecified whether the results of the expressions are printed. The load procedure does not affect the values returned by current-input-port and current-output-port. It returns an unspecified value. Rationale: For portability, load must operate on source files. Its operation on other kinds of files necessarily varies among implementations")))
