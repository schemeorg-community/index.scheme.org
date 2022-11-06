(((name . "error")
  (signature lambda ((string? message) obj ...) undefined)
  (desc . "Message should be a string. Raises an exception as if by calling raise on a newly allocated implementation-defined object which encapsulates the information provided by message, as well as any objs, known as the irritants. The procedure error-object? must return #t on such objects.")))
