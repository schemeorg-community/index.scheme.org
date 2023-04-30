(((name . "string-append!")
  (signature lambda ((string? string) ((or char? string?) value) ...) undefined)
  (desc . "
The string must be a variable-size mutable string. The string-append! procedure extends string by appending each value (in order) to the end of string. A value can be a character or a string.
Using a string port in this situation is probably preferable: It is more portable, and you can expect decent performance in most implementations. Using string-append! may be slighly more efficient on some implementations, due to lower overhead, but that depends on the strategy used by string-append! when the allocated buffer is too small. The string-append! function is most useful when using (reading) a string is interleaved with growing it, or when also using string-replace!."))
 ((name . "string-replace!")
  (signature case-lambda 
             (((string? dst) (integer? dst-start) (integer? dst-end) (string? src)) undefined)
             (((string? dst) (integer? dst-start) (integer? dst-end) (string? src) (integer? src-start)) undefined)
             (((string? dst) (integer? dst-start) (integer? dst-end) (string? src) (integer? src-start) (integer? src-end)) undefined))
  (desc . "Replaces the characters of the variable-size string dst (between dst-start and dst-end) with the characters of the string src (between src-start and src-end). The number of characters from src may be different than the number replaced in dst, so the string may grow or contract. The special case where dst-start is equal to dst-end corresponds to insertion; the case where src-start is equal to src-end corresponds to deletion. The order in which characters are copied is unspecified, except that if the source and destination overlap, copying takes place as if the source is first copied into a temporary string and then into the destination. (This can be achieved without allocating storage by making sure to copy in the correct direction in such circumstances.)
When value is a string then (string-append! dst value) is equivalent to (string-replace! dst (string-length dst) (string-length dst) value).")))
