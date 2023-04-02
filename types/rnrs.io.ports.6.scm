(((name . "file-options")
  (signature syntax-rules () ((_ file-options-symbol ...) file-options))
  (desc . "Each <file-options symbol> must be a symbol. The file-options syntax returns a file-options object that encapsulates the specified options.
When supplied to an operation that opens a file for output, the file-options object returned by (file-options) specifies that the file is created if it does not exist and an exception with condition type &i/o-file-already-exists is raised if it does exist. The following standard options can be included to modify the default behavior.
no-create: If the file does not already exist, it is not created; instead, an exception with condition type &i/o-file-does-not-exist is raised. If the file already exists, the exception with condition type &i/o-file-already-exists is not raised and the file is truncated to zero length.
no-fail: If the file already exists, the exception with condition type &i/o-file-already-exists is not raised, even if no-create is not included, and the file is truncated to zero length.
no-truncate: If the file already exists and the exception with condition type &i/o-file-already-exists has been inhibited by inclusion of no-create or no-fail, the file is not truncated, but the port's current position is still set to the beginning of the file.
These options have no effect when a file is opened only for input. Symbols other than those listed above may be used as <file-options symbol>s; they have implementation-specific meaning, if any."))
 ((name . "buffer-mode")
  (signature syntax-rules () ((_ buffer-mode-symbol) buffer-mode?))
  (desc . "<Buffer-mode symbol> must be a symbol whose name is one of none, line, and block. The result is the corresponding symbol, and specifies the associated buffer mode."))
 ((name . "buffer-mode?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if the argument is a valid buffer-mode symbol, and returns #f otherwise."))
 ((group
    ((name . "latin-1-codec") (signature lambda () codec) (tags pure))
    ((name . "utf-8-codec") (signature lambda () codec) (tags pure))
    ((name . "utf-16-codec") (signature lambda () codec) (tags pure)))
  (desc . "These are predefined codecs for the ISO 8859-1, UTF-8, and UTF-16 encoding schemes.
A call to any of these procedures returns a value that is equal in the sense of eqv? to the result of any other call to the same procedure."))
 ((name . "eol-style")
  (signature syntax-rules () ((_ eol-style-symbol) symbol?))
  (desc . "<Eol-style symbol> should be a symbol whose name is one of lf, cr, crlf, nel, crnel, ls, and none. The form evaluates to the corresponding symbol. If the name of eol-style symbol is not one of these symbols, the effect and result are implementation-dependent; in particular, the result may be an eol-style symbol acceptable as an eol-style argument to make-transcoder. Otherwise, an exception is raised. For a textual port with a transcoder, and whose transcoder has an eol-style symbol none, no conversion occurs. For a textual input port, any eol-style symbol other than none means that all of the above line-ending encodings are recognized and are translated into a single linefeed. For a textual output port, none and lf are equivalent. Linefeed characters are encoded according to the specified eol-style symbol, and all other characters that participate in possible line endings are encoded as is."))
 ((name . "native-eol-style")
  (signature lambda () symbol?)
  (tags pure)
  (desc . " Returns the default end-of-line style of the underlying platform, e.g., lf on Unix and crlf on Windows."))
 ((group
    ((name . "&i/o-decoding") (signature value record-type-descriptor?))
    ((name . "make-i/o-decoding-error")
     (signature lambda ((port? port)) i/o-decoding-error?)
     (tags pure))
    ((name . "i/o-decoding-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate)))
  (desc . "This condition type could be defined by
(define-condition-type &i/o-decoding &i/o-port
                       make-i/o-decoding-error i/o-decoding-error?)

An exception with this type is raised when one of the operations for textual input from a port encounters a sequence of bytes that cannot be translated into a character or string by the input direction of the port's transcoder. When such an exception is raised, the port's position is past the invalid encoding."))
 ((group
    ((name . "&i/o-encoding") (signature value record-type-descriptor?))
    ((name . "make-i/o-encoding-error")
     (signature lambda ((port? port) (char? char)) i/o-encoding-error?)
     (tags pure))
    ((name . "i/o-encoding-error?")
     (signature lambda (obj) boolean?)
     (tags pure predicate))
    ((name . "i/o-encoding-error-char")
     (signature lambda ((i/o-encoding-error? condition)) char?)
     (tags pure)))
  (desc . "This condition type could be defined by
(define-condition-type &i/o-encoding &i/o-port
                       make-i/o-encoding-error i/o-encoding-error?
                       (char i/o-encoding-error-char))

An exception with this type is raised when one of the operations for textual output to a port encounters a character that cannot be translated into bytes by the output direction of the port's transcoder. Char is the character that could not be encoded."))
 ((name . "error-handling-mode")
  (signature syntax-rules () ((_ error-handling-mode-symbol) symbol?))
  (desc . "<Error-handling-mode symbol> should be a symbol whose name is one of ignore, raise, and replace. The form evaluates to the corresponding symbol. If error-handling-mode symbol is not one of these identifiers, effect and result are implementation-dependent: The result may be an error-handling-mode symbol acceptable as a handling-mode argument to make-transcoder. If it is not acceptable as a handling-mode argument to make-transcoder, an exception is raised.
 The error-handling mode of a transcoder specifies the behavior of textual I/O operations in the presence of encoding or decoding errors.
If a textual input operation encounters an invalid or incomplete character encoding, and the error-handling mode is ignore, an appropriate number of bytes of the invalid encoding are ignored and decoding continues with the following bytes. If the error-handling mode is replace, the replacement character U+FFFD is injected into the data stream, an appropriate number of bytes are ignored, and decoding continues with the following bytes. If the error-handling mode is raise, an exception with condition type &i/o-decoding is raised.
If a textual output operation encounters a character it cannot encode, and the error-handling mode is ignore, the character is ignored and encoding continues with the next character. If the error-handling mode is replace, a codec-specific replacement character is emitted by the transcoder, and encoding continues with the next character. The replacement character is U+FFFD for transcoders whose codec is one of the Unicode encodings, but is the ? character for the Latin-1 encoding. If the error-handling mode is raise, an exception with condition type &i/o-encoding is raised."))
 ((name . "make-transcoder")
  (signature
   case-lambda
   (((codec codec)) transcoder)
   (((codec codec) (symbol? eol-style)) transcoder)
   (((codec codec) (symbol? eol-style) (symbol? handling-mode)) transcoder))
  (desc . "Codec must be a codec; eol-style, if present, an eol-style symbol; and handling-mode, if present, an error-handling-mode symbol. Eol-style may be omitted, in which case it defaults to the native end-of-line style of the underlying platform. Handling-mode may be omitted, in which case it defaults to replace. The result is a transcoder with the behavior specified by its arguments."))
 ((name . "native-transcoder") (signature lambda () transcoder) (desc . "Returns an implementation-dependent transcoder that represents a possibly locale-dependent \"native\" transcoding."))
 ((group
    ((name . "transcoder-codec")
     (signature lambda ((transcoder transcoder)) codec)
     (tags pure))
    ((name . "transcoder-eol-style")
     (signature lambda ((transcoder transcoder)) symbol?)
     (tags pure))
    ((name . "transcoder-error-handling-mode")
     (signature lambda ((transcoder transcoder)) symbol?)
     (tags pure)))
  (desc . "These are accessors for transcoder objects; when applied to a transcoder returned by make-transcoder, they return the codec, eol-style, and handling-mode arguments, respectively"))
 ((name . "bytevector->string")
  (signature lambda ((bytevector? bytevector) (transcoder transcoder)) string?)
  (tags pure)
  (desc . "Returns the string that results from transcoding the bytevector according to the input direction of the transcoder."))
 ((name . "string->bytevector")
  (signature lambda ((string? string) (transcoder transcoder)) bytevector?)
  (tags pure)
  (desc . "Returns the bytevector that results from transcoding the string according to the output direction of the transcoder."))
 ((name . "eof-object") (signature lambda () eof-object?) (tags pure) (desc . "Returns the end-of-file object."))
 ((name . "eof-object?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if obj is the end-of-file object, #f otherwise."))
 ((name . "port?") (signature lambda (obj) boolean?) (tags pure predicate) (desc . "Returns #t if the argument is a port, and returns #f otherwise."))
 ((name . "port-transcoder")
  (signature lambda ((port? port)) transcoder)
  (tags pure)
  (desc . "Returns the transcoder associated with port if port is textual and has an associated transcoder, and returns #f if port is binary or does not have an associated transcoder."))
 ((name . "textual-port?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "The textual-port? procedure returns #t if port is textual, and returns #f otherwise."))
 ((name . "binary-port?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "The binary-port? procedure returns #t if port is binary, and returns #f otherwise."))
 ((name . "transcoded-port")
  (signature
   lambda
   ((binary-port? port) (transcoder transcoder))
   textual-port?)
  (desc . "The transcoded-port procedure returns a new textual port with the specified transcoder. Otherwise the new textual port's state is largely the same as that of binary-port. If binary-port is an input port, the new textual port will be an input port and will transcode the bytes that have not yet been read from binary-port. If binary-port is an output port, the new textual port will be an output port and will transcode output characters into bytes that are written to the byte sink represented by binary-port.
As a side effect, however, transcoded-port closes binary-port in a special way that allows the new textual port to continue to use the byte source or sink represented by binary-port, even though binary-port itself is closed and cannot be used by the input and output operations described in this chapter."))
 ((group
    ((name . "port-has-port-position?")
     (signature lambda ((port? port)) boolean?)
     (tags pure))
    ((name . "port-position")
     (signature
       case-lambda
       (((binary-port? port)) integer?)
       (((textual-port? port)) opaque-port-position))))
  (desc . "The port-has-port-position? procedure returns #t if the port supports the port-position operation, and #f otherwise.
For a binary port, the port-position procedure returns the index of the position at which the next byte would be read from or written to the port as an exact non-negative integer object. For a textual port, port-position returns a value of some implementation-dependent type representing the port's position; this value may be useful only as the pos argument to set-port-position!, if the latter is supported on the port (see below).
If the port does not support the operation, port-position raises an exception with condition type &assertion."))
 ((group
    ((name . "port-has-set-port-position!?")
     (signature lambda ((port? port)) boolean?)
     (tags pure))
    ((name . "set-port-position!")
     (signature
       case-lambda
       (((binary-port? port) (integer? pos)) undefined)
       (((textual-port? port) (opaque-port-position pos)) undefined))))
  (desc . "If port is a binary port, pos should be a non-negative exact integer object. If port is a textual port, pos should be the return value of a call to port-position on port.
The port-has-set-port-position!? procedure returns #t if the port supports the set-port-position! operation, and #f otherwise.
The set-port-position! procedure raises an exception with condition type &assertion if the port does not support the operation, and an exception with condition type &i/o-invalid-position if pos is not in the range of valid positions of port. Otherwise, it sets the current position of the port to pos. If port is an output port, set-port-position! first flushes port. (See flush-output-port, section 8.2.10.)
If port is a binary output port and the current position is set beyond the current end of the data in the underlying data sink, the object is not extended until new data is written at that position. The contents of any intervening positions are unspecified. Binary ports created by open-file-output-port and open-file-input/output-port can always be extended in this manner within the limits of the underlying operating system. In other cases, attempts to set the port beyond the current end of data in the underlying object may result in an exception with condition type &i/o-invalid-position."))
 ((name . "close-port") 
  (signature lambda ((port? port)) undefined)
  (desc . "Closes the port, rendering the port incapable of delivering or accepting data. If port is an output port, it is flushed before being closed. This has no effect if the port has already been closed. A closed port is still a port. The close-port procedure returns unspecified values."))
 ((name . "call-with-port")
  (signature lambda ((port? port) (procedure? proc)) *)
  (subsigs (proc (lambda ((port? port)) *)))
  (desc . "Proc must accept one argument. The call-with-port procedure calls proc with port as an argument. If proc returns, port is closed automatically and the values returned by proc are returned. If proc does not return, port is not closed automatically, except perhaps when it is possible to prove that port will never again be used for an input or output operation"))
 ((name . "input-port?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if the argument is an input port (or a combined input and output port), and returns #f otherwise."))
 ((name . "port-eof?") 
  (signature lambda ((input-port? port)) boolean?)
  (desc . "Returns #t if the lookahead-u8 procedure (if input-port is a binary port) or the lookahead-char procedure (if input-port is a textual port) would return the end-of-file object, and #f otherwise. The operation may block indefinitely if no data is available but the port cannot be determined to be at end of file."))
 ((name . "open-file-input-port")
  (signature
   case-lambda
   (((string? string)) input-port?)
   (((string? string) (file-options options)) input-port?)
   (((string? string) (file-options options) (buffer-mode? buffer-mode))
    input-port?)
   (((string? string)
     (file-options options)
     (buffer-mode? buffer-mode)
     ((or #f transcoder) transcoder))
    input-port?))
  (desc . "Maybe-transcoder must be either a transcoder or #f.
The open-file-input-port procedure returns an input port for the named file. The file-options and maybe-transcoder arguments are optional.
The file-options argument, which may determine various aspects of the returned port (see section 8.2.2), defaults to the value of (file-options).
The buffer-mode argument, if supplied, must be one of the symbols that name a buffer mode. The buffer-mode argument defaults to block.
If maybe-transcoder is a transcoder, it becomes the transcoder associated with the returned port.
If maybe-transcoder is #f or absent, the port will be a binary port and will support the port-position and set-port-position! operations. Otherwise the port will be a textual port, and whether it supports the port-position and set-port-position! operations is implementation-dependent (and possibly transcoder-dependent)."))
 ((name . "open-bytevector-input-port")
  (signature
   case-lambda
   (((bytevector? bytevector)) input-port?)
   (((bytevector? bytevector) ((or #f transcoder) transcoder)) input-port?))
  (desc . "Maybe-transcoder must be either a transcoder or #f.
The open-bytevector-input-port procedure returns an input port whose bytes are drawn from bytevector. If transcoder is specified, it becomes the transcoder associated with the returned port.
If maybe-transcoder is #f or absent, the port will be a binary port and will support the port-position and set-port-position! operations. Otherwise the port will be a textual port, and whether it supports the port-position and set-port-position! operations will be implementation-dependent (and possibly transcoder-dependent).
If bytevector is modified after open-bytevector-input-port has been called, the effect on the returned port is unspecified."))
 ((name . "open-string-input-port")
  (signature lambda ((string? string)) input-port?)
  (desc . "Returns a textual input port whose characters are drawn from string. The port may or may not have an associated transcoder; if it does, the transcoder is implementation-dependent. The port should support the port-position and set-port-position! operations. If string is modified after open-string-input-port has been called, the effect on the returned port is unspecified."))
 ((name . "standard-input-port") 
  (signature lambda () binary-port?)
  (desc . "Returns a fresh binary input port connected to standard input. Whether the port supports the port-position and set-port-position! operations is implementation-dependent."))
 ((name . "current-input-port") 
  (signature lambda () textual-port?)
  (desc . "This returns a default textual port for input. Normally, this default port is associated with standard input, but can be dynamically re-assigned using the with-input-from-file procedure from the (rnrs io simple (6)) library (see section 8.3). The port may or may not have an associated transcoder; if it does, the transcoder is implementation-dependent."))
 ((name . "make-custom-binary-input-port")
  (signature
   lambda
   ((string? id)
    (procedure? read!)
    ((or #f procedure?) get-position)
    ((or #f procedure?) set-position!)
    ((or #f procedure?) close))
   input-port?)
  (subsigs
   (read!
    (lambda ((bytevector? bytevector) (integer? start) (integer? count))
      integer?))
   (get-position (lambda () integer?))
   (set-position! (lambda ((integer? position)) undefined))
   (close (lambda () undefined)))
  (desc . "Returns a newly created binary input port whose byte source is an arbitrary algorithm represented by the read! procedure. Id must be a string naming the new port, provided for informational purposes only. Read! must be a procedure and should behave as specified below; it will be called by operations that perform binary input.
Each of the remaining arguments may be #f; if any of those arguments is not #f, it must be a procedure and should behave as specified below.
    (read! bytevector start count) Start will be a non-negative exact integer object, count will be a positive exact integer object, and bytevector will be a bytevector whose length is at least start + count. The read! procedure should obtain up to count bytes from the byte source, and should write those bytes into bytevector starting at index start. The read! procedure should return an exact integer object. This integer object should represent the number of bytes that it has read. To indicate an end of file, the read! procedure should write no bytes and return 0.
    (get-position) The get-position procedure (if supplied) should return an exact integer object representing the current position of the input port. If not supplied, the custom port will not support the port-position operation.
    (set-position! pos) Pos will be a non-negative exact integer object. The set-position! procedure (if supplied) should set the position of the input port to pos. If not supplied, the custom port will not support the set-port-position! operation.
    (close) The close procedure (if supplied) should perform any actions that are necessary when the input port is closed."))
 ((name . "make-custom-textual-input-port")
  (signature
   lambda
   ((string? id)
    (procedure? read!)
    ((or #f procedure?) get-position)
    ((or #f procedure?) set-position!)
    ((or #f procedure?) close))
   input-port?)
  (subsigs
   (read!
    (lambda ((string? string) (integer? start) (integer? count)) integer?))
   (get-position (lambda () opaque-port-position))
   (set-position! (lambda ((opaque-port-position position)) undefined))
   (close (lambda () undefined)))
  (desc . "Returns a newly created textual input port whose character source is an arbitrary algorithm represented by the read! procedure. Id must be a string naming the new port, provided for informational purposes only. Read! must be a procedure and should behave as specified below; it will be called by operations that perform textual input.
Each of the remaining arguments may be #f; if any of those arguments is not #f, it must be a procedure and should behave as specified below.
    (read! string start count) Start will be a non-negative exact integer object, count will be a positive exact integer object, and string will be a string whose length is at least start + count. The read! procedure should obtain up to count characters from the character source, and should write those characters into string starting at index start. The read! procedure should return an exact integer object representing the number of characters that it has written. To indicate an end of file, the read! procedure should write no bytes and return 0.
    (get-position) The get-position procedure (if supplied) should return a single value. The return value should represent the current position of the input port. If not supplied, the custom port will not support the port-position operation.
    (set-position! pos) The set-position! procedure (if supplied) should set the position of the input port to pos if pos is the return value of a call to get-position. If not supplied, the custom port will not support the set-port-position! operation.
    (close) The close procedure (if supplied) should perform any actions that are necessary when the input port is closed.
The port may or may not have an an associated transcoder; if it does, the transcoder is implementation-dependent."))
 ((name . "get-u8")
  (signature lambda ((input-port? input-port)) (or eof-object? integer?))
  (desc . "Reads from binary-input-port, blocking as necessary, until a byte is available from binary-input-port or until an end of file is reached. If a byte becomes available, get-u8 returns the byte as an octet and updates binary-input-port to point just past that byte. If no input byte is seen before an end of file is reached, the end-of-file object is returned."))
 ((name . "lookahead-u8")
  (signature lambda ((input-port? input-port)) (or eof-object? integer?))
  (desc . "The lookahead-u8 procedure is like get-u8, but it does not update binary-input-port to point past the byte."))
 ((name . "get-bytevector-n")
  (signature
   lambda
   ((input-port? input-port) (integer? count))
   (or eof-object? bytevector?))
  (desc . "Count must be an exact, non-negative integer object representing the number of bytes to be read. The get-bytevector-n procedure reads from binary-input-port, blocking as necessary, until count bytes are available from binary-input-port or until an end of file is reached. If count bytes are available before an end of file, get-bytevector-n returns a bytevector of size count. If fewer bytes are available before an end of file, get-bytevector-n returns a bytevector containing those bytes. In either case, the input port is updated to point just past the bytes read. If an end of file is reached before any bytes are available, get-bytevector-n returns the end-of-file object."))
 ((name . "get-bytevector-n!")
  (signature
   lambda
   ((input-port? input-port)
    (bytevector? bytevector)
    (integer? start)
    (integer? count))
   (or integer? eof-object?))
  (desc . "Count must be an exact, non-negative integer object, representing the number of bytes to be read. bytevector must be a bytevector with at least start + count elements.
The get-bytevector-n! procedure reads from binary-input-port, blocking as necessary, until count bytes are available from binary-input-port or until an end of file is reached. If count bytes are available before an end of file, they are written into bytevector starting at index start, and the result is count. If fewer bytes are available before the next end of file, the available bytes are written into bytevector starting at index start, and the result is a number object representing the number of bytes actually read. In either case, the input port is updated to point just past the bytes read. If an end of file is reached before any bytes are available, get-bytevector-n! returns the end-of-file object."))
 ((name . "get-bytevector-some")
  (signature lambda ((input-port? input-port)) (or bytevector? eof-object?))
  (desc . "Reads from binary-input-port, blocking as necessary, until bytes are available from binary-input-port or until an end of file is reached. If bytes become available, get-bytevector-some returns a freshly allocated bytevector containing the initial available bytes (at least one), and it updates binary-input-port to point just past these bytes. If no input bytes are seen before an end of file is reached, the end-of-file object is returned."))
 ((name . "get-bytevector-all")
  (signature lambda ((input-port? input-port)) (or bytevector? eof-object?))
  (desc . "Attempts to read all bytes until the next end of file, blocking as necessary. If one or more bytes are read, get-bytevector-all returns a bytevector containing all bytes up to the next end of file. Otherwise, get-bytevector-all returns the end-of-file object. The operation may block indefinitely waiting to see if more bytes will become available, even if some bytes are already available."))
 ((name . "get-char")
  (signature lambda ((input-port? input-port)) (or eof-object? char?))
  (desc . "Reads from textual-input-port, blocking as necessary, until a complete character is available from textual-input-port, or until an end of file is reached.
If a complete character is available before the next end of file, get-char returns that character and updates the input port to point past the character. If an end of file is reached before any character is read, get-char returns the end-of-file object."))
 ((name . "lookahead-char")
  (signature lambda ((input-port? input-port)) (or eof-object? char?))
  (desc . "The lookahead-char procedure is like get-char, but it does not update textual-input-port to point past the character."))
 ((name . "get-string-n")
  (signature
   lambda
   ((input-port? input-port) (integer? count))
   (or eof-object? string?))
  (desc . "Count must be an exact, non-negative integer object, representing the number of characters to be read.
The get-string-n procedure reads from textual-input-port, blocking as necessary, until count characters are available, or until an end of file is reached.
If count characters are available before end of file, get-string-n returns a string consisting of those count characters. If fewer characters are available before an end of file, but one or more characters can be read, get-string-n returns a string containing those characters. In either case, the input port is updated to point just past the characters read. If no characters can be read before an end of file, the end-of-file object is returned."))
 ((name . "get-string-n!")
  (signature
   lambda
   ((input-port? input-port)
    (string? string)
    (integer? start)
    (integer? count))
   (or eof-object? integer?))
  (desc . "Start and count must be exact, non-negative integer objects, with count representing the number of characters to be read. String must be a string with at least start + count characters.
The get-string-n! procedure reads from textual-input-port in the same manner as get-string-n. If count characters are available before an end of file, they are written into string starting at index start, and count is returned. If fewer characters are available before an end of file, but one or more can be read, those characters are written into string starting at index start and the number of characters actually read is returned as an exact integer object. If no characters can be read before an end of file, the end-of-file object is returned."))
 ((name . "get-string-all")
  (signature lambda ((input-port? input-port)) (or eof-object? string?))
  (desc . "Reads from textual-input-port until an end of file, decoding characters in the same manner as get-string-n and get-string-n!. If characters are available before the end of file, a string containing all the characters decoded from that data are returned. If no character precedes the end of file, the end-of-file object is returned."))
 ((name . "get-line")
  (signature lambda ((input-port? input-port)) (or eof-object? string?))
  (desc . "Reads from textual-input-port up to and including the linefeed character or end of file, decoding characters in the same manner as get-string-n and get-string-n!.
If a linefeed character is read, a string containing all of the text up to (but not including) the linefeed character is returned, and the port is updated to point just past the linefeed character. If an end of file is encountered before any linefeed character is read, but some characters have been read and decoded as characters, a string containing those characters is returned. If an end of file is encountered before any characters are read, the end-of-file object is returned."))
 ((name . "get-datum") 
  (signature lambda ((input-port? input-port)) *)
  (desc . "Reads an external representation from textual-input-port and returns the datum it represents. The get-datum procedure returns the next datum that can be parsed from the given textual-input-port, updating textual-input-port to point exactly past the end of the external representation of the object.
Any <interlexeme space> (see report section on “Lexical syntax”) in the input is first skipped. If an end of file occurs after the <interlexeme space>, the end-of-file object (see section 8.2.5) is returned.
If a character inconsistent with an external representation is encountered in the input, an exception with condition types &lexical and &i/o-read is raised. Also, if the end of file is encountered after the beginning of an external representation, but the external representation is incomplete and therefore cannot be parsed, an exception with condition types &lexical and &i/o-read is raised."))
 ((name . "output-port?")
  (signature lambda (obj) boolean?)
  (tags pure predicate)
  (desc . "Returns #t if the argument is an output port (or a combined input and output port), #f otherwise."))
 ((name . "flush-output-port")
  (signature lambda ((output-port? port)) undefined)
  (desc . "Flushes any buffered output from the buffer of output-port to the underlying file, device, or object. The flush-output-port procedure returns unspecified values."))
 ((name . "output-port-buffer-mode")
  (signature lambda ((output-port? port)) buffer-mode?)
  (desc . "Returns the symbol that represents the buffer mode of output-port."))
 ((name . "open-file-output-port")
  (signature
   case-lambda
   (((string? filename)) output-port?)
   (((string? filename) (file-options file-options)) output-port?)
   (((string? filename) (file-options file-options) (buffer-mode? buffer-mode))
    output-port?)
   (((string? filename)
     (file-options file-options)
     (buffer-mode? buffer-mode)
     ((or transcoder #f) maybe-transcoder))
    output-port?))
  (desc . "Maybe-transcoder must be either a transcoder or #f.
The open-file-output-port procedure returns an output port for the named file.
The file-options argument, which may determine various aspects of the returned port (see section 8.2.2), defaults to the value of (file-options).
The buffer-mode argument, if supplied, must be one of the symbols that name a buffer mode. The buffer-mode argument defaults to block.
If maybe-transcoder is a transcoder, it becomes the transcoder associated with the port.
If maybe-transcoder is #f or absent, the port will be a binary port and will support the port-position and set-port-position! operations. Otherwise the port will be a textual port, and whether it supports the port-position and set-port-position! operations is implementation-dependent (and possibly transcoder-dependent)."))
 ((name . "open-bytevector-output-port")
  (signature
   case-lambda
   (() (values output-port? procedure?))
   ((((or #f transcoder) maybe-transcoder)) (values output-port? procedure?)))
  (subsigs (return (lambda () bytevector?)))
  (desc . "Maybe-transcoder must be either a transcoder or #f.
The open-bytevector-output-port procedure returns two values: an output port and an extraction procedure. The output port accumulates the bytes written to it for later extraction by the procedure.
If maybe-transcoder is a transcoder, it becomes the transcoder associated with the port. If maybe-transcoder is #f or absent, the port will be a binary port and will support the port-position and set-port-position! operations. Otherwise the port will be a textual port, and whether it supports the port-position and set-port-position! operations is implementation-dependent (and possibly transcoder-dependent).
The extraction procedure takes no arguments. When called, it returns a bytevector consisting of all the port's accumulated bytes (regardless of the port's current position), removes the accumulated bytes from the port, and resets the port's position."))
 ((name . "call-with-bytevector-output-port")
  (signature
   case-lambda
   (((procedure? proc)) bytevector?)
   (((procedure? proc) ((or #f transcoder) maybe-transcoder)) bytevector?))
  (subsigs (proc (lambda ((output-port? port)) *)))
  (desc . "Proc must accept one argument. Maybe-transcoder must be either a transcoder or #f.
The call-with-bytevector-output-port procedure creates an output port that accumulates the bytes written to it and calls proc with that output port as an argument. Whenever proc returns, a bytevector consisting of all of the port's accumulated bytes (regardless of the port's current position) is returned and the port is closed.
The transcoder associated with the output port is determined as for a call to open-bytevector-output-port."))
 ((name . "open-string-output-port")
  (signature lambda () (values output-port? procedure?))
  (subsigs (return (lambda () string?)))
  (desc . "Returns two values: a textual output port and an extraction procedure. The output port accumulates the characters written to it for later extraction by the procedure.
The port may or may not have an associated transcoder; if it does, the transcoder is implementation-dependent. The port should support the port-position and set-port-position! operations.
The extraction procedure takes no arguments. When called, it returns a string consisting of all of the port's accumulated characters (regardless of the current position), removes the accumulated characters from the port, and resets the port's position."))
 ((name . "call-with-string-output-port")
  (signature lambda ((procedure? proc)) string?)
  (subsigs (proc (lambda ((output-port? port)) *)))
  (desc . "Proc must accept one argument. The call-with-string-output-port procedure creates a textual output port that accumulates the characters written to it and calls proc with that output port as an argument. Whenever proc returns, a string consisting of all of the port's accumulated characters (regardless of the port's current position) is returned and the port is closed.
The port may or may not have an associated transcoder; if it does, the transcoder is implementation-dependent. The port should support the port-position and set-port-position! operations."))
 ((group
    ((name . "standard-output-port") (signature lambda () output-port?))
    ((name . "standard-error-port") (signature lambda () output-port?)))
  (desc . "Returns a fresh binary output port connected to the standard output or standard error respectively. Whether the port supports the port-position and set-port-position! operations is implementation-dependent."))
 ((group
    ((name . "current-output-port")
     (signature lambda () output-port?))
    ((name . "current-error-port")
     (signature lambda () output-port?)))
  (desc . "These return default textual ports for regular output and error output. Normally, these default ports are associated with standard output, and standard error, respectively. The return value of current-output-port can be dynamically re-assigned using the with-output-to-file procedure from the (rnrs io simple (6)) library (see section 8.3). A port returned by one of these procedures may or may not have an associated transcoder; if it does, the transcoder is implementation-dependent."))
 ((name . "make-custom-binary-output-port")
  (signature
   lambda
   ((string? id)
    (procedure? write!)
    ((or #f procedure?) get-position)
    ((or #f procedure?) set-position!)
    ((or #f procedure?) close))
   output-port?)
  (subsigs
   (write!
    (lambda ((bytevector? bytevector) (integer? start) (integer? count))
      integer?))
   (get-position (lambda () integer?))
   (set-position! (lambda ((integer? position)) undefined))
   (close (lambda () undefined)))
  (desc . "Returns a newly created binary output port whose byte sink is an arbitrary algorithm represented by the write! procedure. Id must be a string naming the new port, provided for informational purposes only. Write! must be a procedure and should behave as specified below; it will be called by operations that perform binary output.
Each of the remaining arguments may be #f; if any of those arguments is not #f, it must be a procedure and should behave as specified in the description of make-custom-binary-input-port.
    (write! bytevector start count) Start and count will be non-negative exact integer objects, and bytevector will be a bytevector whose length is at least start + count. The write! procedure should write up to count bytes from bytevector starting at index start to the byte sink. If count is 0, the write! procedure should have the effect of passing an end-of-file object to the byte sink. In any case, the write! procedure should return the number of bytes that it wrote, as an exact integer object."))
 ((name . "make-custom-textual-output-port")
  (signature
   lambda
   ((string? id)
    (procedure? write!)
    ((or #f procedure?) get-position)
    ((or #f procedure?) set-position!)
    ((or #f procedure?) close))
   output-port?)
  (subsigs
   (write!
    (lambda ((string? string) (integer? start) (integer? count)) integer?))
   (get-position (lambda () integer?))
   (set-position! (lambda ((integer? position)) undefined))
   (close (lambda () undefined)))
  (desc . "Returns a newly created textual output port whose byte sink is an arbitrary algorithm represented by the write! procedure. Id must be a string naming the new port, provided for informational purposes only. Write! must be a procedure and should behave as specified below; it will be called by operations that perform textual output.
Each of the remaining arguments may be #f; if any of those arguments is not #f, it must be a procedure and should behave as specified in the description of make-custom-textual-input-port.
    (write! string start count) Start and count will be non-negative exact integer objects, and string will be a string whose length is at least start + count. The write! procedure should write up to count characters from string starting at index start to the character sink. If count is 0, the write! procedure should have the effect of passing an end-of-file object to the character sink. In any case, the write! procedure should return the number of characters that it wrote, as an exact integer object.

The port may or may not have an associated transcoder; if it does, the transcoder is implementation-dependent."))
 ((name . "put-u8")
  (signature lambda ((output-port? port) (integer? octet)) undefined)
  (desc . "Writes octet to the output port and returns unspecified values."))
 ((name . "put-bytevector")
  (signature
   case-lambda
   (((output-port? port) (bytevector? bytevector)) undefined)
   (((output-port? port) (bytevector? bytevector) (integer? start)) undefined)
   (((output-port? port)
     (bytevector? bytevector)
     (integer? start)
     (integer? count))
    undefined))
  (desc . "Start and count must be non-negative exact integer objects that default to 0 and (bytevector-length bytevector) − start, respectively. Bytevector must have a length of at least start + count. The put-bytevector procedure writes the count bytes of the bytevector bytevector starting at index start to the output port. The put-bytevector procedure returns unspecified values."))
 ((name . "put-char")
  (signature lambda ((output-port? port) (char? char)) undefined)
  (desc . "Writes char to the port. The put-char procedure returns unspecified values."))
 ((name . "put-string")
  (signature
   case-lambda
   (((output-port? port) (string? string)) undefined)
   (((output-port? port) (string? string) (integer? start)) undefined)
   (((output-port? port) (string? string) (integer? start) (integer? count))
    undefined))
  (desc . "Start and count must be non-negative exact integer objects. String must have a length of at least start + count. Start defaults to 0. Count defaults to (string-length string) − start. The put-string procedure writes the count characters of string starting at index start to the port. The put-string procedure returns unspecified values."))
 ((name . "put-datum")
  (signature lambda ((output-port? port) datum) undefined)
  (desc . "Datum should be a datum value. The put-datum procedure writes an external representation of datum to textual-output-port. The specific external representation is implementation-dependent. However, whenever possible, an implementation should produce a representation for which get-datum, when reading the representation, will return an object equal (in the sense of equal?) to datum."))
 ((name . "open-file-input/output-port")
  (signature
   case-lambda
   (((string? string)) port?)
   (((string? string) (file-options options)) port?)
   (((string? string) (file-options options) (buffer-mode? buffer-mode)) port?)
   (((string? string)
     (file-options options)
     (buffer-mode? buffer-mode)
     ((or #f transcoder) transcoder))
    port?))
  (desc . "Returns a single port that is both an input port and an output port for the named file. The optional arguments default as described in the specification of open-file-output-port. If the input/output port supports port-position and/or set-port-position!, the same port position is used for both input and output."))
 ((name . "make-custom-binary-input/output-port")
  (signature
   lambda
   ((string? id)
    (procedure? read!)
    (procedure? write!)
    ((or #f procedure?) get-position)
    ((or #f procedure?) set-position!)
    ((or #f procedure?) close))
   port?)
  (subsigs
   (read!
    (lambda ((bytevector? bytevector) (integer? start) (integer? count))
      integer?))
   (write!
    (lambda ((bytevector? bytevector) (integer? start) (integer? count))
      integer?))
   (get-position (lambda () integer?))
   (set-position! (lambda ((integer? position)) undefined))
   (close (lambda () undefined)))
  (desc . "Returns a newly created binary input/output port whose byte source and sink are arbitrary algorithms represented by the read! and write! procedures. Id must be a string naming the new port, provided for informational purposes only. Read! and write! must be procedures, and should behave as specified for the make-custom-binary-input-port and make-custom-binary-output-port procedures.
Each of the remaining arguments may be #f; if any of those arguments is not #f, it must be a procedure and should behave as specified in the description of make-custom-binary-input-port."))
 ((name . "make-custom-textual-input/output-port")
  (signature
   lambda
   ((string? id)
    (procedure? read!)
    (procedure? write!)
    ((or #f procedure?) get-position)
    ((or #f procedure?) set-position!)
    ((or #f procedure?) close))
   port?)
  (subsigs
   (read!
    (lambda ((string? string) (integer? start) (integer? count)) integer?))
   (write!
    (lambda ((string? string) (integer? start) (integer? count)) integer?))
   (get-position (lambda () opaque-port-position))
   (set-position! (lambda ((opaque-port-position position)) undefined))
   (close (lambda () undefined)))
  (desc . "Returns a newly created textual input/output port whose textual source and sink are arbitrary algorithms represented by the read! and write! procedures. Id must be a string naming the new port, provided for informational purposes only. Read! and write! must be procedures, and should behave as specified for the make-custom-textual-input-port and make-custom-textual-output-port procedures.
Each of the remaining arguments may be #f; if any of those arguments is not #f, it must be a procedure and should behave as specified in the description of make-custom-textual-input-port.")))
