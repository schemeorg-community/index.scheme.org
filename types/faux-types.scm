(
 ;; Standard predicate-less types
 (environment . "Environment (set of libraries and their identifiers,) as per R7RS section 6.12")
 (file-options . "Object representing special flags for file handling and I/O (R6RS)")
 (codec . "An immutable Scheme object that represents a Unicode or similar encoding scheme (R6RS)")
 (transcoder . "A transcoder is an immutable Scheme object that combines a codec with an end-of-line style and a method for handling decoding errors (R6RS)")
 (enum-set . "Finite set of enumerated values of a certain type (R6RS)")
 (opaque-port-position . "Position in a port (R6RS)")
 (transformer-spec . "Object returned by macro-creating forms")
 ;; Procedure types
 (predicate . "A procedure that returns a (usually) boolean value for whether its argument has some property")
 (comparison . "A procedure that compares its two arguments, usually for ordering")
 (equality . "A procedure that compares its two arguments for being similar (in some sense)")
 ;; SRFI-specific types
 (computation . "A computation can be thought of as suspended code much like promises but with the difference that a computation can be executed more than once and that its results are not cached (SRFI-165)")
 (computation-env . "The computations of the environment monad are executed on an environment (SRFI-165)")
 (computation-env-variable . "Environment variables can be bound to values in computation environments (SRFI-165)")
 (constructor-descriptor . "Record constructor descriptor specifies a record constructor (or constructor for short), that can be used to construct record values of the type specified by rtd (R6RS)")
 (endianness . "(Big, little) endianness object, as returned by endianness macro (SRFI-74)")
 (formatter . "Formatter (encapsulated formatting code) object (SRFI-159)")
 (formatter-variable . "Formatting-related variables that can be overriden (SRFI-159)")
 (generator-macro . "Eager comprehension macro (SRFI-42)")
 (option . "Object representing a command line option passed to the program (SRFI-37)")
 (random-source-state . "Object representing a state of the random bit source (SRFI-27)")
 (timer-id . "ID of the task in a certain timer (SRFI-120)")
 (time-type . "Symbol representing a certain time system, e.g. time-utc (SRFI-19)")
 (vicinity . "Descriptor for a place in the file system (SRFI-59)")
 ;; SRFI-106 types
 ;; Technically, their type is unspecified.
 ;; However, any implementation of SRFI-106 is likely to just provide
 ;; POSIX integers. So integer? is safe.
 (message-type . "Message types, as used in SRFI-106 socket-recv and socket-send")
 (socket-domain . "Socket domain flag (SRFI-106)")
 (address-family . "Address family flag (SRFI-106)")
 (address-info . "Merged address info flags for socket (SRFI-106)")
 (ip-protocol . "Protocol flag for a socket (SRFI-106)")
 (shutdown-method . "Shutdown method for a socket (SRFI-106)"))
