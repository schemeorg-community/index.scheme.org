(((name . "implementation-name")
  (signature lambda () (or string? #f))
  (tags pure)
  (desc . "Returns the name of the Scheme implementation. This procedure corresponds roughly to Common Lisp's lisp-implementation-type function."))
 ((name . "implementation-version")
  (signature lambda () (or string? #f))
  (tags pure)
  (desc . "Returns the version of the Scheme implementation. This procedure corresponds roughly to Common Lisp's lisp-implementation-version function."))
 ((name . "cpu-architecture")
  (signature lambda () (or string? #f))
  (tags pure)
  (desc . "Returns the CPU architecture, real or virtual, on which this implementation is executing. This procedure corresponds roughly to Common Lisp's machine-type function. On Posix systems, the result may be derived from the machine field of the utsname structure."))
 ((name . "machine-name") 
  (signature lambda () (or string? #f))
  (tags pure)
  (desc . "Returns a name for the particular machine on which the implementation is running. Possible values are the DNS or WINS host name, the DNS full name, an IP address in string form associated with the system, or a MAC address in string form associated with the system. This procedure corresponds roughly to Common Lisp's machine-instance function. On Posix systems, the result may be derived from the nodename field of the utsname structure."))
 ((name . "os-name") 
  (signature lambda () (or string? #f))
  (tags pure)
  (desc . "Returns a name for the operating system, platform, or equivalent on which the implementation is running. This procedure corresponds roughly to Common Lisp's software-type function. On Posix systems, the result may be derived from the sysname field of the utsname structure."))
 ((name . "os-version") 
  (signature lambda () (or string? #f))
  (tags pure)
  (desc . "Returns the version of the operating system, platform, or equivalent on which the implementation is running. This procedure corresponds roughly to Common Lisp's software-version function. On Posix systems, the result may be derived from the release and/or version fields of the utsname structure.")))
