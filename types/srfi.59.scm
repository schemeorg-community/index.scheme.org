(((name . "program-vicinity")
  (signature lambda () vicinity)
  (tags pure)
  (desc . "Returns the vicinity of the currently loading Scheme code. For an interpreter this would be the directory containing source code. For a compiled system (with multiple files) this would be the directory where the object or executable files are. If no file is currently loading, then the result is undefined. Warning: program-vicinity can return incorrect values if your program escapes back into a load continuation."))
 ((name . "library-vicinity")
  (signature lambda () vicinity)
  (tags pure)
  (desc . "Returns the vicinity of the shared Scheme library."))
 ((name . "implementation-vicinity")
  (signature lambda () vicinity)
  (tags pure)
  (desc . "Returns the vicinity of the underlying Scheme implementation. This vicinity will likely contain startup code and messages and a compiler."))
 ((name . "user-vicinity")
  (signature lambda () vicinity)
  (tags pure)
  (desc . "Returns the vicinity of the current directory of the user. On most systems this is `\"\"' (the empty string)."))
 ((name . "home-vicinity") 
  (signature lambda () vicinity) 
  (tags pure)
  (desc . "Returns the vicinity of the user's HOME directory, the directory which typically contains files which customize a computer environment for a user. If scheme is running without a user (eg. a daemon) or if this concept is meaningless for the platform, then home-vicinity returns #f."))
 ((name . "in-vicinity")
  (signature lambda ((vicinity vicinity) (string? filename)) string?)
  (tags pure)
  (desc . "Returns a filename suitable for use by load, open-input-file, open-output-file, etc. The returned filename is filename in vicinity. in-vicinity should allow filename to override vicinity when filename is an absolute pathname and vicinity is equal to the value of (user-vicinity). The behavior of in-vicinity when filename is absolute and vicinity is not equal to the value of (user-vicinity) is unspecified. For most systems in-vicinity can be string-append."))
 ((name . "sub-vicinity")
  (signature lambda ((vicinity vicinity) (string? name)) vicinity)
  (tags pure)
  (desc . "Returns the vicinity of vicinity restricted to name. This is used for large systems where names of files in subsystems could conflict. On systems with directory structure sub-vicinity will return a pathname of the subdirectory name of vicinity."))
 ((name . "make-vicinity")
  (signature lambda ((string? dir-path)) vicinity)
  (tags pure)
  (desc . "Returns dirpath as a vicinity for use as first argument to in-vicinity."))
 ((name . "pathname->vicinity")
  (signature lambda ((string? path)) vicinity)
  (tags pure)
  (desc . "Returns the vicinity containing path."))
 ((name . "vicinity:suffix?")
  (signature lambda ((char? chr)) boolean?)
  (tags pure)
  (desc . "Returns the `#t' if chr is a vicinity suffix character; and #f otherwise. Typical vicinity suffixes are `/', `:', and `\\'.")))
