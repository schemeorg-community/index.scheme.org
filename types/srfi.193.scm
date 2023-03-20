(((name . "command-line")
  (signature lambda () list?)
  (tags pure)
  (subsigs
    (return (list string?)))
  (desc . "This procedure is equivalent to the R6RS and R7RS command-line procedure, but specified in more detail.
R6RS definition: \"Returns a nonempty list of strings. The first element is an implementation-specific name for the running top-level program. The remaining elements are command-line arguments according to the operating system’s conventions.\"
R7RS definition: \"Returns the command line passed to the process as a list of strings. The first string corresponds to the command name, and is implementation-dependent. It is an error to mutate any of these strings.\"
Additional stipulations by this SRFI:
* If the calling program is not a command in this SRFI’s terminology, a list equal to (\"\") is returned.
* If the calling program is a standalone executable, the command line is equal to the command line of the operating system process, except that arguments belonging to the Scheme runtime system are omitted. (Such arguments start with -: in Gambit and Chicken.)
* If the calling program is a script as well as a command, the command name is equal to the filename given to load, on the command line, in an environment variable, etc. The command args are the args (if any) that belong to the script.
* Otherwise, if the command name comes from a filename, the filename is preserved as given to the implementation.
* It is an error to mutate a (command-line) list returned by the implementation, or any of the strings in it.
* Implementations are encouraged to have command-line as a parameter object. In that case, the implementation binds the value of the parameter as above for the duration of the command. The program may freely rebind command-line to any other string list containing at least one element, either temporarily with parameterize or permanently.
* If a program rebinds the command-line parameter, the new binding may share structure with the old binding."))
 ((name . "command-name")
  (signature lambda () (or string? #f))
  (tags pure)
  (subsigs
    (return (list string?)))
  (desc . "Returns a friendly version of (car (command-line)) evaluated in the current lexical environment.
If (car (command-line)) is a zero-length string, #f is returned to indicate \"not a command\".
Otherwise a friendly command name is typically derived from a filename as follows:
* The directory part (if any) is removed.
* Filename extensions known to belong to executable files or Scheme scripts on the underlying operating system are removed at the discretion of the implementation. For example, .exe or .scm.
* Other changes may also be made according to local conventions.

For example, both the Windows filename C:\\Program Files\\Fantastic Scheme\\fantastic-scheme-1.0.EXE and the Unix filename /usr/local/bin/fantastic-scheme-1.0 would be typically shortened to fantastic-scheme-1.0."))
 ((name . "command-args")
  (signature lambda () list?)
  (tags pure)
  (subsigs
    (return (list string?)))
  (desc . "Returns (cdr (command-line)) evaluated in the current lexical environment."))
 ((name . "script-file")
  (signature lambda () (or string? #f))
  (tags pure)
  (desc . "Returns an absolute pathname pointing to the calling script. Symbolic links are not resolved.
(The script may or may not be a command; use command-name to find out.)
If the calling program is not a script, #f is returned.
Implementations must resolve the absolute pathname of a script before running that script. The script may change the working directory, thereby changing the interpretation of relative pathnames."))
 ((name . "script-directory")
  (signature lambda () (or string? #f))
  (tags pure)
  (desc . "Returns only the non-filename part of script-file as a string. As with script-file, this is an absolute pathname.
The string should end with a directory separator (a forward slash on Unix; a backslash on Windows; an appropriate character on other operating systems) so that string-append can be easily used to build pathnames based on it: for example, (string-append (script-directory) \"my-data-file\"). However, if appending such a separator would make the pathname invalid on the underlying operating system, the separator is not added.
If the calling program is not a script, #f is returned.")))
