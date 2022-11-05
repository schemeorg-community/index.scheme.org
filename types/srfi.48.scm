(((name . "format")
  (signature
   case-lambda
   (((string? format-string) obj ...) string?)
   (((#f port) (string? format-string) obj ...) string?)
   (((output-port? port) (string? format-string) obj ...) undefined))
  (spec-values
   (format-string
    ("~a" "Mnemonic: Any. Action: (display obj) for humans. Consumes: yes")
    ("~s"
     "Mnemonic: Slashified. Action: (write obj) for parsers. Consumes: yes")
    ("~w"
     "Mnemonic: WriteCircular. Action: (write-with-shared-structure obj) like ~s, but handles recursive structures. Consumes: yes")
    ("~d"
     "Mnemonic: Decimal. Action: the obj is a number which is output in decimal radix. Consumes: yes")
    ("~x"
     "Mnemonic: heXadecimal. Action: the obj is a number which is output in hexdecimal radix. Consumes: yes")
    ("~o"
     "Mnemonic: Octal. Action: the obj is a number which is output in octal radix. Consumes: yes")
    ("~b"
     "Mnemonic: Binary. Action: the obj is a number which is output in binary radix. Consumes: yes")
    ("~c"
     "Mnemonic: Character. Action: the single charater obj is output by write-char. Consumes: yes")
    ("~y"
     "Mnemonic: Yuppify. Action: the list obj is pretty-printed to the output. Consumes: yes")
    ("~?"
     "Mnemonic: Indirection. Action: the obj is another format-string and the following obj is a list of arguments; format is called recursively. Consumes: yes")
    ("~K"
     "Mnemonic: Indirection. Action: the same as ~? for backward compatability with some existing implementations. Consumes: yes")
    ("~[w[,d]]F"
     "Mnemonic: Fixed. Action: ~w,dF outputs a number with width w and d digits after the decimal; ~wF outputs a string or number with width w. Consumes: yes")
    ("~~" "Mnemonic: Tilde. Action: output a tilde. Consumes: no")
    ("~t" "Mnemonic: Tab. Action: output a tab character. Consumes: no")
    ("~%"
     "Mnemonic: Newline. Action: output a newline character. Consumes: no")
    ("~&"
     "Mnemonic: Freshline. Action: output a newline character if it is known that the previous output was not a newline. Consumes: no")
    ("~_"
     "Mnemonic: Space. Action: a single space character is output. Consumes: no")
    ("~h"
     "Mnemonic: Help. Action: outputs one line of call synopsis, one line of comment, and one line of synopsis for each format directive, starting with the directive (e.g. \"~t\"). Consumes: no")))))
