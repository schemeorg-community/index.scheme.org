(((name . "format")
  (signature
   case-lambda
   (((string? format-string) obj ...) string?)
   (((#f port) (string? format-string) obj ...) string?)
   (((output-port? port) (string? format-string) obj ...) undefined))
  (desc . "Accepts a format template (a Scheme String), and processes it, replacing any format directives in order with one or more characters, the characters themselves dependent on the semantics of the format directive encountered. Each directive may consume one obj. It is an error if fewer or more obj values are provided than format directives that require them.
When a port is specified it must be either an output port or a boolean. If an output-port is specified, the formatted output is output into that port. If the port argument is #t, output is to the current-output-port. If the port is #f or no port is specified, the output is returned as a string. If the port is specified and is #t or an output-port, the result of the format function is unspecified.
It is unspecified which encoding is used (e.g. ASCII, EBCDIC, UNICODE). A given implementation must specify which encoding is used. The implementation may or may not allow the encoding to be selected or changed.
It is an error if an format directive consumes an obj argument and that argument does not confirm to a required type as noted in the table below.
It is permissible, but highly discouraged, to implement pretty-print as (define pretty-print write).
An format directive is a two character sequence in the string where the first character is a tilde '~'. Directive characters are case-independent, i.e. upper and lower case characters are interpreted the same. Each directive code's meaning is described in the following table:
    DIRECTIVE 	MNEMONIC 	ACTION 	CONSUMES?	
    ~a 	Any 	(display obj) for humans 	yes
    ~s 	Slashified 	(write obj) for parsers 	yes
    ~w 	WriteCircular 	(write-with-shared-structure obj) like ~s, but handles recursive structures 	yes
    ~d 	Decimal 	the obj is a number which is output in decimal radix 	yes
    ~x 	heXadecimal 	the obj is a number which is output in hexdecimal radix 	yes
    ~o 	Octal 	the obj is a number which is output in octal radix 	yes
    ~b 	Binary 	the obj is a number which is output in binary radix 	yes
    ~c 	Character 	the single charater obj is output by write-char 	yes
    ~y 	Yuppify 	the list obj is pretty-printed to the output 	yes
    ~? 	Indirection 	the obj is another format-string and the following obj is a list of arguments; format is called recursively 	yes
    ~K 	Indirection 	the same as ~? for backward compatability with some existing implementations 	yes
    ~[w[,d]]F 	Fixed 	~w,dF outputs a number with width w and d digits after the decimal; ~wF outputs a string or number with width w. 	yes
    ~~ 	Tilde 	output a tilde 	no
    ~t 	Tab 	output a tab character 	no
    ~% 	Newline 	output a newline character 	no
    ~& 	Freshline 	output a newline character if it is known that the previous output was not a newline 	no
    ~_ 	Space 	a single space character is output 	no
    ~h 	Help 	outputs one line of call synopsis, one line of comment, and one line of synopsis for each format directive, starting with the directive (e.g. \"~t\") 	no

The ~F, fixed format, directive requires some elucidation.
~wF is useful for strings or numbers. Where the string (or number->string of the number) has fewer characters than the integer width w, the string is padded on the left with space characters.
~w,dF is typically used only on numbers. For strings, the d specifier is ignored. For numbers, the integer d specifies the number of decimal digits after the decimal place. Both w and d must be zero or positive.
If d is specified, the number is processed as if added to 0.0, i.e. it is converted to an inexact value.
(format \"~8,2F\" 1/3) => \"    0.33\"
If no d is specified, the number is not coerced to inexact.
(format \"~6F\" 32) => \"    32\"
Digits are padded to the right with zeros
(format \"~8,2F\" 32) => \"   32.00\"
If the number it too large to fit in the width specified, a string longer than the width is returned
(format \"~1,2F\" 4321) => \"4321.00\"
If the number is complex, d is applied to both real and imaginal parts
(format \"~1,2F\" (sqrt -3.9)) => \"0.00+1.97i\"

For very large or very small numbers, the point where exponential notation is used is implementation defined.
(format \"~8F\" 32e5) => \"   3.2e6\" or \"3200000.0\"")))
