(((name . "CAT")
  (signature lambda (object formatting-specifier ...) string?)
  (tags pure)
  (spec-values
   (formatting-specifier
    ("<exactness%>" "symbol: exact or inexact")
    ("<radix%>" "symbol: binary, octal, decimal, or hexadecimal")
    ("<sign%>"
     "if <sign%> is a symbol that takes the form of 'sign, and object is a positive number without a positive sign, the positive sign is prefixed to the resulting string")
    ("<precision%>"
     "inexact integer whose absolute value specifies the number of decimal digits after a decimal point. If <precision%> is a non-negative integer, an exact sign is prefixed to the resulting string as needed")
    ("<separator%>"
     "a list whose first element is a character serving as a separator and second element is a positive exact integer. If the integer is n, the resulting string is separated in every n-characters of the resulting string. When the integer is omitted, the default value is 3.")
    ("<writer$>"
     "a procedure of two arguments; object and a string port. It writes object to the string port. The default value of <writer$> is varied according to the type of object. When object is a self-evaluating constant, it becomes equivalent to DISPLAY procedure, otherwise, it becomes WRITE procedure. If you want any objects to be displayed in your own way, you have to define your own <writer$>. Otherwise, they are displayed simply in their evaluated forms")
    ("<pipe>"
     "a list which is composed of one or more procedures. Each procedure takes at least one string argument and returns a string. One procedure connects with another as a pipe")
    ("<take>"
     "a list whose elements are two exact integers; n and m, and the absolute values of n and m are N and M, respectively. First, the resulting string takes from the left n-characters, if it is non-negative, or all the characters but N-characters, if negative. Second, it takes from the right m-characters, if it is non-negative, or all the characters but M-characters, if negative. Then, it concatenates two set of characters taken. The second element can be omitted. If omitted, the default value is 0")
    ("<converter>"
     "a pair whose car value is a predicate procedure that checks whether object satisfies it, and cdr value is a procedure that takes the object as an argument and returns a string. When object satisfies the predicate procedure, all optional arguments are ineffective except <width>, <char>, <port>, and <string>")
    ("<width>"
     "an exact integer whose absolute value specifies the width of the resulting string. When the resulting string has fewer characters than the absolute value of <width>, it is placed rightmost with the rest being padded with <char>s, if <width> is positive, or it is placed leftmost with the rest being padded with <char>s, if <width> is negative. On the other hand, when the resulting string has more characters than the absolute value of <width>, the <width> is ignored. The default value is 0")
    ("<char>" "a padding character. The default value is #\\space")
    ("<port>"
     "an output port or a boolean. If an output port is specified, the resulting string and <string>s are output into that port and simultaneously returned as a string. If <port> is #t, the output port is current output port. If <port> is #f, the output is only returned as a string. The default value is #f")
    ("<string>" "a string that is appended to the resulting string")))))
