(((name . "CAT")
  (signature lambda (object formatting-specifier ...) string?)
  (tags pure)
  (desc . "Each formatting-specifier should match one of following:
<suffix%>: effective only for the number type of <object>.
<suffix$>: effective for all types except the number type of <object>.
<suffix*>: effective for all types of <object>.
<object> is any Scheme object.
<exactness> is a symbol: exact or inexact.
<radix> is a symbol: binary, octal, decimal, or hexadecimal. Each radix sign except decimal is prefixed to the resulting string. The default value is decimal. If <sign> is a symbol that takes the form of 'sign, and <object> is a positive number without a positive sign, the positive sign is prefixed to the resulting string.
<precision> is an inexact integer whose absolute value specifies the number of decimal digits after a decimal point. If <precision> is a non-negative integer, an exact sign is prefixed to the resulting string as needed.
<separator> is a list whose first element is a character serving as a separator and second element is a positive exact integer. If the integer is n, the resulting string is separated in every n-characters of the resulting string. When the integer is omitted, the default value is 3.
<writer> is a procedure of two arguments; <object> and a string port. It writes <object> to the string port. The default value of <writer> is varied according to the type of <object>. When <object> is a self-evaluating constant, it becomes equivalent to DISPLAY procedure, otherwise, it becomes WRITE procedure. If you want any objects to be displayed in your own way, you have to define your own <writer>. Otherwise, they are displayed simply in their evaluated forms.
<pipe> is a list which is composed of one or more procedures. Each procedure takes at least one string argument and returns a string. One procedure connects with another as a pipe.
<take> is a list whose elements are two exact integers; n and m, and the absolute values of n and m are N and M, respectively. First, the resulting string takes from the left n-characters, if it is non-negative, or all the characters but N-characters, if negative. Second, it takes from the right m-characters, if it is non-negative, or all the characters but M-characters, if negative. Then, it concatenates two set of characters taken. The second element can be omitted. If omitted, the default value is 0.
<converter> is a pair whose car value is a predicate procedure that checks whether <object> satisfies it, and cdr value is a procedure that takes the <object> as an argument and returns a string. When <object> satisfies the predicate procedure, all optional arguments are ineffective except <width>, <char>, <port>, and <string>.
<width> is an exact integer whose absolute value specifies the width of the resulting string. When the resulting string has fewer characters than the absolute value of <width>, it is placed rightmost with the rest being padded with <char>s, if <width> is positive, or it is placed leftmost with the rest being padded with <char>s, if <width> is negative. On the other hand, when the resulting string has more characters than the absolute value of <width>, the <width> is ignored. The default value is 0.
<char> is a padding character. The default value is #\\space.
<port> is an output port or a boolean. If an output port is specified, the resulting string and <string>s are output into that port and simultaneously returned as a string. If <port> is #t, the output port is current output port. If <port> is #f, the output is only returned as a string. The default value is #f.
<string> is a string that is appended to the resulting string.

The order of all optional arguments does not matter. The CAT procedure processes optional arguments in the following order; <exactness>, <radix>, <precision>, <separator>, <sign> for the number type of <object>, or in the following order; <writer>, <pipe>, <take> for all other types.")))
