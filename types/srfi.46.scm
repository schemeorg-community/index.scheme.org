(((name . "syntax-rules")
  (signature
   syntax-rules
   (_)
   ((_ (literal ...) syntax-rule ...) transformer-spec)
   ((_ ellipsis (literal ...) syntax-rule ...) transformer-spec))
  (subsigs
   (syntax-rule (pattern (pattern template)))
   (pattern
    (pattern
     _
     identifier
     constant
     (pattern ...)
     (pattern pattern ... . pattern)
     (pattern ... pattern ellipsis pattern ...)
     (pattern ... pattern ellipsis pattern ... . pattern)
     (_append "#" (pattern ...))
     (_append "#" (pattern ... pattern ellipsis pattern ...))))
   (template
    (pattern
     identifier
     constant
     (element ...)
     (element element ... . template)
     (ellipsis template)
     (_append "#" (element ...))))
   (element (pattern template (_append template ellipsis))))
  (desc . "Syntax-rules syntax is extended so that there is an extra possible token before the literal identifier list:
(syntax-rules [<ellipsis-identifier>] (<literal-identifier> ...)
    (<pattern> <template>)
    ...)

Ellipsis-identifier specifies the token used for ellipsis. It defaults to the usual R5RS1 ellipsis identifier, ..., but it can be specified to be any identifier, such as :::. This identifier's specification is considered to be a binding whose scope is the rules of the transformer. The macro system implementation must make the hygienic arrangements described in R5RS's section 4.3 to preserve the lexical scope of these bindings.

The syntax-rules pattern language is also extended to allow 'tail patterns.' The following clauses are added to <pattern>:
 (<pattern> ... <ellipsis> <pattern> ...)
#(<pattern> ... <ellipsis> <pattern> ...)

And the following clauses are added to the semantics of syntax-rules' pattern matching:
* P is of the form (P1 ... Px-1 Px <ellipsis> Px+1 ... Py) where <ellipsis> is the identifier ... and F is a proper list of M forms such that M >= Y, the first X-1 of which match P1 through Px-1, respectively, the forms Fx through Fm-(y-x), where Fi is the Ith element of the proper list F, all match Px, and the forms Fm-(y-x) through Fm match the patterns Px+1 through Py.
* P is of the form #(P1 ... Px-1 Px <ellipsis> Px+1 ... Py) where <ellipsis> is the identifier ... and F is a vector of M forms such that M >= Y, the first X-1 of which match P1 through Px-1, respectively, the forms Fx through Fm-(y-x), where Fi is the Ith element of the vector F, all match Px and the forms Fm-(y-x) through Fm all match Px+1 through Py, respectively.")))
