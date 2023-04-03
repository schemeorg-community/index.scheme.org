(((name . "list-case")
  (signature syntax-rules ()
             ((_ expression list-case-clause ...) *))
  (subsigs
    (list-case-clause
      (pattern ((variable . variable) body)
               (() body)
               (variable body))))
  (desc . "A list-case expression is evaluated as follows: The ⟨expression⟩ is evaluated. The further evaluation then depends on the type of the result:
If the result is a pair and a clause of the form [(⟨variable1⟩ . ⟨variable2⟩) ⟨body⟩] is present, the ⟨variables⟩ are bound to locations holding the car and the cdr of the pair, the ⟨body⟩ is evaluated and its results returned.
If the result is () and clause of the form [() ⟨body⟩] is present, the ⟨body⟩ is evaluated and its results returned.
If the result is neither a pair nor () and a clause of the form [⟨variable⟩ ⟨body⟩] is present, the ⟨variable⟩ is bound to a location holding the result, the ⟨body⟩ is evaluated and its results returned.
If no corresponding clause is present, an exception of type &assertion-violation is raised.
The region of the bindings consists of the corresponding ⟨body⟩. If _ appears in place of a ⟨variable⟩, the corresponding location is not bound to any variable.
If the list-case expression is in tail context, the ⟨bodies⟩ are in tail context as well.")))
