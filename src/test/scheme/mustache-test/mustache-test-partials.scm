(test-mustache "Basic Behavior"
               '()
               '(("text" . "from partial"))
               "\"{{>text}}\""
               "\"from partial\"")

(test-mustache "Failed Lookup"
               '()
               '()
               "\"{{>text}}\""
               "\"\"")

(test-mustache "Context"
               '((text . "content"))
               '(("partial" . "*{{text}}*"))
               "\"{{>partial}}\""
               "\"*content*\"")

(test-mustache "Recursion"
               '((content . "X")
                 (nodes . #(((content . "Y")
                             (nodes . #())))))
               '(("node" . "{{content}}<{{#nodes}}{{>node}}{{/nodes}}>"))
               "{{>node}}"
               "X<Y<>>")

(test-mustache "Surrounding Whitespace"
               '()
               '(("partial" . "\t|\t"))
               "| {{>partial}} |"
               "| \t|\t |")

(test-mustache "Inline Indentation"
               '((data . "|"))
               '(("partial" . ">\n>"))
               "  {{data}}  {{> partial}}\n"
               "  |  >\n>\n")

(test-mustache "Standalone Line Endings"
               '()
               '(("partial" . ">"))
               "|\r\n{{>partial}}\r\n|"
               "|\r\n>|")
