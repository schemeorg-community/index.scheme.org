(test-mustache "Falsey"
               '((boolean . #f))
               "\"{{^boolean}}This should be rendered.{{/boolean}}\""
               "\"This should be rendered.\"")

(test-mustache "Truthy"
               '((boolean . #t))
               "\"{{^boolean}}This should not be rendered.{{/boolean}}\""
               "\"\"")

;; "Null is falsey" test is skipped; no meaningful value for null

(test-mustache "Context"
               '((context . ((name . "Joe"))))
               "\"{{^context}}Hi {{name}}.{{/context}}\""
               "\"\"")

(test-mustache "List"
               '(list . #(((n . 1))
                          ((n . 2))
                          ((n . 3))))
               "\"{{^list}}{{n}}{{/list}}\""
               "\"\"")

(test-mustache "Empty List"
               '(list . #())
               "\"{{^list}}Yay lists!{{/list}}\""
               "\"Yay lists!\"")

(test-mustache "Doubled"
               '((bool . #f) (two . "second"))
               "
               {{^bool}}
               * first
               {{/bool}}
               * {{two}}
               {{^bool}}
               * third
               {{/bool}}
               "
               "
               * first
               * second
               * third
               ")

(test-mustache "Nested (Falsey)"
               '((bool . #f))
               "| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |"
               "| A B C D E |")

(test-mustache "Nested (Truthy)"
               '((bool . #t))
               "| A {{^bool}}B {{^bool}}C{{/bool}} D{{/bool}} E |"
               "| A  E |")

(test-mustache "Context Misses"
               '(())
               "[{{^missing}}Cannot find key 'missing'!{{/missing}}]"
               "[Cannot find key 'missing'!]")

(test-mustache "Dotted Names - Truthy"
               '((a . ((b . ((c . #t))))))
               "\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"\""
               "\"\" == \"\"")

(test-mustache "Dotted Names - Falsey"
               '((a . ((b . ((c . #f))))))
               "\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"Not Here\""
               "\"Not Here\" == \"Not Here\"")

(test-mustache "Dotted Names - Broken Chains"
               '((a . ()))
               "\"{{^a.b.c}}Not Here{{/a.b.c}}\" == \"Not Here\""
               "\"Not Here\" == \"Not Here\"")

(test-mustache "Surrounding Whitespace"
               '((boolean . #f))
               " | {{^boolean}}\t|\t{{/boolean}} | \n"
               " | \t|\t | \n")

(test-mustache "Internal Whitespace"
               '((boolean . #f))
               " | {{^boolean}} {{! Important Whitespace }}\n {{/boolean}} | \n"
               " |  \n  | \n")

(test-mustache "Indented Inline Sections"
               '((boolean . #f))
               " {{^boolean}}NO{{/boolean}}\n {{^boolean}}WAY{{/boolean}}\n"
               " NO\n WAY\n")

(test-mustache "Standalone Lines"
               '((boolean . #f))
               "
               |
               | This Is
               {{^boolean}}
               |
               {{/boolean}}
               | A Line
               "
               "
               |
               | This Is
               |
               | A Line
               ")

(test-mustache "Standalone Indented Lines"
               '((boolean . #f))
               "
               |
               | This Is
                  {{^boolean}}
               |
                  {{/boolean}}
               | A Line
               "
               "
               |
               | This Is
               |
               | A Line
               ")

(test-mustache "Standalone Line Endings"
               '((boolean . #f))
               "|\r\n{{^boolean}}\r\n{{/boolean}}\r\n|"
               "|\r\n|")

(test-mustache "Standalone Without Previous Line"
               '((boolean . #f))
               "  {{^boolean}}\n^{{/boolean}}\n/"
               "^\n/")

(test-mustache "Standalone Without Newline"
               '((boolean . #f))
               "^{{^boolean}}\n/\n  {{/boolean}}"
               "^\n/\n")

(test-mustache "Padding"
               '((boolean . #f))
               "|{{^ boolean }}={{/ boolean }}|"
               "|=|")
