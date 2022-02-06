(test-mustache "No Interpolation"
               '()
               "Hello from {Mustache}!"
               "Hello from {Mustache}!")

(test-mustache "Basic Interpolation"
               '((subject . "world"))
               "Hello, {{subject}}!"
               "Hello, world!")

(test-mustache "HTML Escaping"
               '((forbidden . "& \" < >"))
               "These characters should be HTML escaped: {{forbidden}}"
               "These characters should be HTML escaped: &amp; &quot; &lt; &gt;")

(test-mustache "Triple Mustache"
               '((forbidden . "& \" < >"))
               "These characters should not be HTML escaped: {{{forbidden}}}"
               "These characters should not be HTML escaped: & \" < >")

(test-mustache "Ampersand"
               '((forbidden . "& \" < >"))
               "These characters should not be HTML escaped: {{&forbidden}}"
               "These characters should not be HTML escaped: & \" < >")

(test-mustache "Basic Integer Interpolation"
               '((mph . 85))
               "\"{{mph}} miles an hour!\""
               "\"85 miles an hour!\"")

(test-mustache "Triple Mustache Integer Interpolation"
               '((mph . 85))
               "\"{{{mph}}} miles an hour!\""
               "\"85 miles an hour!\"")

(test-mustache "Ampersand Mustache Integer Interpolation"
               '((mph . 85))
               "\"{{&mph}} miles an hour!\""
               "\"85 miles an hour!\"")

(test-mustache "Basic Decimal Interpolation"
               '((power . 1.210))
               "\"{{power}} jiggawatts!\""
               "\"1.21 jiggawatts!\"")

(test-mustache "Triple Mustache Decimal Interpolation"
               '((power . 1.210))
               "\"{{{power}}} jiggawatts!\""
               "\"1.21 jiggawatts!\"")

(test-mustache "Ampersand Mustache Decimal Interpolation"
               '((power . 1.210))
               "\"{{&power}} jiggawatts!\""
               "\"1.21 jiggawatts!\"")

(test-mustache "Basic Null Interpolation"
               '((cannot . #f))
               "I ({{cannot}}) be seen!"
               "I () be seen!")

(test-mustache "Triple Mustache Null Interpolation"
               '((cannot . #f))
               "I ({{{cannot}}}) be seen!"
               "I () be seen!")

(test-mustache "Ampersand Null Interpolation"
               '((cannot . #f))
               "I ({{&cannot}}) be seen!"
               "I () be seen!")

(test-mustache "Basic Context Miss Interpolation"
               '()
               "I ({{cannot}}) be seen!"
               "I () be seen!")

(test-mustache "Triple Mustache Context Miss Interpolation"
               '()
               "I ({{{cannot}}}) be seen!"
               "I () be seen!")

(test-mustache "Ampersand Context Miss Interpolation"
               '()
               "I ({{&cannot}}) be seen!"
               "I () be seen!")

(test-mustache "Dotted Names - Basic Interpolation"
               '((person . ((name . "Joe"))))
               "\"{{person.name}}\" == \"{{#person}}{{name}}{{/person}}\""
               "\"Joe\" == \"Joe\"")

(test-mustache "Dotted Names - Triple Mustache Interpolation"
               '((person . ((name . "Joe"))))
               "\"{{{person.name}}}\" == \"{{#person}}{{{name}}}{{/person}}\""
               "\"Joe\" == \"Joe\"")

(test-mustache "Dotted Names - Ampersand Interpolation"
               '((person . ((name . "Joe"))))
               "\"{{&person.name}}\" == \"{{#person}}{{&name}}{{/person}}\""
               "\"Joe\" == \"Joe\"")

(test-mustache "Dotted Names - Arbitrary Depth"
               '((a . ((b . ((c . ((d . ((e . ((name . "Phil"))))))))))))
               "\"{{a.b.c.d.e.name}}\" == \"Phil\""
               "\"Phil\" == \"Phil\"")

(test-mustache "Dotted Names - Broken Chains"
               '((a . ()))
               "\"{{a.b.c}}\" == \"\""
               "\"\" == \"\"")

(test-mustache "Dotted Names - Broken Chain Resolution"
               '((a . ((b . ())))
                 (c . ((name . "Jim"))))
               "\"{{a.b.c.name}}\" == \"\""
               "\"\" == \"\"")

(test-mustache "Dotted Names - Initial Resolution"
               '((a . ((b . ((c . ((d . ((e . ((name . "Phil")))))))))))
                 (b . ((c . ((d . ((e . ((name . "Wrong"))))))))))
               "\"{{#a}}{{b.c.d.e.name}}{{/a}}\" == \"Phil\""
               "\"Phil\" == \"Phil\"")

(test-mustache "Dotted Names - Context Precedence"
               '((a . ((b . ())))
                 (b . ((c . "ERROR"))))
               "{{#a}}{{b.c}}{{/a}}"
               "")

(test-mustache "Implicit Iterators - Basic Interpolation"
               "world"
               "Hello, {{.}}!"
               "Hello, world!")

(test-mustache "Implicit Iterators - HTML Escaping"
               "& \" < >"
               "These characters should be HTML escaped: {{.}}"
               "These characters should be HTML escaped: &amp; &quot; &lt; &gt;")

(test-mustache "Implicit Iterators - Triple Mustache"
               "& \" < >"
               "These characters should not be HTML escaped: {{{.}}}"
               "These characters should not be HTML escaped: & \" < >")

(test-mustache "Implicit Iterators - Ampersand"
               "& \" < >"
               "These characters should not be HTML escaped: {{&.}}"
               "These characters should not be HTML escaped: & \" < >")

(test-mustache "Implicit Iterators - Basic Integer Interpolation"
               85
               "\"{{.}} miles an hour!\""
               "\"85 miles an hour!\"")

(test-mustache "Interpolation - Surrounding Whitespace"
               '((string . "---"))
               "| {{string}} |"
               "| --- |")

(test-mustache "Triple Mustache - Surrounding Whitespace"
               '((string . "---"))
               "| {{{string}}} |"
               "| --- |")

(test-mustache "Ampersand - Surrounding Whitespace"
               '((string . "---"))
               "| {{&string}} |"
               "| --- |")

(test-mustache "Interpolation - Standalone"
               '((string . "---"))
               "  {{string}}\n"
               "  ---\n")

(test-mustache "Triple Mustache - Standalone"
               '((string . "---"))
               "  {{{string}}}\n"
               "  ---\n")

(test-mustache "Ampersand - Standalone"
               '((string . "---"))
               "  {{&string}}\n"
               "  ---\n")

(test-mustache "Interpolation With Padding"
               '((string . "---"))
               "|{{ string }}|"
               "|---|")

(test-mustache "Triple Mustache With Padding"
               '((string . "---"))
               "|{{{ string }}}|"
               "|---|")

(test-mustache "Ampersand With Padding"
               '((string . "---"))
               "|{{& string }}|"
               "|---|")
