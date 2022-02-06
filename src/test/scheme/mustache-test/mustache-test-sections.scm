(test-mustache "Truthy"
               '((boolean . #t))
               "\"{{#boolean}}This should be rendered.{{/boolean}}\""
               "\"This should be rendered.\"")

(test-mustache "Falsey"
               '((boolean . #f))
               "\"{{#boolean}}This should not be rendered.{{/boolean}}\""
               "\"\"")

;; "Null is falsey" test is skipped; no meaningful value for null

(test-mustache "Context"
               '((context . ((name . "Joe"))))
               "\"{{#context}}Hi {{name}}.{{/context}}\""
               "\"Hi Joe.\"")

(test-mustache "Parent contexts"
               '((a . "foo")
                 (b . "wrong")
                 (sec . ((b . "bar")))
                 (c . ((d . "baz"))))
               "\"{{#sec}}{{a}}, {{b}}, {{c.d}}{{/sec}}\""
               "\"foo, bar, baz\"")

(test-mustache "Variable test"
               '((foo . "bar"))
               "\"{{#foo}}{{.}} is {{foo}}{{/foo}}\""
               "\"bar is bar\"")

(test-mustache "List Contexts"
               '((tops . #(((tname . ((upper . "A")
                                      (lower . "a")))
                            (middles . #(((mname . "1")
                                          (bottoms . #(((bname . "x"))
                                                       ((bname . "y")))))))))))
               "{{#tops}}{{#middles}}{{tname.lower}}{{mname}}.{{#bottoms}}{{tname.upper}}{{mname}}{{bname}}.{{/bottoms}}{{/middles}}{{/tops}}"
               "a1.A1x.A1y.")

(test-mustache "Deeply Nested Contexts"
               '((a . ((one . 1)))
                 (b . ((two . 2)))
                 (c . ((three . 3)
                       (d . ((four . 4)
                             (five . 5))))))
               "
               {{#a}}
               {{one}}
               {{#b}}
               {{one}}{{two}}{{one}}
               {{#c}}
               {{one}}{{two}}{{three}}{{two}}{{one}}
               {{#d}}
               {{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
               {{#five}}
               {{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}
               {{one}}{{two}}{{three}}{{four}}{{.}}6{{.}}{{four}}{{three}}{{two}}{{one}}
               {{one}}{{two}}{{three}}{{four}}{{five}}{{four}}{{three}}{{two}}{{one}}
               {{/five}}
               {{one}}{{two}}{{three}}{{four}}{{three}}{{two}}{{one}}
               {{/d}}
               {{one}}{{two}}{{three}}{{two}}{{one}}
               {{/c}}
               {{one}}{{two}}{{one}}
               {{/b}}
               {{one}}
               {{/a}}
               "
               "
               1
               121
               12321
               1234321
               123454321
               12345654321
               123454321
               1234321
               12321
               121
               1
               "
               )

(test-mustache "List"
               '((list . #(((item . 1))
                           ((item . 2))
                           ((item . 3)))))
               "\"{{#list}}{{item}}{{/list}}\""
               "\"123\"")

                        (test-mustache "Empty List"
'((list . #()))
"\"{{#list}}Yay lists!{{/list}}\""
"\"\"")

      (test-mustache "Doubled"
                     '((bool . #t) (two . "second"))
                     "
                     {{#bool}}
                     * first
                     {{/bool}}
                     * {{two}}
                     {{#bool}}
                     * third
                     {{/bool}}
                     "

                     "
                     * first
                     * second
                     * third
                     "
                     )

(test-mustache "Nested (Truthy)"
               '((bool . #t))
               "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"
               "| A B C D E |"
               )

(test-mustache "Nested (Falsey)"
               '((bool . #f))
               "| A {{#bool}}B {{#bool}}C{{/bool}} D{{/bool}} E |"
               "| A  E |"
               )

(test-mustache "Context Misses"
               '()
               "[{{#missing}}Found key 'missing'!{{/missing}}]"
               "[]")

(test-mustache "Implicit Iterator - String"
               '((list . #("a" "b" "c" "d" "e")))
               "\"{{#list}}({{.}}){{/list}}\""
               "\"(a)(b)(c)(d)(e)\"")

(test-mustache "Implicit Iterator - Integer"
               '((list . #(1 2 3 4 5)))
               "\"{{#list}}({{.}}){{/list}}\""
               "\"(1)(2)(3)(4)(5)\"")

(test-mustache "Implicit Iterator - Decimal"
               '((list . #(1.10 2.20 3.30 4.40 5.50)))
               "\"{{#list}}({{.}}){{/list}}\""
               "\"(1.1)(2.2)(3.3)(4.4)(5.5)\"")

(test-mustache "Implicit Iterator - Array"
               '((list . #(#(1 2 3) #("a" "b" "c"))))
               "\"{{#list}}({{#.}}{{.}}{{/.}}){{/list}}\""
               "\"(123)(abc)\"")

(test-mustache "Dotted Names - Truthy"
               '((a . ((b . ((c . #t))))))
               "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"Here\""
               "\"Here\" == \"Here\""
               )

(test-mustache "Dotted Names - Falsey"
               '((a . ((b . ((c . #f))))))
               "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\""
               "\"\" == \"\""
               )

(test-mustache "Dotted Names - Broken Chains"
               '((a . ()))
               "\"{{#a.b.c}}Here{{/a.b.c}}\" == \"\""
               "\"\" == \"\""
               )

(test-mustache "Surrounding Whitespace"
               '((boolean . #t))
               " | {{#boolean}}\t|\t{{/boolean}} | \n"
               " | \t|\t | \n"
               )

(test-mustache "Internal Whitespace"
               '((boolean . #t))
               " | {{#boolean}} {{! Important Whitespace }}\n {{/boolean}} | \n"
               " |  \n  | \n"
               )

(test-mustache "Indented Inline Sections"
               '((boolean . #t))
               " {{#boolean}}YES{{/boolean}}\n {{#boolean}}GOOD{{/boolean}}\n"
               " YES\n GOOD\n"
               )

(test-mustache "Standalone Lines"
               '((boolean . #t))
               "
               |
               | This Is
               {{#boolean}}
               |
               {{/boolean}}
               | A Line
               "
               "
               |
               | This Is
               |
               | A Line
               "
               )

(test-mustache "Indented Standalone Lines"
               '((boolean . #t))
               "
               |
               | This Is
               {{#boolean}}
               |
               {{/boolean}}
               | A Line
               "

               "
               |
               | This Is
               |
               | A Line
               "

               )

(test-mustache "Standalone Line Endings"
               '((boolean . #t))
               "|\r\n{{#boolean}}\r\n{{/boolean}}\r\n|"
               "|\r\n|"
               )
(test-mustache "Standalone Without Previous Line"
               '((boolean . #t))
               "  {{#boolean}}\n#{{/boolean}}\n/"
               "#\n/"
               )

(test-mustache "Standalone Without Newline"
               '((boolean . #t))
               "#{{#boolean}}\n/\n  {{/boolean}}"
               "#\n/\n"
               )

(test-mustache "Padding"
               '((boolean . #t))
               "|{{# boolean }}={{/ boolean }}|"
               "|=|"
               )
