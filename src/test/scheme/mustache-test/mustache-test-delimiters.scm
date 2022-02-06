(test-mustache "Pair Behavior"
               '((text . "Hey!"))
               "{{=<% %>=}}(<%text%>)"
               "(Hey!)")

(test-mustache "Special Characters"
               '((text . "It worked!"))
               "({{=[ ]=}}[text])"
               "(It worked!)")

(test-mustache "Sections"
               '((section . #t)
                 (data . "I got interpolated."))
               "[\n{{#section}}\n  {{data}}\n  |data|\n{{/section}}\n\n{{= | | =}}\n|#section|\n  {{data}}\n  |data|\n|/section|\n]\n"
               "[\n  I got interpolated.\n  |data|\n\n  {{data}}\n  I got interpolated.\n]\n")

(test-mustache "Inverted Sections"
               '((section . #f)
                 (data . "I got interpolated."))
               "[\n{{^section}}\n  {{data}}\n  |data|\n{{/section}}\n\n{{= | | =}}\n|^section|\n  {{data}}\n  |data|\n|/section|\n]\n"
               "[\n  I got interpolated.\n  |data|\n\n  {{data}}\n  I got interpolated.\n]\n")

(test-mustache "Partial Inheritence"
               '((value . "yes"))
               '(("include" . ".{{value}}."))
               "[ {{>include}} ]\n{{= | | =}}\n[ |>include| ]\n"
               "[ .yes. ]\n[ .yes. ]\n")

(test-mustache "Post-Partial Behavior"
               '((value . "yes"))
               '(("include" . ".{{value}}. {{= | | =}} .|value|."))
               "[ {{>include}} ]\n[ .{{value}}.  .|value|. ]\n"
               "[ .yes.  .yes. ]\n[ .yes.  .|value|. ]\n")

(test-mustache "Surrounding Whitespace"
               '()
               "| {{=@ @=}} |"
               "|  |")

(test-mustache "Outlying Whitespace (Inline)"
               '()
               " | {{=@ @=}}\n"
               " | \n")

(test-mustache "Standalone Tag"
               '()
               "Begin.\n{{=@ @=}}\nEnd.\n"
               "Begin.\nEnd.\n")

(test-mustache "Indented Standalone Tag"
               '()
               "Begin.\n  {{=@ @=}}\nEnd.\n"
               "Begin.\nEnd.\n")

(test-mustache "Standalone Line Endings"
               '()
               "|\r\n{{= @ @ =}}\r\n|"
               "|\r\n|")

(test-mustache "Standalone Without Previous Line"
               '()
               "  {{=@ @=}}\n="
               "=")

(test-mustache "Standalone Without Newline"
               '()
               "=\n  {{=@ @=}}"
               "=\n")

(test-mustache "Pair with Padding"
               '()
               "|{{= @   @ =}}|"
               "||")
