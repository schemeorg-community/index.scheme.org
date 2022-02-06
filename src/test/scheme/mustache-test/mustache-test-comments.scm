(test-mustache "Inline"
               '()
               "12345{{! Comment Block! }}67890"
               "1234567890")

(test-mustache "Multiline"
               '()
               "12345{{!\n This is a\n multi-line comment...\n}}67890"
               "1234567890")

(test-mustache "Standalone"
               '()
               "Begin.\n{{! Comment Block! }}\nEnd."
               "Begin.\nEnd.")

(test-mustache "Indented Standalone"
               '()
               "Begin.\n     {{! Comment Block! }}\nEnd."
               "Begin.\nEnd.")

(test-mustache "Standalone Line Endings"
               '()
               "\r\n{{! Standalone Comment }}\r\n"
               "\r\n")

(test-mustache "Standalone Without Previous Line"
               '()
               "  {{! I'm Still Standalone }}\n!"
               "!")

(test-mustache "Standalone Without Newline"
               '()
               "!\n  {{! I'm Still Standalone }}"
               "!\n")

(test-mustache "Multiline Standalone"
               '()
               "Begin.\n{{!\nSomething's going on here...\n}}\nEnd."
               "Begin.\nEnd.")

(test-mustache "Indented Multiline Standalone"
               '()
               "Begin.\n  {{!\n    Something's going on here...\n  }}\nEnd."
               "Begin.\nEnd.")

(test-mustache "Indented Inline"
               '()
               "  12 {{! 34 }}\n"
               "  12 \n")

(test-mustache "Surrounding Whitespace"
               '()
               "12345 {{! Comment Block! }} 67890"
               "12345  67890")

