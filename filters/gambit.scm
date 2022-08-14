(
 ;; r7rs small
 ((scheme base) . #t)
 ((scheme case-lambda) . #t)
 ((scheme complex) . #t)
 ((scheme char) . #t)
 ((scheme cxr) . #t)
 ((scheme eval) . #t)
 ((scheme file) . #t)
 ((scheme inexact) . #t)
 ((scheme lazy) . #t)
 ((scheme load) . #t)
 ((scheme process-context) . #t)
 ((scheme read) . #t)
 ((scheme repl) . #t)
 ((scheme time) . #t)
 ((scheme write) . #t)

 ;; r7rs large red
 ((scheme box) . (srfi 111))
 ((scheme charset) . (srfi 14))
 ((scheme ephemeron) . (srfi 124))
 ((scheme generator) . (srfi 158))
 ((scheme list) . (srfi 1))
 ((scheme sort) . (srfi 132))
 ((scheme stream) . (srfi 41))

 ; r7rs large tangerine
 ((scheme bitwise) . #t) ;; aka srfi 151
 ((scheme regex) . (srfi 115))

 ;; misc srfi, added in order of popularity according to https://misc.lassi.io/2019/srfi-implementation-counts.scm
 ;; omitting anything added to r7rs small / large
 ((srfi 8) . #t)
 ((srfi 2) . #t)
 ((srfi 27) . #t)
 ((srfi 13) . #t)
 ((srfi 26) . #t)
 ((srfi 28) . #t)
 ((srfi 19) . #t)
 ((srfi 31) . #t)
 ((srfi 42) . #t)
 ((srfi 69) . #t)
 ((srfi 45) . #t)
 ((srfi 18) . #t)
 ((srfi 48) . #t)
 ((srfi 64) . #t)
)
