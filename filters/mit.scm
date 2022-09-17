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
 ((scheme r5rs) . #t)
 ((scheme read) . #t)
 ((scheme repl) . #t)
 ((scheme time) . #t)
 ((scheme write) . #t)
 
 ; r7rs large red
 ((scheme comparator) . (srfi 128))
 ((scheme charset) . (srfi 14))
 ((scheme ephemeron) . (srfi 124))
 ((scheme hash-table) . (srfi 125))
 ((scheme list) . (srfi 1))
 ((scheme vector) . (srfi 133))
 
 ; r7rs large tangerine
 ((scheme fixnum) . (srfi 143))
 ((scheme regex) . (srfi 115))

 ;; misc srfi, added in order of popularity according to https://misc.lassi.io/2019/srfi-implementation-counts.scm
 ;; omitting anything added to r7rs small / large
 ((srfi 8) . #t)
 ((srfi 2) . #t)
 ((srfi 27) . #t)
 ((srfi 69) . #t)
 )
