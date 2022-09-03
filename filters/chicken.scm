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
 
 ; r7rs large red
 ((scheme box) . (srfi 111)) ;; aka srfi 111
 ((scheme comparator) . (srfi 128)) ;; aka srfi 128
 ((scheme charset) . (srfi 14)) ;; aka srfi 14
 ((scheme generator) . (srfi 158)) ;; aka srfi 121 / 158
 ((scheme ideque) . (srfi 134)) ;; aka srfi 134
 ((scheme ilist) . (srfi 116)) ;; aka srfi 116
 ((scheme list) . (srfi 1)) ;; aka srfi 1
 ((scheme list-queue) . (srfi 117)) ;; aka srfi 117
 ((scheme lseq) . (srfi 127)) ;; aka srfi 127
 ((scheme rlist) . (srfi 101)) ;; aka srfi 101
 ((scheme set) . (srfi 113)) ;; aka srfi 113
 ((scheme stream) . (srfi 41)) ;; aka srfi 41
 ((scheme sort) . (srfi 132)) ;; aka srfi 132
 ((scheme text) . (srfi 135)) ;; aka srfi 135
 ((scheme vector) . (srfi 133)) ;; aka srfi 133
 
 ; r7rs large tangerine
 ((scheme bitwise) . (srfi 151)) ;; aka srfi 151
 ((scheme bytevector) . (r6rs bytevectors)) ;; aka r6rs bytevectors
 ((scheme division) . (srfi 141)) ;; aka srfi 141
 ((scheme fixnum) . (srfi 143)) ;; aka srfi 143
 ((scheme flonum) . (srfi 144)) ;; aka srfi 144
 ((scheme mapping) . (srfi 146)) ;; aka srfi 146
 ((scheme mapping hash) . (srfi 146 hash)) ;; aka srfi 146
 ((scheme vector base) . (srfi 160 base)) ;; aka srfi 160
 ((scheme vector u8) . (srfi 160 u8)) ;; aka srfi 160
 ((scheme vector s8) . (srfi 160 s8)) ;; aka srfi 160
 ((scheme vector u16) . (srfi 160 u16)) ;; aka srfi 160
 ((scheme vector s16) . (srfi 160 s16)) ;; aka srfi 160
 ((scheme vector u32) . (srfi 160 u32)) ;; aka srfi 160
 ((scheme vector s32) . (srfi 160 s32)) ;; aka srfi 160
 ((scheme vector u64) . (srfi 160 u64)) ;; aka srfi 160
 ((scheme vector s64) . (srfi 160 s64)) ;; aka srfi 160
 ((scheme vector f32) . (srfi 160 f32)) ;; aka srfi 160
 ((scheme vector f64) . (srfi 160 f64)) ;; aka srfi 160
 ((scheme vector c64) . (srfi 160 c64)) ;; aka srfi 160
 ((scheme vector c128) . (srfi 160 c128)) ;; aka srfi 160

 ;; misc srfi, added in order of popularity according to https://misc.lassi.io/2019/srfi-implementation-counts.scm
 ;; omitting anything added to r7rs small / large
 ((srfi 8) . #t)
 ((srfi 2) . #t)
 ((srfi 27) . #t)
 ((srfi 13) . #t)
 ((srfi 26) . #t)
 ((srfi 60) . #t)
 ((srfi 28) . #t)
 ((srfi 19) . #t)
 ((srfi 38) . #t)
 ((srfi 31) . #t)
 ((srfi 42) . #t)
 ((srfi 69) . #t)
 ((srfi 17) . #t)
 ((srfi 37) . #t)
 ((srfi 45) . #t)
 ((srfi 61) . #t)
 ((srfi 25) . #t)
 ((srfi 78) . #t)
 ((srfi 18) . #t)
 ((srfi 48) . #t)
 ((srfi 64) . #t)
 ((srfi 95) . #t)
 ((srfi 67) . #t)
 ((srfi 35) . #t)

)
