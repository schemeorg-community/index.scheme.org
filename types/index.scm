(
 (r5rs . "types/r5rs.scm")
 ;; r6rs
 ((rnrs base (6)) . "types/rnrs.base.6.scm")
 ((rnrs arithmetic bitwise (6)) . "types/rnrs.arithmetic.bitwise.6.scm")
 ((rnrs arithmetic fixnums (6)) . "types/rnrs.arithmetic.fixnums.6.scm")
 ((rnrs arithmetic flonums (6)) . "types/rnrs.arithmetic.flonums.6.scm")
 ((rnrs bytevectors (6)) . "types/rnrs.bytevectors.6.scm")
 ((rnrs conditions (6)) . "types/rnrs.conditions.6.scm")
 ((rnrs control (6)) . "types/rnrs.control.6.scm")
 ((rnrs enums (6)) . "types/rnrs.enums.6.scm")
 ((rnrs eval (6)) . "types/scheme.eval.scm")
 ((rnrs exceptions (6)) . "types/rnrs.exceptions.6.scm")
 ((rnrs files (6)) . "types/rnrs.files.6.scm")
 ((rnrs files (6)) . "types/rnrs.io.commons.scm")
 ((rnrs hashtables (6)) . "types/rnrs.hashtables.6.scm")
 ((rnrs io ports (6)) . "types/rnrs.io.ports.6.scm")
 ((rnrs io ports (6)) . "types/rnrs.io.commons.scm")
 ((rnrs io simple (6)) . "types/rnrs.io.simple.6.scm")
 ((rnrs io simple (6)) . "types/rnrs.io.commons.scm")
 ((rnrs lists (6)) . "types/rnrs.lists.6.scm")
 ((rnrs mutable-pairs (6)) . "types/rnrs.mutable-pairs.6.scm")
 ((rnrs mutable-strings (6)) . "types/rnrs.mutable-strings.6.scm")
 ((rnrs programs (6)) . "types/rnrs.programs.6.scm")
 ((rnrs r5rs (6)) . "types/rnrs.r5rs.6.scm")
 ((rnrs records inspection (6)) . "types/rnrs.records.inspection.6.scm")
 ((rnrs records procedural (6)) . "types/rnrs.records.procedural.6.scm")
 ((rnrs records syntactic (6)) . "types/rnrs.records.syntactic.6.scm")
 ((rnrs sorting (6)) . "types/rnrs.sorting.6.scm")
 ((rnrs unicode (6)) . "types/rnrs.unicode.6.scm")

 ;; r7rs small
 ((scheme base) . "types/scheme.base.scm")
 ((scheme base) . "types/srfi.0.scm")
 ((scheme base) . "types/srfi.6.scm")
 ((scheme base) . "types/srfi.9.scm")
 ((scheme base) . "types/srfi.11.scm")
 ((scheme base) . "types/srfi.23.scm")
 ((scheme base) . "types/srfi.34.scm")
 ((scheme base) . "types/srfi.39.scm")
 ((scheme case-lambda) . "types/srfi.16.scm")
 ((scheme complex) . "types/scheme.complex.scm")
 ((scheme char) . "types/scheme.char.scm")
 ((scheme cxr) . "types/scheme.cxr.scm")
 ((scheme eval) . "types/scheme.eval.scm")
 ((scheme file) . "types/scheme.file.scm")
 ((scheme inexact) . "types/scheme.inexact.scm")
 ((scheme lazy) . "types/scheme.lazy.scm")
 ((scheme load) . "types/scheme.load.scm")
 ((scheme process-context) . "types/scheme.process-context.scm")
 ((scheme read) . "types/scheme.read.scm")
 ((scheme repl) . "types/scheme.repl.scm")
 ((scheme time) . "types/scheme.time.scm")
 ((scheme write) . "types/scheme.write.scm")
 ((scheme r5rs) . ((file . "types/r5rs.scm") (exclude . (transcript-on transcript-off))))
 
 ; r7rs large red
 ((scheme box) . "types/srfi.111.scm")
 ((scheme comparator) . "types/srfi.128.scm")
 ((scheme charset) . "types/srfi.14.scm")
 ((scheme ephemeron) . "types/srfi.124.scm")
 ((scheme generator) . "types/srfi.158.scm")
 ((scheme hash-table) . "types/srfi.125.scm")
 ((scheme ideque) . "types/srfi.134.scm")
 ((scheme ilist) . "types/srfi.116.scm")
 ((scheme list) . "types/srfi.1.scm")
 ((scheme list-queue) . "types/srfi.117.scm")
 ((scheme lseq) . "types/srfi.127.scm")
 ((scheme rlist) . "types/scheme.rlist.scm")
 ((scheme set) . "types/srfi.113.scm")
 ((scheme stream) . "types/srfi.41.scm")
 ((scheme sort) . "types/srfi.132.scm")
 ((scheme text) . "types/srfi.135.scm")
 ((scheme vector) . "types/srfi.133.scm")
 
 ; r7rs large tangerine
 ((scheme bitwise) . "types/srfi.151.scm")
 ((scheme bytevector) . "types/rnrs.bytevectors.6.scm")
 ((scheme division) . "types/srfi.141.scm")
 ((scheme fixnum) . "types/srfi.143.scm")
 ((scheme flonum) . "types/srfi.144.scm")
 ((scheme mapping) . "types/srfi.146.scm")
 ((scheme hashmap) . "types/srfi.146.hash.scm")
 ((scheme regex) . "types/srfi.115.scm")
 ((scheme show) . "types/srfi.159.scm")
 ((scheme vector base) . "types/srfi.160.base.scm")
 ((scheme vector u8) . "types/srfi.160.u8.scm")
 ((scheme vector s8) . "types/srfi.160.s8.scm")
 ((scheme vector u16) . "types/srfi.160.u16.scm")
 ((scheme vector s16) . "types/srfi.160.s16.scm")
 ((scheme vector u32) . "types/srfi.160.u32.scm")
 ((scheme vector s32) . "types/srfi.160.s32.scm")
 ((scheme vector u64) . "types/srfi.160.u64.scm")
 ((scheme vector s64) . "types/srfi.160.s64.scm")
 ((scheme vector f32) . "types/srfi.160.f32.scm")
 ((scheme vector f64) . "types/srfi.160.f64.scm")
 ((scheme vector c64) . "types/srfi.160.c64.scm")
 ((scheme vector c128) . "types/srfi.160.c128.scm")

 ((srfi 0) . "types/srfi.0.scm")
 ((srfi 1) . "types/srfi.1.scm")
 ((srfi 2) . "types/srfi.2.scm")
 ;; srfi 3 -- withdrawn
 ((srfi 4) . "types/srfi.4.u8.scm")
 ((srfi 4) . "types/srfi.4.s8.scm")
 ((srfi 4) . "types/srfi.4.u16.scm")
 ((srfi 4) . "types/srfi.4.s16.scm")
 ((srfi 4) . "types/srfi.4.u32.scm")
 ((srfi 4) . "types/srfi.4.s32.scm")
 ((srfi 4) . "types/srfi.4.u64.scm")
 ((srfi 4) . "types/srfi.4.s64.scm")
 ((srfi 4) . "types/srfi.4.f32.scm")
 ((srfi 4) . "types/srfi.4.f64.scm")
 ((srfi 5) . "types/srfi.5.scm")
 ((srfi 6) . "types/srfi.6.scm")
 ;; srfi 7 -- ??
 ((srfi 8) . "types/srfi.8.scm")
 ((srfi 9) . "types/srfi.9.scm")
 ;; srfi 10 -- non-sexrp syntax
 ((srfi 11) . "types/srfi.11.scm")
 ;; srfi 12 -- withdrawn
 ((srfi 13) . "types/srfi.13.scm")
 ((srfi 14) . "types/srfi.14.scm")
 ;; srfi 15 -- withdrawn
 ((srfi 16) . "types/srfi.16.scm")
 ((srfi 17) . "types/srfi.17.scm")
 ((srfi 18) . "types/srfi.18.scm")
 ((srfi 19) . "types/srfi.19.scm")
 ;; srfi 20 -- withdrawn
 ((srfi 21) . "types/srfi.21.scm")
 ;; srfi 22 -- non-sexpr syntax
 ((srfi 23) . "types/srfi.23.scm")
 ;; srfi 24 -- withdrawn
 ((srfi 25) . "types/srfi.25.scm")
 ((srfi 26) . "types/srfi.26.scm")
 ((srfi 27) . "types/srfi.27.scm")
 ((srfi 28) . "types/srfi.28.scm")
 ((srfi 29) . "types/srfi.29.scm")
 ;; srfi 30 -- non-sexpr syntax
 ((srfi 31) . "types/srfi.31.scm")
 ;; srfi 32 -- withdrawn
 ;; srfi 33 -- withdrawn
 ((srfi 34) . "types/srfi.34.scm")
 ((srfi 35) . "types/srfi.35.scm")
 ((srfi 36) . "types/srfi.36.scm")
 ((srfi 37) . "types/srfi.37.scm")
 ((srfi 38) . "types/srfi.38.scm")
 ((srfi 39) . "types/srfi.39.scm")
 ;; srfi 40 -- superceded
 ((srfi 41) . "types/srfi.41.scm")
 ((srfi 42) . "types/srfi.42.scm")
 ((srfi 43) . "types/srfi.43.scm")
 ;; srfi 44 -- "meta" srfi
 ((srfi 45) . "types/srfi.45.scm")
 ((srfi 46) . "types/srfi.46.scm")
 ((srfi 47) . "types/srfi.47.scm")
 ((srfi 48) . "types/srfi.48.scm")
 ;; srfi 49 -- non-sexpr syntax
 ;; srfi 50 -- withdrawn
 ((srfi 51) . "types/srfi.51.scm")
 ;; srfi 52 -- withdrawn
 ;; srfi 53 -- withdrawn
 ((srfi 54) . "types/srfi.54.scm")
 ;; srfi 55 -- ??
 ;; srfi 56 -- withdrawn
 ;; ((srfi 57) . "types/srfi.57.scm") TODO
 ;; srfi 58 -- non-sexpr syntax
 ((srfi 59) . "types/srfi.59.scm")
 ((srfi 60) . "types/srfi.60.scm")
 ((srfi 61) . "types/srfi.61.scm")
 ;; srfi 62 -- non-sexpr syntax
 ((srfi 63) . "types/srfi.63.scm")
 ((srfi 64) . "types/srfi.64.scm")
 ;; srfi 65 -- withdrawn
 ((srfi 66) . "types/srfi.66.scm")
 ((srfi 67) . "types/srfi.67.scm")
 ;; srfi 68 -- withdrawn
 ((srfi 69) . "types/srfi.69.scm")
 ((srfi 70) . "types/srfi.70.scm")
 ((srfi 71) . "types/srfi.71.scm")
 ;; srfi 72 -- unpopular (TODO?)
 ;; srfi 73 -- withdrawn
 ((srfi 74) . "types/srfi.74.scm")
 ;; srfi 75 -- withdrawn
 ;; srfi 76 -- withdrawn
 ;; srfi 77 -- withdrawn
 ((srfi 78) . "types/srfi.78.scm")
 ;; srfi 79 -- withdrawn
 ;; srfi 80 -- withdrawn
 ;; srfi 81 -- withdrawn
 ;; srfi 82 -- withdrawn
 ;; srfi 83 -- withdrawn
 ;; srfi 84 -- withdrawn
 ;; srfi 85 -- withdrawn
 ;;((srfi 86) . "types/srfi.86.scm") TODO
 ((srfi 87) . "types/srfi.87.scm")
 ((srfi 88) . "types/srfi.88.scm")
 ;;((srfi 89) . "types/srfi.89.scm") TODO
 ;;((srfi 90) . "types/srfi.90.scm") TODO
 ;; srfi 91 -- withdrawn
 ;; srfi 92 -- withdrawn
 ;; srfi 93 -- withdrawn
 ;; ((srfi 94) . "types/srfi.94.scm") TODO
 ((srfi 95) . "types/srfi.95.scm")
 ;; ((srfi 96) . "types/srfi.96.scm") TODO
 ;; srfi 97 meta srfi
 ((srfi 98) . "types/srfi.98.scm")
 ((srfi 99) . "types/srfi.99.records.procedural.scm")
 ((srfi 99) . "types/srfi.99.records.inspection.scm")
 ((srfi 99) . "types/srfi.99.records.syntactic.scm")
 ((srfi 99 records procedural) . "types/srfi.99.records.procedural.scm")
 ((srfi 99 records inspection) . "types/srfi.99.records.inspection.scm")
 ((srfi 99 records syntactic) . "types/srfi.99.records.syntactic.scm")
 ((srfi 100) . "types/srfi.100.scm")
 ((srfi 101) . "types/srfi.101.scm")
 ;; srfi 102 -- withdrawn
 ;; srfi 103 -- withdrawn
 ;; srfi 104 -- withdrawn
 ;; srfi 105 -- non-sexpr syntax
 ((srfi 106) . "types/srfi.106.scm")
 ;; srfi 107 -- non-sexpr syntax
 ;; srfi 108 -- non-sexpr syntax
 ;; srfi 109 -- non-sexpr syntax
 ;; srfi 110 -- non-sexpr syntax
 ((srfi 111) . "types/srfi.111.scm")
 ((srfi 112) . "types/srfi.112.scm")
 ((srfi 113) . "types/srfi.113.scm")
 ;; srfi 114 -- superceded
 ((srfi 115) . "types/srfi.115.scm")
 ((srfi 116) . "types/srfi.116.scm")
 ((srfi 117) . "types/srfi.117.scm")
 ((srfi 118) . "types/srfi.118.scm")
 ;; srfi 119 -- non-sexpr syntax
 ((srfi 120) . "types/srfi.120.scm")
 ;; srfi 121 -- superceded
 ;; srfi 122 -- superceded
 ((srfi 123) . "types/srfi.123.scm")
 ((srfi 124) . "types/srfi.124.scm")
 ((srfi 125) . "types/srfi.125.scm")
 ((srfi 126) . "types/srfi.126.scm")
 ((srfi 127) . "types/srfi.127.scm")
 ((srfi 128) . "types/srfi.128.scm")
 ((srfi 129) . "types/srfi.129.scm")
 ((srfi 130) . "types/srfi.130.scm")
 ((srfi 131) . "types/srfi.99.records.procedural.scm")
 ((srfi 131) . "types/srfi.99.records.inspection.scm")
 ((srfi 131) . "types/srfi.131.records.syntactic.scm")
 ((srfi 131 records procedural) . "types/srfi.99.records.procedural.scm")
 ((srfi 131 records inspection) . "types/srfi.99.records.inspection.scm")
 ((srfi 131 records syntactic) . "types/srfi.131.records.syntactic.scm")
 ((srfi 132) . "types/srfi.132.scm")
 ((srfi 133) . "types/srfi.133.scm")
 ((srfi 134) . "types/srfi.134.scm")
 ((srfi 135) . "types/srfi.135.scm")
 ;; ((srfi 136) . "types/srfi.136.scm") TODO
 ((srfi 137) . "types/srfi.137.scm")
 ;; srfi 138 -- non-sexpr syntax
 ((srfi 139) . "types/srfi.139.scm")
 ((srfi 140) . "types/srfi.140.scm")
 ((srfi 141) . "types/srfi.141.scm")
 ;; srfi 142 -- superceded
 ((srfi 143) . "types/srfi.143.scm")
 ((srfi 144) . "types/srfi.144.scm")
 ((srfi 145) . "types/srfi.145.scm")
 ((srfi 146) . "types/srfi.146.scm")
 ((srfi 146 hash) . "types/srfi.146.hash.scm")
 ;; ((srfi 147) . "types/srfi.147.scm") TODO
 ;; srfi 148 -- unpopular (TODO?)
 ;; ((srfi 149) . "types/srfi.149.scm") TODO
 ;; srfi 150 -- unpopular (TODO?)
 ((srfi 151) . "types/srfi.151.scm")
 ((srfi 152) . "types/srfi.152.scm")
 ;; srfi 153 -- withdrawn
 ((srfi 154) . "types/srfi.154.scm")
 ;; srfi 155 -- unpopular (TODO?)
 ((srfi 156) . "types/srfi.156.scm")
 ((srfi 157) . "types/srfi.157.scm")
 ((srfi 158) . "types/srfi.158.scm")
 ((srfi 159) . "types/srfi.159.scm")
 ((srfi 160 base) . "types/srfi.160.base.scm")
 ((srfi 160 u8) . "types/srfi.160.u8.scm")
 ((srfi 160 s8) . "types/srfi.160.s8.scm")
 ((srfi 160 u16) . "types/srfi.160.u16.scm")
 ((srfi 160 s16) . "types/srfi.160.s16.scm")
 ((srfi 160 u32) . "types/srfi.160.u32.scm")
 ((srfi 160 s32) . "types/srfi.160.s32.scm")
 ((srfi 160 u64) . "types/srfi.160.u64.scm")
 ((srfi 160 s64) . "types/srfi.160.s64.scm")
 ((srfi 160 f32) . "types/srfi.160.f32.scm")
 ((srfi 160 f64) . "types/srfi.160.f64.scm")
 ((srfi 160 c64) . "types/srfi.160.c64.scm")
 ((srfi 160 c128) . "types/srfi.160.c128.scm")
 ((srfi 161) . "types/srfi.161.scm")
 ((srfi 162) . "types/srfi.162.scm")
 ;; srfi 163 -- non-sexrp syntax
 ;; ((srfi 164) . "types/srfi.164.scm") TODO
 ((srfi 165) . "types/srfi.165.scm")
 ;; ((srfi 166) . "types/srfi.166.scm") TODO
 ;; ((srfi 167) . "types/srfi.167.scm") TODO
 ;; ((srfi 168) . "types/srfi.168.scm") TODO
 ;; srfi 169 -- non-sexrp syntax
 ;; ((srfi 170) . "types/srfi.170.scm") TODO
 ;; ((srfi 171) . "types/srfi.171.scm") TODO
 ;; srfi 172 -- reexports, probably not useful to duplicate
 ;; ((srfi 173) . "types/srfi.173.scm") TODO
 ;; ((srfi 174) . "types/srfi.174.scm") TODO
 ;; ((srfi 175) . "types/srfi.175.scm") TODO
 ;; srfi 176 -- meta srfi
 ;; srfi 177 -- withdrawn
 ;; ((srfi 178) . "types/srfi.178.scm") TODO
 ((srfi 180) . "types/srfi.180.scm")
 ((srfi 190) . "types/srfi.190.scm")
 ((srfi 193) . "types/srfi.193.scm")
 ((srfi 219) . "types/srfi.219.scm")
 ((srfi 236) . "types/srfi.236.scm")
 ((srfi 239) . "types/srfi.239.scm")

 ;; specific implementation handling
 ;(bigloo . "types/r5rs.scm")
 ;(bigloo . "types/srfi.0.scm")
 ;(bigloo . "types/srfi.2.scm")
 ;(bigloo . "types/srfi.6.scm")
 ;(bigloo . "types/srfi.8.scm")
 ;(bigloo . "types/srfi.9.scm")
 ;(bigloo . "types/srfi.18.scm")
 ;(bigloo . "types/srfi.28.scm")
 ;(bigloo . "types/srfi.34.scm")





 )
