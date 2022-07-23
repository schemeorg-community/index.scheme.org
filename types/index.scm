(
 ;; r6rs
 ((rnrs base (6)) . "types/rnrs.base.6.scm")
 ((rnrs arithmetic fixnum (6)) . "types/rnrs.arithmetic.fixnum.6.scm")
 ;;rnrs bytevector is under r7rs tangerine (scheme bytevectors)
 ((rnrs conditions (6)) . "types/rnrs.conditions.6.scm")
 ((rnrs control (6)) . "types/rnrs.control.6.scm")
 ((rnrs exceptions (6)) . "types/rnrs.exceptions.6.scm")
 ((rnrs files (6)) . "types/rnrs.files.6.scm")
 ((rnrs io ports (6)) . "types/rnrs.io.ports.6.scm")
 ((rnrs io simple (6)) . "types/rnrs.io.simple.6.scm")
 ((rnrs lists (6)) . "types/rnrs.lists.6.scm")
 ((rnrs programs (6)) . "types/rnrs.programs.6.scm")
 ((rnrs records inspection (6)) . "types/rnrs.records.inspection.6.scm")
 ((rnrs records procedural (6)) . "types/rnrs.records.procedural.6.scm")
 ((rnrs records syntactic (6)) . "types/rnrs.records.syntactic.6.scm")
 ((rnrs sorting (6)) . "types/rnrs.sorting.6.scm")
 ((rnrs unicode (6)) . "types/rnrs.unicode.6.scm")

 ;; r7rs small
 ((scheme base) . "types/scheme.base.scm")
 ((scheme case-lambda) . "types/scheme.case-lambda.scm")
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
 
 ; r7rs large red
 ((scheme box) . "types/scheme.box.scm")
 ((scheme comparator) . "types/scheme.comparator.scm")
 ((scheme charset) . "types/scheme.charset.scm")
 ((scheme ephemeron) . "types/scheme.ephemeron.scm")
 ((scheme generator) . "types/scheme.generator.scm")
 ((scheme hash-table) . "types/scheme.hash-table.scm")
 ((scheme ideque) . "types/scheme.ideque.scm")
 ((scheme ilist) . "types/scheme.ilist.scm")
 ((scheme list) . "types/scheme.list.scm")
 ((scheme list-queue) . "types/scheme.list-queue.scm")
 ((scheme lseq) . "types/scheme.lseq.scm")
 ((scheme rlist) . "types/scheme.rlist.scm")
 ((scheme set) . "types/scheme.set.scm")
 ((scheme stream) . "types/scheme.stream.scm")
 ((scheme sort) . "types/scheme.sort.scm")
 ((scheme text) . "types/scheme.text.scm")
 ((scheme vector) . "types/scheme.vector.scm")
 
 ; r7rs large tangerine
 ((scheme bitwise) . "types/scheme.bitwise.scm")
 ((scheme bytevector) . "types/scheme.bytevector.scm")
 ((scheme division) . "types/scheme.division.scm")
 ((scheme fixnum) . "types/scheme.fixnum.scm")
 ((scheme flonum) . "types/scheme.flonum.scm")
 ((scheme mapping) . "types/scheme.mapping.scm")
 ((scheme mapping hash) . "types/scheme.mapping.hash.scm")
 ((scheme regex) . "types/scheme.regex.scm")
 ((scheme show) . "types/scheme.show.scm")
 ((scheme vector base) . "types/scheme.vector.base.scm")
 ((scheme vector u8) . "types/scheme.vector.u8.scm")
 ((scheme vector s8) . "types/scheme.vector.s8.scm")
 ((scheme vector u16) . "types/scheme.vector.u16.scm")
 ((scheme vector s16) . "types/scheme.vector.s16.scm")
 ((scheme vector u32) . "types/scheme.vector.u32.scm")
 ((scheme vector s32) . "types/scheme.vector.s32.scm")
 ((scheme vector u64) . "types/scheme.vector.u64.scm")
 ((scheme vector s64) . "types/scheme.vector.s64.scm")
 ((scheme vector f32) . "types/scheme.vector.f32.scm")
 ((scheme vector f64) . "types/scheme.vector.f64.scm")
 ((scheme vector c64) . "types/scheme.vector.c64.scm")
 ((scheme vector c128) . "types/scheme.vector.c128.scm")

 ;; misc srfi, added in order of popularity according to https://misc.lassi.io/2019/srfi-implementation-counts.scm
 ;; omitting anything added to r7rs small / large
 ;; srfi 1 -- part of r7rs large
 ((srfi 8) . "types/srfi.8.scm")
 ((srfi 2) . "types/srfi.2.scm")
 ;; srfi 9 -- part of r7rs small
 ;; srfi 6 -- part of r7rs small
 ;; srfi 0 -- part of r7rs small
 ;; srfi 14 -- part of r7rs large
 ;; srfi 23 -- part of r7rs small
 ((srfi 27) . "types/srfi.27.scm")
 ;; srfi 39 -- part of r7rs small
 ((srfi 13) . "types/srfi.13.scm")
 ((srfi 26) . "types/srfi.26.scm")
 ((srfi 60) . "types/srfi.60.scm")
 ;; srfi 16 -- part of r7rs small
 ((srfi 28) . "types/srfi.28.scm")
 ;; srfi 11 -- part of r7rs small
 ((srfi 19) . "types/srfi.19.scm")
 ((srfi 38) . "types/srfi.38.scm")
 ((srfi 31) . "types/srfi.31.scm")
 ((srfi 42) . "types/srfi.42.scm")
 ;((srfi 69) . "types/srfi.69.scm") TODO? 125 is upwards compatible to 69
 ;; srfi 98 -- part of r7rs small
 ;; srfi 4 -- superceded in r7rs large
 ((srfi 17) . "types/srfi.17.scm")
 ((srfi 37) . "types/srfi.37.scm")
 ;; srfi 41 -- part of r7rs large
 ;; srfi 43 -- part of r7rs large
 ;; srfi 30 -- non-sexpr syntax. Also part of r7rs small
 ((srfi 45) . "types/srfi.45.scm")
 ((srfi 61) . "types/srfi.61.scm")
 ((srfi 25) . "types/srfi.25.scm")
 ;; srfi 34 -- part of r7rs small
 ;; srfi 62 -- non-sexpr syntax
 ((srfi 78) . "types/srfi.78.scm")
 ((srfi 18) . "types/srfi.18.scm")
 ((srfi 48) . "types/srfi.48.scm")
 ((srfi 64) . "types/srfi.64.scm")
 ;;((srfi 95) . "types/srfi.95.scm") TODO
 ;; srfi 22 -- non-sexpr syntax
 ;; srfi 55 -- ??
 ;;((srfi 67) . "types/srfi.67.scm") TODO
 ;;((srfi 87) . "types/srfi.87.scm") TODO
 ;; srfi 128 -- part of r7rs large
 ;; srfi 7 -- ??
 ;; srfi 10 -- non-sexrp syntax
 ;;((srfi 35) . "types/srfi.35.scm") TODO
 ;;((srfi 63) . "types/srfi.63.scm") TODO
 ;;((srfi 66) . "types/srfi.66.scm") TODO
 ;;((srfi 99) . "types/srfi.99.scm") TODO
 ;; srfi 111 -- part of r7rs large
 ;; srfi 117 -- part of r7rs large
 ;; srfi 125 -- part of r7rs large
 ;; srfi 127 -- part of r7rs large
 ;; srfi 133 -- part of r7rs large
 ;;((srfi 5) . "types/srfi.5.scm") TODO








 )
