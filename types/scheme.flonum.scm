(
 
    (fl-e (value flonum?))

    (fl-1/e (value flonum?))

    (fl-e-2 (value flonum?))

    (fl-e-pi/4 (value flonum?))

    (fl-log2-e (value flonum?))

    (fl-log10-e (value flonum?))

    (fl-log-2 (value flonum?))

    (fl-1/log-2 (value flonum?))

    (fl-log-3 (value flonum?))

    (fl-log-pi (value flonum?))

    (fl-log-10 (value flonum?))

    (fl-1/log-10 (value flonum?))

    (fl-pi (value flonum?))

    (fl-1/pi (value flonum?))

    (fl-2pi (value flonum?))

    (fl-pi/2 (value flonum?))

    (fl-pi/4 (value flonum?))

    (fl-pi-squared (value flonum?))

    (fl-degree (value flonum?))

    (fl-2/pi (value flonum?))

    (fl-2/sqrt-pi (value flonum?))

    (fl-sqrt-2 (value flonum?))

    (fl-sqrt-3 (value flonum?))

    (fl-sqrt-5 (value flonum?))

    (fl-sqrt-10 (value flonum?))

    (fl-1/sqrt-2 (value flonum?))

    (fl-cbrt-2 (value flonum?))

    (fl-cbrt-3 (value flonum?))

    (fl-4thrt-2 (value flonum?))

    (fl-phi (value flonum?))

    (fl-log-phi (value flonum?))

    (fl-1/log-phi (value flonum?))

    (fl-euler (value flonum?))

    (fl-e-euler (value flonum?))

    (fl-sin-1 (value flonum?))

    (fl-cos-1 (value flonum?))

    (fl-gamma-1/2 (value flonum?))

    (fl-gamma-1/3 (value flonum?))

    (fl-gamma-2/3 (value flonum?))

    (fl-greatest (value flonum?))

    (fl-least (value flonum?))

    (fl-epsilon (value flonum?))

    (fl-fast-fl+* (value boolean?))

    (fl-integer-exponent-zero (value integer?))

    (fl-integer-exponent-nan (value integer?))

    (flonum
      (lambda ((real? number)) flonum?)
      (pure))

    (fladjacent
      (lambda ((flonum? x) (flonum? y)) flonum?)
      (pure))

    (flcopysign
      (lambda ((flonum? x) (flonum? y)) flonum?)
      (pure))

    (make-flonum
      (lambda ((flonum? x) (integer? n)) flonum?)
      (pure))

    (flinteger-fraction
      (lambda ((flonum? x)) (values flonum? flonum?))
      (pure))

    (flexponent
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flinteger-exponent
      (lambda ((flonum? x)) integer?)
      (pure))

    (flnormalized-fraction-exponent
      (lambda ((flonum? x)) (flonum? integer?))
      (pure))

    (flsign-bit
      (lambda ((flonum? x)) integer?)
      (pure))

    (flonum?
      (lambda (obj) boolean?)
      (pure predicate)
      ()
      (real?))

    (fl=?
      (lambda ((flonum? x) (flonum? y) (flonum? z) ...) boolean?)
      (pure))

    (fl<?
      (lambda ((flonum? x) (flonum? y) (flonum? z) ...) boolean?)
      (pure))

    (fl>?
      (lambda ((flonum? x) (flonum? y) (flonum? z) ...) boolean?)
      (pure))

    (fl<=?
      (lambda ((flonum? x) (flonum? y) (flonum? z) ...) boolean?)
      (pure))

    (fl>=?
      (lambda ((flonum? x) (flonum? y) (flonum? z) ...) boolean?)
      (pure))

    (flunordered
      (lambda ((flonum? x) (flonum? y)) boolean?)
      (pure))

    (flinteger?
      (lambda ((flonum? x)) boolean?)
      (pure))

    (flzero?
      (lambda ((flonum? x)) boolean?)
      (pure))

    (flpositive?
      (lambda ((flonum? x)) boolean?)
      (pure))

    (flnegative?
      (lambda ((flonum? x)) boolean?)
      (pure))

    (flodd?
      (lambda ((flonum? x)) boolean?)
      (pure))

    (fleven?
      (lambda ((flonum? x)) boolean?)
      (pure))

    (flfinite?
      (lambda ((flonum? x)) boolean?)
      (pure))

    (flinfinite?
      (lambda ((flonum? x)) boolean?)
      (pure))

    (flnan?
      (lambda ((flonum? x)) boolean?)
      (pure))

    (flnormalized?
      (lambda ((flonum? x)) boolean?)
      (pure))

    (fldenormalized?
      (lambda ((flonum? x)) boolean?)
      (pure))

    (flmax
      (lambda ((flonum? x) ...) flonum?)
      (pure))

    (flmin
      (lambda ((flonum? x) ...) flonum?)
      (pure))

    (fl+
      (lambda ((flonum? x) ...) flonum?)
      (pure))

    (fl*
      (lambda ((flonum? x) ...) flonum?)
      (pure))

    (fl+*
      (lambda ((flonum? x) (flonum? y) (flonum? z)) flonum?)
      (pure))

    (fl-
      (lambda ((flonum? x) (flonum? y) ...) flonum?)
      (pure))

    (fl/
      (lambda ((flonum? x) (flonum? y) ...) flonum?)
      (pure))

    (flabs
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flabsdiff
      (lambda ((flonum? x) (flonum? y)) flonum?)
      (pure))

    (flposdiff
      (lambda ((flonum? x) (flonum? y)) flonum?)
      (pure))

    (flsgn
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flnumerator
      (lambda ((flonum? x)) flonum?)
      (pure))

    (fldenominator
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flfloor
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flceiling
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flround
      (lambda ((flonum? x)) flonum?)
      (pure))

    (fltruncate
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flexp
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flexp2
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flexp-1
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flsquare
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flsqrt
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flcbrt
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flhypot
      (lambda ((flonum? x) (flonum? y)) flonum?)
      (pure))

    (flexpt
      (lambda ((flonum? x) (flonum? y)) flonum?)
      (pure))

    (fllog
      (lambda ((flonum? x)) flonum?)
      (pure))

    (fllog1+
      (lambda ((flonum? x)) flonum?)
      (pure))

    (fllog2
      (lambda ((flonum? x)) flonum?)
      (pure))

    (fllog10
      (lambda ((flonum? x)) flonum?)
      (pure))

    (make-fllog-base
      (lambda ((flonum? base)) procedure?)
      (pure)
      ((return (lambda ((flonum? x)) flonum?))))

    (flsin
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flcos
      (lambda ((flonum? x)) flonum?)
      (pure))

    (fltan
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flasin
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flacos
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flatan
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flatan
      (lambda ((flonum? y) (flonum? x)) flonum?)
      (pure))

    (flsinh
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flcosh
      (lambda ((flonum? x)) flonum?)
      (pure))

    (fltanh
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flasinh
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flacosh
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flatanh
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flquotient
      (lambda ((flonum? x) (flonum? y)) flonum?)
      (pure))

    (flremainder
      (lambda ((flonum? x) (flonum? y)) flonum?)
      (pure))

    (flremquo
      (lambda ((flonum? x) (flonum? y)) (values flonum? integer?))
      (pure))

    (flgamma
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flloggamma
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flfirst-bessel
      (lambda ((integer? n) (flonum? x)) flonum?)
      (pure))

    (flsecond-bessel
      (lambda ((integer? n) (flonum? x)) flonum?)
      (pure))

    (flerf
      (lambda ((flonum? x)) flonum?)
      (pure))

    (flerfc
      (lambda ((flonum? x)) flonum?)
      (pure))

)
