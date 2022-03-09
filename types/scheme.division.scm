(
 
 (floor/
   (lambda ((integer? numerator) (integer? denominator)) (values integer? integer?))
   (pure))
 
 (floor-quotient
   (lambda ((integer? numerator) (integer? denominator)) integer?)
   (pure))
 
 (floor-remainder
   (lambda ((integer? numerator) (integer? denominator)) integer?)
   (pure))
 
 (ceiling/
   (lambda ((integer? numerator) (integer? denominator)) (values integer? integer?))
   (pure))
 
 (ceiling-quotient
   (lambda ((integer? numerator) (integer? denominator)) integer?)
   (pure))
 
 (ceiling-remainder
   (lambda ((integer? numerator) (integer? denominator)) integer?)
   (pure))
 
 (truncate/
   (lambda ((integer? numerator) (integer? denominator)) (values integer? integer?))
   (pure))
 
 (truncate-quotient
   (lambda ((integer? numerator) (integer? denominator)) integer?)
   (pure))
 
 (truncate-remainder
   (lambda ((integer? numerator) (integer? denominator)) integer?)
   (pure))
 
 (round/
   (lambda ((integer? numerator) (integer? denominator)) (values integer? integer?))
   (pure))
 
 (round-quotient
   (lambda ((integer? numerator) (integer? denominator)) integer?)
   (pure))
 
 (round-remainder
   (lambda ((integer? numerator) (integer? denominator)) integer?)
   (pure))
 
 (euclidean/
   (lambda ((integer? numerator) (integer? denominator)) (values integer? integer?))
   (pure))
 
 (euclidean-quotient
   (lambda ((integer? numerator) (integer? denominator)) integer?)
   (pure))
 
 (euclidean-remainder
   (lambda ((integer? numerator) (integer? denominator)) integer?)
   (pure))
 
 (balanced/
   (lambda ((integer? numerator) (integer? denominator)) (values integer? integer?))
   (pure))
 
 (balanced-quotient
   (lambda ((integer? numerator) (integer? denominator)) integer?)
   (pure))
 
 (balanced-remainder
   (lambda ((integer? numerator) (integer? denominator)) integer?)
   (pure))
 )
