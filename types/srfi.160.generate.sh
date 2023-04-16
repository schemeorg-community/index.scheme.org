#!/bin/bash

set -e

apply() {
    sed "s/@?-ELTYPE/$1/g ; s/@-DESC/$2/g ; s/@/$3/g" srfi.160.BASE_PLACEHOLDER.scm >> srfi.160.base.scm

    echo '(' > srfi.160.$3.scm
    sed "s/@?-ELTYPE/$1/g ; s/@-DESC/$2/g ; s/@/$3/g" srfi.160.BASE_PLACEHOLDER.scm >> srfi.160.$3.scm
    sed "s/@?-ELTYPE/$1/g ; s/@-DESC/$2/g ; s/@/$3/g" srfi.160.REST_PLACEHOLDER.scm >> srfi.160.$3.scm
    echo ')' >> srfi.160.$3.scm
}

echo '(' > srfi.160.base.scm

apply 'integer?' 'unsigned exact integer in the range 0 to (2^8)-1' 'u8'
apply 'integer?' 'signed exact integer in the range -(2^7) to (2^7)-1' 's8'
apply 'integer?' 'unsigned exact integer in the range 0 to (2^16)-1' 'u16'
apply 'integer?' 'signed exact integer in the range -(2^15) to (2^15)-1' 's16'
apply 'integer?' 'unsigned exact integer in the range 0 to (2^32)-1' 'u32'
apply 'integer?' 'signed exact integer in the range -(2^31) to (2^31)-1' 's32'
apply 'integer?' 'unsigned exact integer in the range 0 to (2^64)-1' 'u64'
apply 'integer?' 'signed exact integer in the range -(2^63) to (2^63)-1' 's64'
apply 'real?' 'inexact real' 'f32'
apply 'real?' 'inexact real' 'f64'
apply 'complex?' 'inexact complex' 'c64'
apply 'complex?' 'inexact complex' 'c128'

echo ')' >> srfi.160.base.scm
