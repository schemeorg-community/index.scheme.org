#!/bin/bash
set -e

chicken-install json matchable srfi-1
csc validatedata.scm
cd ../
./scheme-index-util/validatedata filters/index.scm types/index.scm

