#!/bin/bash
set -e

docker build -f Dockerfile -t scheme-index-validator ..
docker run --rm -v ../filters:/filters:ro -v ../types:/types:ro scheme-index-validator
