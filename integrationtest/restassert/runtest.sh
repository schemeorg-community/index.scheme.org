#!/bin/sh
set -e
wget --no-check-certificate 'https://nginx/rest/filterset/r5rs/search?rows=100000' -O data_r5rs.json
wget --no-check-certificate 'https://nginx/rest/filterset/r6rs/search?rows=100000' -O data_r6rs.json
wget --no-check-certificate 'https://nginx/rest/filterset/r7rs_all/search?rows=100000' -O data_r7rs.json
ajv validate --spec=draft2020 --errors=json --verbose -s searchresult.schema.json -d  data_r5rs.json -d  data_r6rs.json -d data_r7rs.json
