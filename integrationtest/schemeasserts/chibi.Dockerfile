FROM integrationtest-base

FROM docker.io/schemers/chibi
COPY --from=0 /app/integrationtest/schemeasserts/chibi-tests /app/chibi-tests
COPY integrationtest/schemeasserts/tester-r7rs.sld /app/
COPY integrationtest/schemeasserts/tester-impl.scm /app/
WORKDIR /app
CMD for filename in chibi-tests/*; do echo "Running test $filename"; chibi-scheme $filename; done
