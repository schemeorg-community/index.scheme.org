FROM integrationtest-base

FROM docker.io/schemers/chezscheme
COPY --from=0 /app/integrationtest/schemeasserts/chez-tests /app/chez-tests
COPY integrationtest/schemeasserts/tester-r6rs.ss /app/
COPY integrationtest/schemeasserts/tester-impl.scm /app/
WORKDIR /app
CMD for filename in chez-tests/*; do echo "Running test $filename"; scheme --program $filename; done
