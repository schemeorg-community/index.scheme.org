FROM integrationtest-base

FROM docker.io/schemers/gauche
COPY --from=0 /app/integrationtest/schemeasserts/gauche-tests /app/gauche-tests
COPY integrationtest/schemeasserts/tester-r7rs.sld /app/
COPY integrationtest/schemeasserts/tester-impl.scm /app/
WORKDIR /app
CMD for filename in gauche-tests/*; do echo "Running test $filename"; gosh -I . $filename; done
