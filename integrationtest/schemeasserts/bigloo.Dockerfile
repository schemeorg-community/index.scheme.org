FROM integrationtest-base

FROM docker.io/schemers/bigloo
COPY --from=0 /app/integrationtest/schemeasserts/bigloo-tests /app/bigloo-tests
COPY integrationtest/schemeasserts/tester-impl.scm /app/
WORKDIR /app
CMD for filename in bigloo-tests/*; do echo "Running test $filename"; bigloo $filename; done
