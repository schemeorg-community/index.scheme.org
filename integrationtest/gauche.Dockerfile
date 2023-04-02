FROM integrationtest-base

FROM docker.io/schemers/gauche
COPY --from=0 /app/integrationtest/gauche-tests /app/gauche-tests
COPY integrationtest/tester-r7rs.sld /app/
COPY integrationtest/tester-impl.scm /app/
WORKDIR /app
CMD for filename in gauche-tests/*; do echo "Running test $filename"; gosh -I . $filename; done
