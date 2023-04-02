FROM integrationtest-base

FROM docker.io/schemers/chibi
COPY --from=0 /app/integrationtest/chibi-tests /app/chibi-tests
COPY integrationtest/tester-r7rs.sld /app/
COPY integrationtest/tester-impl.scm /app/
WORKDIR /app
CMD for filename in chibi-tests/*; do echo "Running test $filename"; chibi-scheme $filename; done
