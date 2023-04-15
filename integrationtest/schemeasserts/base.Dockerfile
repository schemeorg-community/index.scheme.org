FROM docker.io/schemers/gauche
COPY filters /app/filters
COPY types /app/types
COPY integrationtest/schemeasserts/generate.scm /app/integrationtest/schemeasserts/
COPY integrationtest/schemeasserts/testlist.scm /app/integrationtest/schemeasserts/
RUN mkdir /app/integrationtest/schemeasserts/bigloo-tests
RUN mkdir /app/integrationtest/schemeasserts/chez-tests
RUN mkdir /app/integrationtest/schemeasserts/chibi-tests
RUN mkdir /app/integrationtest/schemeasserts/gauche-tests
RUN mkdir /app/integrationtest/schemeasserts/guile-tests
WORKDIR /app
RUN gosh integrationtest/schemeasserts/generate.scm
