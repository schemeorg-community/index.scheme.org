FROM docker.io/schemers/gauche
COPY filters /app/filters
COPY types /app/types
COPY integrationtest/generate.scm /app/integrationtest/
COPY integrationtest/testlist.scm /app/integrationtest/
RUN mkdir /app/integrationtest/bigloo-tests
RUN mkdir /app/integrationtest/chez-tests
RUN mkdir /app/integrationtest/chibi-tests
RUN mkdir /app/integrationtest/gauche-tests
RUN mkdir /app/integrationtest/guile-tests
WORKDIR /app
RUN gosh integrationtest/generate.scm
