#FROM miy4/json-schema-validator
#RUN apk add --no-cache openssl
FROM node:19-alpine3.16
RUN npm i -g ajv-cli
ENV DOCKERIZE_VERSION v0.6.1
RUN wget https://github.com/jwilder/dockerize/releases/download/$DOCKERIZE_VERSION/dockerize-alpine-linux-amd64-$DOCKERIZE_VERSION.tar.gz \
    && tar -C /usr/local/bin -xzvf dockerize-alpine-linux-amd64-$DOCKERIZE_VERSION.tar.gz \
    && rm dockerize-alpine-linux-amd64-$DOCKERIZE_VERSION.tar.gz
RUN mkdir -p /app
COPY integrationtest/restassert/runtest.sh /app/
COPY searchresult.schema.json /app/
WORKDIR /app
ENTRYPOINT dockerize -wait tcp://app:8080 sh runtest.sh
