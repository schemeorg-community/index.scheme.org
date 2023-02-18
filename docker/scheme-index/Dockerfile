FROM sbtscala/scala-sbt:eclipse-temurin-jammy-17.0.5_8_1.8.2_3.2.2
WORKDIR /app
COPY . .
RUN sbt clean assembly

FROM openjdk:11
WORKDIR app
COPY --from=0 /app/target/scala-*/*.jar scheme-index.jar
COPY types types
COPY config/configuration-dist.scm config/configuration.scm
COPY filters filters
COPY solrhome/solr.xml solrhome/solr.xml
COPY solrhome/scmindex/core.properties solrhome/scmindex/core.properties
COPY solrhome/scmindex/conf solrhome/scmindex/conf
ENV JAVA_OPTS=-Xmx1G
CMD java ${JAVA_OPTS} -jar scheme-index.jar
