FROM scmindex-builder:latest

FROM openjdk:11
WORKDIR app
COPY --from=0 /app/dist .
ENV JAVA_OPTS=-Xmx1G
CMD java ${JAVA_OPTS} -jar scheme-index.jar
