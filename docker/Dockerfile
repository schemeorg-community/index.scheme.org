FROM maven:3.8-openjdk-11
RUN apt-get update && apt-get -y install ant asciidoctor npm
RUN npm install -g sass
WORKDIR /app
COPY kawa-web-collection kawa-web-collection
WORKDIR /app/kawa-web-collection
RUN mvn install
WORKDIR /app
COPY pom.xml .
RUN mvn -B org.apache.maven.plugins:maven-dependency-plugin:3.1.0:resolve-plugins org.apache.maven.plugins:maven-dependency-plugin:3.1.0:resolve org.apache.maven.plugins:maven-dependency-plugin:3.1.0:go-offline
COPY . .
RUN ant

FROM openjdk:11
WORKDIR app
COPY --from=0 /app/dist .
CMD java -jar scheme-index.jar
