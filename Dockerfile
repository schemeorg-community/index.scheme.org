FROM maven:3.8-openjdk-11
WORKDIR app
COPY pom.xml .
RUN mvn -B org.apache.maven.plugins:maven-dependency-plugin:3.1.0:resolve-plugins org.apache.maven.plugins:maven-dependency-plugin:3.1.0:resolve org.apache.maven.plugins:maven-dependency-plugin:3.1.0:go-offline
COPY . .
RUN mvn clean package

FROM openjdk:11
WORKDIR app
COPY --from=0 /app/target/scheme-index-0.0.1.jar .
COPY types types
COPY dockerconfig config
COPY templates templates
COPY static static
CMD java -jar scheme-index-0.0.1.jar
