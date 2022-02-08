# R7RS index

Source code for https://r7rsindex.com

## Running locally

### Docker for development

The default `docker-compose up` launches a development configuration. You can access application through port 80, and solr admin gui through port 8983. Under this configuration static and template files
are mounted into the container. Templates aren't cached and are recompiled on each render which may affect performance.

### Docker for use

First generate local certifactes using `make create-dev-certs`. Next, run `docker-compose -f docker-compose.yml -f docker-compose.local.yml up`. Access application through port 443.

### Without docker

Install 
* JDK11, 
* Maven, 
* Solr 6+. 

Launch solr, and create a core using configuration found in `./solrconfig`. Compile the application with `mvn clean package`. You should see `target/r7rs-index-<version>.jar`. Move the jar to desired directory. Copy dockerconfig folder next to the jar, but rename from dockerconfig to just config. Edit configuration.scm file inside to point to solr url. Copy types folder next to the jar. To run the application, change working directory to the jar's folder, and execute `java -jar r7rs-index-<version>.jar`.

## Type definition format

Types are defined in types folder. The `index.scm` acts as a root reference on where types for each library as stored. Each library file is a list of entries, where each entry itself consists of 2 - 5 elements

* function name,
* function signature,
* list of tags,
* list of signatures for parameters, giving extra detail for `procedure?` types. Each entry is a list, where first element is a parameter name, and second element is its signature (same shape as and function signature in the second point),
* list of super types, if the current function being described is a predicate, and can be reasonably equivalted to a type.
