FROM debian:stable
RUN apt-get update && apt-get -y install ant asciidoctor npm
RUN npm install -g sass
WORKDIR /app
COPY . .
RUN ant compile-css compile-doc

FROM nginx:1.21
COPY ./static /www/data/
COPY --from=0 /app/static/css/scmindex.css /www/data/css/scmindex.css
COPY --from=0 /app/README.html /www/data/README.html
COPY ./docker/nginx/nginx.conf /etc/nginx/nginx.conf
