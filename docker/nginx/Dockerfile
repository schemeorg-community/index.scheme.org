FROM node:19
COPY frontend /app
WORKDIR /app
RUN npm install && npm run build

FROM asciidoctor/docker-asciidoctor:1.39
WORKDIR /docs
COPY README.adoc .
COPY searchresult.schema.json .
RUN asciidoctor README.adoc

FROM nginx:1.21
COPY --from=0 /app/dist/frontend /www/data
COPY --from=1 /docs/README.html /www/data/README.html
COPY ./screenshot.png /www/data/
COPY ./docker/nginx/nginx.conf /etc/nginx/nginx.conf
