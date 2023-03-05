FROM node:19
COPY frontend /app
WORKDIR /app
RUN npm install && npm run build

FROM node:19
COPY client /app
WORKDIR /app
RUN npm install && npm run build

FROM asciidoctor/docker-asciidoctor:1.39
WORKDIR /docs
COPY README.adoc .
COPY searchresult.schema.json .
RUN asciidoctor README.adoc

FROM nginx:1.21
COPY --from=0 /app/dist/frontend /www/data
COPY --from=1 /app/dist /www/data/downloads
COPY --from=2 /docs/README.html /www/data/
COPY ./static/downloads.json /www/data/
COPY ./screenshot.png /www/data/
COPY ./docker/nginx/nginx.conf /etc/nginx/nginx.conf
