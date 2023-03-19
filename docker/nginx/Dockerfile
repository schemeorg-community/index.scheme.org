FROM node:18
COPY client /app
WORKDIR /app/common
RUN npm install && npm run build
WORKDIR /app/cli
RUN npm install && npm run build
WORKDIR /app/frontend
RUN npm install && npm run build

FROM asciidoctor/docker-asciidoctor:1.39
WORKDIR /docs
COPY README.adoc .
COPY searchresult.schema.json .
RUN asciidoctor README.adoc

FROM nginx:1.21
COPY --from=0 /app/frontend/dist/frontend /www/data
COPY --from=0 /app/cli/dist /www/data/downloads
COPY --from=1 /docs/README.html /www/data/
COPY ./static/downloads.json /www/data/
COPY ./screenshot.png /www/data/
COPY ./docker/nginx/nginx.conf /etc/nginx/nginx.conf
