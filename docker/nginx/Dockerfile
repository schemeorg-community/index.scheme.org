FROM scmindex-builder:latest

FROM nginx:1.21
COPY --from=0 /app/static/ /www/data/
COPY --from=0 /app/README.html /www/data/README.html
COPY ./docker/nginx/nginx.conf /etc/nginx/nginx.conf
