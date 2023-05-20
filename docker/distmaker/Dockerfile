FROM scheme-index-tmp

FROM scheme-index-nginx-tmp

FROM alpine:latest
RUN apk update; apk add zip
RUN mkdir /dist
COPY --from=0 /app /dist/app
RUN mkdir /dist/nginx
RUN mkdir /dist/nginx/conf
COPY --from=1 /www/data /dist/nginx/www/data
COPY --from=1 /etc/nginx/mime.types /dist/nginx/conf/mime.types
COPY nginx.conf /dist/nginx/conf/nginx.conf
COPY README.txt /dist/README.txt
RUN zip -r /dist.zip /dist
