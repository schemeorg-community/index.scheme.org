FROM schemers/chicken:5 as chicken
RUN chicken-install json matchable srfi-1
COPY ./scheme-index-util /app/scheme-index-util
COPY ./types /app/types
COPY ./filters /app/filters
WORKDIR /app/scheme-index-util
RUN csc buildtypes.scm
RUN csc buildfilters.scm
WORKDIR /app
RUN ./scheme-index-util/buildtypes types/index.scm ./scheme-index-util/types.json
RUN ./scheme-index-util/buildfilters filters/index.scm ./scheme-index-util/filters.json

FROM node:22 as node
RUN apt-get update && apt-get install zip
COPY ./scheme-index-app/package.json /app/scheme-index-app/package.json
COPY ./scheme-index-app/package-lock.json /app/scheme-index-app/package-lock.json
WORKDIR /app/scheme-index-app
RUN npm install
COPY . /app
COPY --from=chicken /app/scheme-index-util/types.json /app/scheme-index-app/src/assets/types.json
COPY --from=chicken /app/scheme-index-util/filters.json /app/scheme-index-app/src/assets/filters.json
RUN npm run build
WORKDIR /app/scheme-index-app/dist/scheme-index
RUN zip -r ../scheme-index.zip *

FROM nginx:1.27
COPY --from=node /app/scheme-index-app/dist/scheme-index /schemeindex
COPY --from=node /app/scheme-index-app/dist/scheme-index.zip /schemeindex.zip
RUN cp -r /schemeindex /usr/share/nginx/html
