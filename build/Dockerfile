FROM schemers/chicken:5 as chicken
RUN chicken-install json matchable srfi-1
COPY ./chicken /app/chicken
COPY ./types /app/types
COPY ./filters /app/filters
WORKDIR /app/chicken
RUN csc builddb.scm
RUN csc buildfilters.scm
WORKDIR /app
RUN ./chicken/builddb types/index.scm ./chicken/types.json
RUN ./chicken/buildfilters filters/index.scm ./chicken/filters.json

FROM node:22 as node
COPY . /app
COPY --from=chicken /app/chicken/types.json /app/client/frontend/src/assets/types.json
COPY --from=chicken /app/chicken/filters.json /app/client/frontend/src/assets/filters.json
WORKDIR /app/client/frontend
RUN npm install
RUN npm run build

FROM nginx:1.27
COPY --from=node /app/client/frontend/dist/frontend /usr/share/nginx/html
