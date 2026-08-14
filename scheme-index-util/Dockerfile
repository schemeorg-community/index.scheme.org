FROM schemers/chicken:5 as chicken
RUN chicken-install json matchable srfi-1
COPY ./scheme-index-util /scheme-index-util
WORKDIR /scheme-index-util
RUN csc validatedata.scm
WORKDIR /
ENTRYPOINT ./scheme-index-util/validatedata filters/index.scm types/index.scm
