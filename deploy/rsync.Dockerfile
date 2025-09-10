FROM ubuntu:24.04
RUN apt-get update && apt-get install -y ssh rsync
RUN useradd -ms /bin/bash indexupdater
USER indexupdater
