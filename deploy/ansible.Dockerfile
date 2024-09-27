FROM python:3.9.20
RUN useradd -ms /bin/bash ansible
USER ansible
RUN pip install ansible
RUN mkdir -p /home/ansible/.ssh
