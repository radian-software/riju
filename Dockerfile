FROM ubuntu:rolling

COPY scripts/docker-install.bash /tmp/
RUN /tmp/docker-install.bash

EXPOSE 6119
