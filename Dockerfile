FROM ubuntu:rolling

ARG UID

COPY scripts/docker-install.bash /tmp/
RUN /tmp/docker-install.bash "$UID"

USER $UID
WORKDIR /home/docker
EXPOSE 6119

ENTRYPOINT ["/usr/local/bin/pid1.bash"]
COPY scripts/pid1.bash /usr/local/bin/
