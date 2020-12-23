FROM ubuntu:rolling

COPY docker/packaging/install.bash /tmp/
RUN /tmp/install.bash

WORKDIR /src
COPY docker/packaging/pid1.bash /usr/local/sbin/
ENTRYPOINT ["/usr/local/sbin/pid1.bash"]
CMD ["bash"]
