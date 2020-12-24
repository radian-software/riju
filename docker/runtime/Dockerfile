FROM ubuntu:rolling

COPY docker/runtime/install.bash /tmp/
RUN /tmp/install.bash

WORKDIR /src
COPY docker/runtime/pid1.bash /usr/local/sbin/
ENTRYPOINT ["/usr/local/sbin/pid1.bash"]
CMD ["bash"]
