FROM ubuntu:rolling

COPY docker/admin/install.bash /tmp/
RUN /tmp/install.bash

WORKDIR /src
COPY docker/admin/pid1.bash /usr/local/sbin/
ENTRYPOINT ["/usr/local/sbin/pid1.bash"]
CMD ["bash"]
