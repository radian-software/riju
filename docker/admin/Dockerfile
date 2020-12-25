FROM ubuntu:rolling

COPY docker/admin/install.bash /tmp/
RUN /tmp/install.bash

WORKDIR /src
COPY docker/shared/my_init docker/admin/pid1.bash /usr/local/sbin/
ENTRYPOINT ["/usr/local/sbin/my_init", "/usr/local/sbin/pid1.bash"]
CMD ["bash"]
