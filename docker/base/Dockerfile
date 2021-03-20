FROM ubuntu:rolling

COPY docker/base/install.bash /tmp/
RUN /tmp/install.bash

WORKDIR /src
COPY docker/shared/my_init /usr/local/sbin/
ENTRYPOINT ["/usr/local/sbin/my_init", "--quiet", "--"]
CMD ["bash"]
