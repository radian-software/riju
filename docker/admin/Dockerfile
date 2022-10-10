# EOL: April 2027
FROM ubuntu:22.04

COPY docker/admin/install.bash /tmp/
RUN /tmp/install.bash

WORKDIR /src
COPY docker/shared/my_init /usr/local/sbin/
COPY docker/shared/admin-pid1.bash /usr/local/sbin/pid1.bash
ENTRYPOINT ["/usr/local/sbin/my_init", "--quiet", "--skip-runit", "--", "/usr/local/sbin/pid1.bash"]
CMD ["bash"]
