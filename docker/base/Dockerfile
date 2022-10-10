# EOL: April 2027
FROM ubuntu:22.04

COPY docker/base/install.bash /tmp/
RUN /tmp/install.bash

RUN useradd -p '!' -m -l -s /usr/bin/bash riju
RUN runuser -u riju -- mkdir /home/riju/src
WORKDIR /home/riju/src

COPY docker/shared/my_init /usr/local/sbin/
ENTRYPOINT ["/usr/local/sbin/my_init", "--quiet", "--skip-runit", "--"]
CMD ["bash"]
