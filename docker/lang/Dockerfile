FROM riju:base

ARG LANG

COPY docker/lang/install.bash /tmp/
RUN /tmp/install.bash

ENTRYPOINT ["/usr/local/sbin/my_init", "--quiet", "--"]
RUN rm /usr/local/sbin/pid1.bash

RUN useradd -p '!' -m -l -s /usr/bin/bash riju
WORKDIR /home/riju/src
USER riju
CMD ["bash"]
