FROM riju:runtime

COPY docker/composite/install.bash /tmp/

# The number of commands here must match NUM_SHARDS in
# build-composite-image.js.
RUN /tmp/install.bash 0
RUN /tmp/install.bash 1
RUN /tmp/install.bash 2
RUN /tmp/install.bash 3
RUN /tmp/install.bash 4
RUN /tmp/install.bash 5
RUN /tmp/install.bash 6
RUN /tmp/install.bash 7
RUN /tmp/install.bash 8
RUN /tmp/install.bash 9

RUN rm /tmp/install.bash
