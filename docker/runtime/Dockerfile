FROM ubuntu:rolling

COPY docker/runtime/install.bash /tmp/
RUN /tmp/install.bash

WORKDIR /src
COPY docker/shared/my_init docker/runtime/pid1.bash /usr/local/sbin/
ENTRYPOINT ["/usr/local/sbin/my_init", "--quiet", "--", "/usr/local/sbin/pid1.bash"]
CMD ["bash"]
EXPOSE 6119
EXPOSE 6120
ENV HOST=0.0.0.0
