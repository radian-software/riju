FROM riju:ubuntu

COPY docker/packaging/install.bash /tmp/
RUN /tmp/install.bash

WORKDIR /src
COPY docker/shared/my_init docker/packaging/pid1.bash /usr/local/sbin/
ENTRYPOINT ["/usr/local/sbin/my_init", "--quiet", "--skip-runit", "--", "/usr/local/sbin/pid1.bash"]
CMD ["bash"]
