FROM riju:ubuntu

COPY docker/runtime/install.bash /tmp/
RUN /tmp/install.bash

COPY docker/shared/my_init docker/runtime/pid1.bash /usr/local/sbin/
ENTRYPOINT ["/usr/local/sbin/my_init", "--quiet", "--skip-runit", "--", "/usr/local/sbin/pid1.bash"]

WORKDIR /src
CMD ["bash"]
EXPOSE 6119
EXPOSE 6120
EXPOSE 6121
ENV HOST=0.0.0.0
