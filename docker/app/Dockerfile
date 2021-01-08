FROM riju:compile AS compile
FROM riju:composite

ENTRYPOINT ["/usr/local/sbin/my_init"]
RUN useradd -p '!' -m -l -s /usr/bin/bash riju

WORKDIR /src

COPY --chown=riju:riju --from=compile /src ./
RUN chown root:riju system/out/*-privileged && chmod a=,g=rx,u=rwxs system/out/*-privileged

USER riju
CMD ["make", "server"]
ENV RIJU_ASSUME_SINGLE_PROCESS 1
