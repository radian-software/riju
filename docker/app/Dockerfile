FROM riju:compile AS compile
FROM riju:composite

RUN useradd -p '!' -m -l -s /usr/bin/bash riju

WORKDIR /src

COPY --chown=riju:riju --from=compile . ./
RUN chown root:riju system/out/*-privileged && chmod a=,g=rx,u=rwxs system/out/*-privileged

USER riju
