FROM ubuntu:rolling AS build

COPY docker/app/install-build.bash /tmp/
RUN /tmp/install-build.bash

WORKDIR /src
COPY Makefile ./

COPY system ./system/
RUN make system

COPY package.json yarn.lock ./
RUN yarn install

COPY webpack.config.cjs ./
COPY frontend/src ./frontend/src/
RUN make frontend

COPY frontend/pages ./frontend/pages/
COPY frontend/styles ./frontend/styles/
COPY backend ./backend/

FROM ubuntu:rolling

COPY docker/app/install.bash /tmp/
RUN /tmp/install.bash

COPY docker/shared/my_init /usr/local/sbin/
ENTRYPOINT ["/usr/local/sbin/my_init", "--quiet", "--"]

RUN useradd -p '!' -m -l -s /usr/bin/bash riju

WORKDIR /src
COPY --chown=riju:riju --from=build /src ./
RUN chown root:riju system/out/*-privileged && chmod a=,g=rx,u=rwxs system/out/*-privileged

USER riju
CMD ["make", "server"]
ENV RIJU_ASSUME_SINGLE_PROCESS 1
