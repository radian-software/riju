# EOL: April 2027
FROM ubuntu:22.04 AS build

COPY docker/app/install-build.bash /tmp/
RUN /tmp/install-build.bash

WORKDIR /src
COPY Makefile ./

COPY package.json yarn.lock ./
RUN yarn install

COPY webpack.config.cjs ./
COPY frontend/src ./frontend/src/
RUN make frontend

COPY system ./system/
RUN make system UNPRIVILEGED=1

COPY frontend/pages ./frontend/pages/
COPY frontend/styles ./frontend/styles/
COPY lib ./lib/
COPY backend ./backend/
COPY langs ./langs/

FROM riju:runtime

ENTRYPOINT ["/usr/local/sbin/my_init", "--quiet", "--skip-runit","--"]
RUN useradd -p '!' -m -l -s /usr/bin/bash riju
COPY --chown=riju:riju --from=build /src ./
RUN chown root:riju system/out/*-privileged && chmod a=,g=rx,u=rwxs system/out/*-privileged

USER riju
CMD make server 2>&1 | ts
