ARG REGISTRY_PREFIX

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
RUN make system

COPY frontend/pages ./frontend/pages/
COPY frontend/styles ./frontend/styles/
COPY lib ./lib/
COPY backend ./backend/
COPY langs ./langs/

FROM ${REGISTRY_PREFIX}runtime:latest

ENTRYPOINT ["/usr/local/sbin/my_init", "--quiet", "--skip-runit","--"]
RUN useradd -p '!' -m -l -s /usr/bin/bash riju
COPY --chown=riju:riju --from=build /src ./

USER riju
CMD make server 2>&1 | ts
