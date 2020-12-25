FROM ubuntu:rolling AS build

COPY docker/runtime/install.bash /tmp/
RUN /tmp/install.bash

WORKDIR /src
ENV RIJU_PRIVILEGED=1

COPY package.json yarn.lock ./
RUN yarn install

COPY webpack.config.cjs ./
COPY frontend/src ./frontend/src/
RUN make frontend

COPY system ./system/
RUN make system

COPY frontend/pages ./frontend/pages/
COPY frontend/styles ./frontend/styles/
COPY backend ./backend/
