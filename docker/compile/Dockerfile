FROM ubuntu:rolling

COPY docker/compile/install.bash /tmp/
RUN /tmp/install.bash

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
