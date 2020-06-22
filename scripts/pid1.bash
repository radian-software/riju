#!/usr/bin/env bash

set -e
set -o pipefail

export LANG=C.UTF-8
export LC_ALL=C.UTF-8
export SHELL="$(which bash)"

export HOST=0.0.0.0
export RIJU_PRIVILEGED=yes

if [[ -d /home/docker/src ]]; then
    cd /home/docker/src
fi

exec "$@"
