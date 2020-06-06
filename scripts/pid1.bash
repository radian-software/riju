#!/usr/bin/env bash

set -e
set -o pipefail

export LANG=C.UTF-8
export LC_ALL=C.UTF-8
export SHELL="$(which bash)"

if [[ -d src ]]; then
    cd src
fi

exec "$@"
