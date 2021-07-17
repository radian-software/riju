#!/usr/bin/env bash

set -euo pipefail

function verbosely {
    echo "$@"
    "$@"
}

cd supervisor
mkdir -p build/go out

export GOCACHE="$PWD/build/go/cache"
export GOMODCACHE="$PWD/build/go/mod"

verbosely go build -o out/riju-supervisor ./src
