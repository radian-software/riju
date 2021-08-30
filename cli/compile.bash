#!/usr/bin/env bash

set -euo pipefail

function verbosely {
    echo "$@"
    "$@"
}

cd cli
mkdir -p build/go out

export GOCACHE="$PWD/build/go/cache"
export GOMODCACHE="$PWD/build/go/mod"

verbosely go build -o out/riju ./src
