#!/usr/bin/env bash

set -euo pipefail

function verbosely {
    echo "$@"
    "$@"
}

cd supervisor
mkdir -p out
verbosely go build -o out/riju-supervisor ./src
