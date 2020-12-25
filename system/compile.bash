#!/usr/bin/env bash

set -euo pipefail

if [[ ! -d system/src ]]; then
    echo "compile.bash: no system/src directory" >&2
    exit 1
fi

function verbosely {
    echo "$@"
    "$@"
}

mkdir -p system/out
rm -f system/out/*
for src in system/src/*.c; do
    out="${src/src/out}"
    out="${out/.c}"
    verbosely clang -Wall -Wextra -Werror -std=c11 "${src}" -o "${out}"
    if [[ "${out}" == *-privileged && -n "${RIJU_PRIVILEGED:-}" ]]; then
        sudo chown root:riju "${out}"
        sudo chmod a=,g=rx,u=rwxs "${out}"
    fi
done
