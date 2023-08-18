#!/usr/bin/env bash

set -euo pipefail

if [[ ! -d system/src ]]; then
    echo "compile.bash: no system/src directory" >&2
    exit 1
fi

function verbosely {
    echo >&2 "$@"
    "$@"
}

mkdir -p system/out
rm -f system/out/*

for src in system/src/*.c; do
    out="${src/src/out}"
    out="${out/.c}"
    verbosely clang -Isystem/res -Wall -Wextra -Werror -std=c11 "${src}" -o "${out}"
done
