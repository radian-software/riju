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

pushd system/res >/dev/null
verbosely xxd -i sentinel.bash > ../src/sentinel.h
popd >/dev/null

for src in system/src/*.c; do
    out="${src/src/out}"
    out="${out/.c}"
    verbosely clang -Isystem/res -Wall -Wextra -Werror -std=c11 "${src}" -o "${out}"
    if [[ "${out}" == *-privileged && -z "${UNPRIVILEGED:-}" ]]; then
        verbosely sudo chown root:riju "${out}"
        verbosely sudo chmod a=,g=rx,u=rwxs "${out}"
    fi
done
