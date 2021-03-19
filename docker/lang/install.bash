#!/usr/bin/env bash

set -euo pipefail

: "${LANG}"

mkdir /tmp/riju-work
pushd /tmp/riju-work

function riju-curl {
    echo >&2 "fetching ./$1"
    curl -fsSL "localhost:8487/fs/$1"
}

export DEBIAN_FRONTEND=noninteractive

riju-curl "build/lang/${LANG}/riju-lang-${LANG}.deb" > "riju-lang-${LANG}.deb"

(
    dpkg-deb -f "riju-lang-${LANG}.deb" -f Depends |
        (grep -Eo 'riju-shared-[^, ]+' || true) |
        sed 's/riju-shared-//'
) | while read name; do
    riju-curl "build/shared/${name}/riju-shared-${name}.deb" > "riju-shared-${name}.deb"
done

if dpkg-deb -f "riju-lang-${LANG}.deb" -f Depends | grep .; then
    apt-get update
fi

for file in ./riju-shared-*.deb; do
    apt-get install -y "${file}"
done

apt-get install -y "./riju-lang-${LANG}.deb"

popd
rm -rf /tmp/riju-work

rm "$0"
