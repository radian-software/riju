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

riju-curl "build/lang/${LANG}/install.bash" > "install-lang-${LANG}.bash"
riju-curl "build/lang/${LANG}/riju-lang-${LANG}.deb" > "riju-lang-${LANG}.deb"
chmod +x "install-lang-${LANG}.bash"

(
    dpkg-deb -f "riju-lang-${LANG}.deb" -f Depends |
        (grep -Eo 'riju-shared-[^, ]+' || true) |
        sed 's/riju-shared-//'
) | while read name; do
    riju-curl "build/shared/${name}/install.bash" > "install-shared-${name}.bash"
    riju-curl "build/shared/${name}/riju-shared-${name}.deb" > "riju-shared-${name}.deb"
    chmod +x "install-shared-${name}.bash"
done

if compgen -G "./install-shared-*.bash"; then
    for file in ./install-shared-*.bash; do
        "${file}"
    done
fi

"./install-lang-${LANG}.bash"

if dpkg-deb -f "riju-lang-${LANG}.deb" -f Depends | grep .; then
    apt-get update
fi

if compgen -G "./riju-shared-*.deb"; then
    for file in ./riju-shared-*.deb; do
        apt-get install -y "${file}"
    done
fi

apt-get install -y "./riju-lang-${LANG}.deb"

popd
rm -rf /tmp/riju-work

rm "$0"
