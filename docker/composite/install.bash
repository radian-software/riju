#!/usr/bin/env bash

set -euxo pipefail

function riju-curl {
    curl -fsSL "localhost:8487$1"
}

function riju-apt-install {
    riju-curl "$1" > "$(basename "$1")"
    apt-get reinstall -y "./$(basename "$1")"
}

pushd /tmp

export DEBIAN_FRONTEND=noninteractive

apt-get update

riju-curl /shared | while read lang; do
    riju-apt-install "/fs/build/shared/${lang}/riju-shared-${lang}.deb"
done

riju-curl /langs | while read lang; do
    riju-apt-install "/fs/build/lang/${lang}/riju-lang-${lang}.deb"
    riju-apt-install "/fs/build/config/${lang}/riju-config-${lang}.deb"
done

rm -rf *.deb
rm -rf /var/lib/apt/lists/*

popd

rm "$0"
