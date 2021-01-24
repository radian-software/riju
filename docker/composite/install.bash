#!/usr/bin/env bash

set -euxo pipefail

shard="$1"

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

riju-curl "/shard/${shard}" | while read path; do
    riju-apt-install "/fs/${path}"
done

rm -rf *.deb
rm -rf /var/lib/apt/lists/*

popd

rm "$0"
