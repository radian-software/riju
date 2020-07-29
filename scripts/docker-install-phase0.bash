#!/usr/bin/env bash

set -e
set -o pipefail
set -x

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get dist-upgrade -y
(yes || true) | unminimize
rm -rf /var/lib/apt/lists/*

rm "$0"
