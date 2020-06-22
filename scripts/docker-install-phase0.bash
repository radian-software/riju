#!/usr/bin/env bash

set -e
set -o pipefail
set -x

export DEBIAN_FRONTEND=noninteractive
apt-get update
(yes || true) | unminimize
rm -rf /var/lib/apt/lists/*

rm "$0"
