#!/usr/bin/env bash

set -euxo pipefail

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get dist-upgrade -y

apt-get install -y certbot

rm -rf /var/lib/apt/lists/*
