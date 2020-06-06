#!/usr/bin/env bash

set -e
set -o pipefail

if (( $# != 1 )); then
    echo "usage: docker-install.bash UID" >&2
    exit 1
fi

uid="$1"

packages="

# Handy utilities
bsdmainutils
curl
emacs-nox
git
make
nano
vim
wget

# C/C++
clang

# Haskell
cabal-install
ghc

# Node.js
nodejs
npm

# Python
python3
python3-pip
python3-venv

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

if (( "$uid" != 0 )); then
    useradd --uid="$uid" --create-home --groups sudo docker
    passwd -d docker
else
    ln -s /root /home/docker
fi

rm "$0"
