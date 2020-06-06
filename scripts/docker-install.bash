#!/usr/bin/env bash

set -e
set -o pipefail

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

rm "$0"
