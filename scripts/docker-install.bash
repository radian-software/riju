#!/usr/bin/env bash

set -e
set -o pipefail

if (( $# != 1 )); then
    echo "usage: docker-install.bash UID" >&2
    exit 1
fi

uid="$1"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y curl gnupg
rm -rf /var/lib/apt/lists/*

curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -

tee -a /etc/apt/sources.list.d/yarn.list >/dev/null <<"EOF"
deb https://dl.yarnpkg.com/debian/ stable main
EOF

packages="

# Handy utilities
bsdmainutils
curl
emacs-nox
git
make
nano
sudo
tmux
vim
wget

# C/C++
clang

# Haskell
cabal-install
ghc

# Node.js
nodejs
yarn

# Python
python3
python3-pip
python3-venv

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

cd /tmp
wget -nv https://github.com/watchexec/watchexec/releases/download/1.13.1/watchexec-1.13.1-x86_64-unknown-linux-gnu.deb
dpkg -i watchexec-*.deb
rm watchexec-*.deb

if (( "$uid" != 0 )); then
    useradd --uid="$uid" --create-home --groups sudo docker
    passwd -d docker
else
    ln -s /root /home/docker
fi

rm "$0"
