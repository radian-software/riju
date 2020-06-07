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
apt-get install -y curl gnupg lsb-release
rm -rf /var/lib/apt/lists/*

curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
curl -sS https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -

tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<"EOF"
deb https://deb.nodesource.com/node_14.x focal main
deb https://dl.yarnpkg.com/debian/ stable main
deb-src https://deb.nodesource.com/node_14.x
EOF

packages="

# Needed for project infrastructure
bash
git
make
nodejs
yarn

# Handy utilities
bsdmainutils
curl
emacs-nox
git
lsof
make
man-db
nano
sudo
tmux
vim
wget

# Bash
bash

# C/C++
clang

# Clojure
clojure

# Emacs Lisp
emacs-nox

# Fish
fish

# Go
golang

# Haskell
cabal-install
ghc

# Java
default-jdk

# Julia
julia

# Lua
lua5.3

# Node.js
nodejs
yarn

# Python
python3
python3-pip
python3-venv

# Ruby
ruby

# Rust
rustc

# Vimscript
vim

# Zsh
zsh

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

touch /home/docker/.zshrc
chown docker:docker /home/docker/.zshrc

rm "$0"
