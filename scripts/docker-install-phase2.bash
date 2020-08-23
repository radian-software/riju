#!/usr/bin/env bash

set -e
set -o pipefail
set -x

packages="

# Needed for project infrastructure
bash
dctrl-tools
git
make
nodejs
python3-pip
yarn

# Handy utilities
apt-file
bbe
bsdmainutils
curl
emacs-nox
git
httpie
htop
jq
lsof
make
man-db
moreutils
nano
ncdu
iputils-ping
ripgrep
strace
sudo
tmux
trash-cli
tree
vim
wget

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

rm "$0"
