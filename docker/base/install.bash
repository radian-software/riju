#!/usr/bin/env bash

set -euxo pipefail

latest_release() {
    curl -sSL "https://api.github.com/repos/$1/releases/latest" | jq -r .tag_name
}

mkdir /tmp/riju-work
pushd /tmp/riju-work

export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get dist-upgrade -y
(yes || true) | unminimize

apt-get install -y curl gnupg lsb-release wget

ubuntu_name="$(lsb_release -cs)"

node_repo="$(curl -fsSL https://deb.nodesource.com/setup_current.x | grep NODEREPO= | grep -Eo 'node_[0-9]+\.x' | head -n1)"

# Node.js
curl -fsSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -

# Yarn
curl -fsSL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -

tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<EOF
# Node.js
deb [arch=amd64] https://deb.nodesource.com/${node_repo} ${ubuntu_name} main

# Yarn
deb [arch=amd64] https://dl.yarnpkg.com/debian/ stable main
EOF

apt-get update
apt-get install -y dctrl-tools

libicu="$(grep-aptavail -wF Package 'libicu[0-9]+' -s Package -n | head -n1)"

packages="

# compilation tools
clang
g++
gcc
make

# base languages
nodejs
ocaml
perl
python3
ruby

# packaging tools
apt-file
dctrl-tools

# basic utilities
bind9-dnsutils
less
git
htop
jq
make
man
moreutils
psmisc
ripgrep
strace
sudo
tmux
tree
vim

# shared dependencies
${libicu}

"

apt-get install -y $(sed 's/#.*//' <<< "${packages}")

rm -rf /var/lib/apt/lists/*

tee /etc/sudoers.d/90-riju >/dev/null <<"EOF"
%sudo ALL=(ALL:ALL) NOPASSWD: ALL
EOF

popd
rm -rf /tmp/riju-work

rm "$0"
