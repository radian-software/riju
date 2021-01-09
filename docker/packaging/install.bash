#!/usr/bin/env bash

set -euxo pipefail

export DEBIAN_FRONTEND=noninteractive

apt-get update
(yes || true) | unminimize

apt-get install -y curl gnupg lsb-release

curl -sSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
curl -sSL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -

ubuntu_ver="$(lsb_release -rs)"
ubuntu_name="$(lsb_release -cs)"

node_repo="$(curl -sS https://deb.nodesource.com/setup_current.x | grep NODEREPO= | grep -Eo 'node_[0-9]+\.x' | head -n1)"

tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<EOF
deb https://deb.nodesource.com/${node_repo} ${ubuntu_name} main
deb https://dl.yarnpkg.com/debian/ stable main
EOF

packages="

fakeroot
git
less
make
man
nodejs
ripgrep
sudo
tmux
unzip
vim
wget
yarn

"

apt-get update
apt-get install -y $(sed 's/#.*//' <<< "${packages}")

rm -rf /var/lib/apt/lists/*

tee /etc/sudoers.d/90-riju >/dev/null <<"EOF"
%sudo ALL=(ALL:ALL) NOPASSWD: ALL
EOF

rm "$0"
