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

curl -fsSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
curl -fsSL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -

ubuntu_name="$(lsb_release -cs)"

node_repo="$(curl -sS https://deb.nodesource.com/setup_current.x | grep NODEREPO= | grep -Eo 'node_[0-9]+\.x' | head -n1)"

tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<EOF
deb [arch=amd64] https://deb.nodesource.com/${node_repo} ${ubuntu_name} main
deb [arch=amd64] https://dl.yarnpkg.com/debian/ stable main
deb [arch=amd64] https://download.docker.com/linux/ubuntu ${ubuntu_name} stable
EOF

packages="

# project tools
clang
docker-ce-cli
make
nodejs
python3-pip
yarn

# packaging tools
apt-file
dctrl-tools

# basic utilities
bind9-dnsutils
less
git
golang
htop
httpie
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
uuid-runtime
vim

"

apt-get update
apt-get install -y $(sed 's/#.*//' <<< "${packages}")

pip3 install poetry

ver="$(latest_release watchexec/watchexec | sed 's/^cli-v//')"
wget "https://github.com/watchexec/watchexec/releases/download/cli-v${ver}/watchexec-${ver}-x86_64-unknown-linux-gnu.deb"
apt-get install -y ./watchexec-*.deb

rm -rf /var/lib/apt/lists/*

tee /etc/sudoers.d/90-riju >/dev/null <<"EOF"
%sudo ALL=(ALL:ALL) NOPASSWD: ALL
EOF

popd
rm -rf /tmp/riju-work

rm "$0"
