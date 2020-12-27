#!/usr/bin/env bash

set -euxo pipefail

latest_release() {
    curl -sSL "https://api.github.com/repos/$1/releases/latest" | jq -r .tag_name
}

pushd /tmp

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

apt-get update
apt-get install -y dctrl-tools

libicu="$(grep-aptavail -wF Package 'libicu[0-9]+' -s Package -n | head -n1)"

apt-get update
apt-get install -y less clang jq "${libicu}" make man nodejs sudo tmux wget yarn

ver="$(latest_release watchexec/watchexec)"
wget "https://github.com/watchexec/watchexec/releases/download/${ver}/watchexec-${ver}-x86_64-unknown-linux-gnu.deb"
apt-get install -y ./watchexec-*.deb
rm watchexec-*.deb

rm -rf /var/lib/apt/lists/*

tee /etc/sudoers.d/90-riju >/dev/null <<"EOF"
%sudo ALL=(ALL:ALL) NOPASSWD: ALL
EOF

mkdir -p /opt/riju/langs
touch /opt/riju/langs/.keep

popd

rm "$0"
