#!/usr/bin/env bash

set -euxo pipefail

# See install.bash for the base image for much of the same, but with
# more comments.

mkdir /tmp/riju-work
pushd /tmp/riju-work

export DEBIAN_FRONTEND=noninteractive

apt-get update
apt-get dist-upgrade -y
(yes || true) | unminimize

apt-get install -y curl gnupg lsb-release wget

ubuntu_name="$(lsb_release -cs)"

node_repo="$(curl -sS https://deb.nodesource.com/setup_current.x | grep NODEREPO= | grep -Eo 'node_[0-9]+\.x' | head -n1)"

curl -fsSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
curl -fsSL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -

tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<EOF
deb [arch=amd64] https://deb.nodesource.com/${node_repo} ${ubuntu_name} main
deb [arch=amd64] https://dl.yarnpkg.com/debian/ stable main
EOF

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

# package managers
opam
python3-pip
yarn

# packaging tools
apt-file
dctrl-tools
fakeroot
unzip

# basic utilities
bind9-dnsutils
git
htop
jq
less
man
moreutils
psmisc
ripgrep
strace
sudo
tmux
tree
vim

"

apt-get update
apt-get install -y $(sed 's/#.*//' <<< "${packages}")

rm -rf /var/lib/apt/lists/*

PERL_MM_USE_DEFAULT=1 cpan App::cpanminus
rm -rf /tmp/cpan_install_*.txt

tee /etc/sudoers.d/90-riju >/dev/null <<"EOF"
%sudo ALL=(ALL:ALL) NOPASSWD: ALL
EOF

popd
rm -rf /tmp/riju-work

rm "$0"
