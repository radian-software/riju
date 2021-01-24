#!/usr/bin/env bash

set -euxo pipefail

mkdir /tmp/riju-work
pushd /tmp/riju-work

export DEBIAN_FRONTEND=noninteractive

dpkg --add-architecture i386

apt-get update
(yes || true) | unminimize

apt-get install -y curl gnupg lsb-release wget

wget https://cacerts.digicert.com/DigiCertTLSRSASHA2562020CA1.crt.pem -O /usr/local/share/ca-certificates/DigiCertTLSRSASHA2562020CA1.crt
wget https://letsencrypt.org/certs/lets-encrypt-r3.pem -O /usr/local/share/ca-certificates/lets-encrypt-r3.crt

update-ca-certificates

ubuntu_ver="$(lsb_release -rs)"
ubuntu_name="$(lsb_release -cs)"

node_repo="$(curl -sS https://deb.nodesource.com/setup_current.x | grep NODEREPO= | grep -Eo 'node_[0-9]+\.x' | head -n1)"

wget "https://packages.microsoft.com/config/ubuntu/${ubuntu_ver}/packages-microsoft-prod.deb"
apt-get install ./packages-microsoft-prod.deb

curl -fsSL https://downloads.ceylon-lang.org/apt/ceylon-debian-repo.gpg.key | apt-key add -
curl -fsSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
curl -fsSL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -

tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<EOF
deb [arch=amd64] https://downloads.ceylon-lang.org/apt/ unstable main
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
