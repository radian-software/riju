#!/usr/bin/env bash

set -euxo pipefail

latest_release() {
    curl -sSL "https://api.github.com/repos/$1/releases/latest" | jq -r .tag_name
}

pushd /tmp

export DEBIAN_FRONTEND=noninteractive

dpkg --add-architecture i386

apt-get update
(yes || true) | unminimize

apt-get install -y curl gnupg lsb-release wget

# Ceylon
wget https://cacerts.digicert.com/DigiCertTLSRSASHA2562020CA1.crt.pem -O /usr/local/share/ca-certificates/DigiCertTLSRSASHA2562020CA1.crt

# D
wget https://letsencrypt.org/certs/lets-encrypt-r3.pem -O /usr/local/share/ca-certificates/lets-encrypt-r3.crt

update-ca-certificates

ubuntu_ver="$(lsb_release -rs)"
ubuntu_name="$(lsb_release -cs)"

cran_repo="$(curl -fsSL https://cran.r-project.org/bin/linux/ubuntu/ | grep '<tr>' | grep "${ubuntu_name}" | grep -Eo 'cran[0-9]+' | head -n1)"
node_repo="$(curl -fsSL https://deb.nodesource.com/setup_current.x | grep NODEREPO= | grep -Eo 'node_[0-9]+\.x' | head -n1)"

# .NET
wget "https://packages.microsoft.com/config/ubuntu/${ubuntu_ver}/packages-microsoft-prod.deb"
apt-get install ./packages-microsoft-prod.deb

# Ceylon
curl -fsSL https://downloads.ceylon-lang.org/apt/ceylon-debian-repo.gpg.key | apt-key add -

# Crystal
curl -fsSL https://keybase.io/crystal/pgp_keys.asc | apt-key add -

# Dart
curl -fsSL https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -

# Hack
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys B4112585D386EB94

# MongoDB
curl -fsSL https://www.mongodb.org/static/pgp/server-4.4.asc | apt-key add -

# Node.js
curl -fsSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -

# R
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

# Yarn
curl -fsSL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -

tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<EOF
# Ceylon
deb [arch=amd64] https://downloads.ceylon-lang.org/apt/ unstable main

# Crystal
deb [arch=amd64] https://dist.crystal-lang.org/apt crystal main

# Dart
deb [arch=amd64] https://storage.googleapis.com/download.dartlang.org/linux/debian stable main

# Hack
deb [arch=amd64] https://dl.hhvm.com/ubuntu ${ubuntu_name} main

# MongoDB
deb [arch=amd64] https://repo.mongodb.org/apt/ubuntu focal/mongodb-org/4.4 multiverse

# Node.js
deb [arch=amd64] https://deb.nodesource.com/${node_repo} ${ubuntu_name} main

# R
deb [arch=amd64] https://cloud.r-project.org/bin/linux/ubuntu ${ubuntu_name}-${cran_repo}/

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

# project tools
yarn

# packaging tools
apt-file
dctrl-tools

# basic utilities
less
git
jq
make
man
ripgrep
strace
sudo
tmux
vim

# shared dependencies
${libicu}

"

apt-get install -y $(sed 's/#.*//' <<< "${packages}")

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
