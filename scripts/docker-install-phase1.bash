#!/usr/bin/env bash

set -e
set -o pipefail
set -x

dpkg --add-architecture i386

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y apt-transport-https curl gnupg lsb-release software-properties-common wget
rm -rf /var/lib/apt/lists/*

curl -sSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
curl -sSL https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
curl -sSL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
curl -sSL https://keybase.io/crystal/pgp_keys.asc | apt-key add -
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

cd /tmp
wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb
dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb

tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<"EOF"
deb [arch=amd64] https://storage.googleapis.com/download.dartlang.org/linux/debian stable main
deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/
deb https://deb.nodesource.com/node_14.x focal main
deb https://dist.crystal-lang.org/apt crystal main
deb https://dl.yarnpkg.com/debian/ stable main
deb-src https://deb.nodesource.com/node_14.x focal main
EOF

add-apt-repository -y -n ppa:deadsnakes/ppa

rm "$0"
