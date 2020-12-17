#!/usr/bin/env bash

set -e
set -o pipefail
set -x
pushd /tmp >/dev/null

dpkg --add-architecture i386

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y apt-transport-https curl gnupg libc6 libc6:i386 lsb-release software-properties-common wget
rm -rf /var/lib/apt/lists/*

ubuntu_ver="$(lsb_release -rs)"
ubuntu_name="$(lsb_release -cs)"

curl -sSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
curl -sSL https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
curl -sSL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
curl -sSL https://downloads.ceylon-lang.org/apt/ceylon-debian-repo.gpg.key | apt-key add -
curl -sSL https://keybase.io/crystal/pgp_keys.asc | apt-key add -
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys B4112585D386EB94

wget "https://packages.microsoft.com/config/ubuntu/${ubuntu_ver}/packages-microsoft-prod.deb"
dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb

nodesource="$(curl -sS https://deb.nodesource.com/setup_current.x | grep NODEREPO= | grep -Eo 'node_[0-9]+\.x' | head -n1)"
cran="$(curl -sS https://cran.r-project.org/bin/linux/ubuntu/ | grep '<tr>' | grep focal | grep -Eo 'cran[0-9]+' | head -n1)"

tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<EOF
deb [arch=amd64] https://storage.googleapis.com/download.dartlang.org/linux/debian stable main
deb https://cloud.r-project.org/bin/linux/ubuntu ${ubuntu_name}-${cran}/
deb https://deb.nodesource.com/${nodesource} ${ubuntu_name} main
deb https://dist.crystal-lang.org/apt crystal main
deb https://dl.hhvm.com/ubuntu ${ubuntu_name} main
deb https://dl.yarnpkg.com/debian/ stable main
deb https://downloads.ceylon-lang.org/apt/ unstable main
EOF

popd >/dev/null
rm "$0"
