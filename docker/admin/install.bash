#!/usr/bin/env bash

set -euxo pipefail

latest_release() {
    curl -sSL "https://api.github.com/repos/$1/releases/latest" | jq -r .tag_name
}

mkdir /tmp/riju-work
pushd /tmp/riju-work

export DEBIAN_FRONTEND=noninteractive

apt-get update
(yes || true) | unminimize

apt-get install -y curl gnupg lsb-release

curl -fsSL https://apt.releases.hashicorp.com/gpg | apt-key add -
curl -fsSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
curl -fsSL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -

ubuntu_ver="$(lsb_release -rs)"
ubuntu_name="$(lsb_release -cs)"

node_repo="$(curl -sS https://deb.nodesource.com/setup_16.x | grep NODEREPO= | grep -Eo 'node_[0-9]+\.x' | head -n1)"

tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<EOF
deb [arch=amd64] https://apt.releases.hashicorp.com ${ubuntu_name} main
deb [arch=amd64] https://deb.nodesource.com/${node_repo} ${ubuntu_name} main
deb [arch=amd64] https://dl.yarnpkg.com/debian/ stable main
deb [arch=amd64] https://download.docker.com/linux/ubuntu ${ubuntu_name} stable
EOF

packages="

apt-file
bind9-dnsutils
black
clang
clang-format
dctrl-tools
docker-ce-cli
file
g++
gettext
git
golang
htop
httpie
jq
less
make
man
moreutils
nodejs
packer
psmisc
python3-pip
pwgen
skopeo
ssh
strace
sudo
terraform
tmux
tree
unzip
uuid-runtime
vim
wget
yarn

"

apt-get update
apt-get install -y $(sed 's/#.*//' <<< "${packages}")

pip3 install ec2instanceconnectcli poetry

npm install -g prettier

wget -nv https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip -O awscli.zip
unzip -q awscli.zip
./aws/install

ver="$(latest_release grafana/cortex-tools | sed 's/^v//')"
wget -nv "https://github.com/grafana/cortex-tools/releases/download/v${ver}/cortextool_${ver}_linux_amd64.tar.gz" -O cortextool.tar.gz
tar -xf cortextool.tar.gz
cp cortextool /usr/local/bin/

rm -rf /var/lib/apt/lists/*

tee /etc/sudoers.d/90-riju >/dev/null <<"EOF"
%sudo ALL=(ALL:ALL) NOPASSWD: ALL
EOF

popd
rm -rf /tmp/riju-work

rm "$0"
