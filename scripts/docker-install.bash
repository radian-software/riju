#!/usr/bin/env bash

set -e
set -o pipefail

if (( $# != 1 )); then
    echo "usage: docker-install.bash UID" >&2
    exit 1
fi

uid="$1"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y apt-transport-https curl gnupg lsb-release
rm -rf /var/lib/apt/lists/*

curl -sSL https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
curl -sSL https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
curl -sSL https://keybase.io/crystal/pgp_keys.asc | apt-key add -
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

cd /tmp
wget https://packages.microsoft.com/config/ubuntu/20.04/packages-microsoft-prod.deb
dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb

tee -a /etc/apt/sources.list.d/custom.list >/dev/null <<"EOF"
deb https://cloud.r-project.org/bin/linux/ubuntu focal-cran40/
deb https://deb.nodesource.com/node_14.x focal main
deb https://dist.crystal-lang.org/apt crystal main
deb https://dl.yarnpkg.com/debian/ stable main
deb-src https://deb.nodesource.com/node_14.x
EOF

packages="

# Needed for project infrastructure
bash
git
make
nodejs
yarn

# Handy utilities
bsdmainutils
curl
emacs-nox
git
lsof
make
man-db
nano
sudo
tmux
vim
wget

# Bash
bash

# C/C++
clang

# C#
mono-mcs

# Clojure
clojure

# Crystal
crystal

# Elixir
elixir

# Emacs Lisp
emacs-nox

# F#
fsharp

# Fish
fish

# Go
golang

# Haskell
cabal-install
ghc

# Java
default-jdk

# Julia
julia

# Lua
lua5.3

# Node.js
nodejs
yarn

# Python
python3
python3-pip
python3-venv

# R
r-base

# Ruby
ruby

# Rust
rustc

# Scheme
mit-scheme

# Swift
libpython2.7

# Vimscript
vim

# Zsh
zsh

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

cd /tmp
wget -nv https://github.com/watchexec/watchexec/releases/download/1.13.1/watchexec-1.13.1-x86_64-unknown-linux-gnu.deb
dpkg -i watchexec-*.deb
rm watchexec-*.deb

cd /tmp
wget -nv https://github.com/JetBrains/kotlin/releases/download/v1.3.72/kotlin-compiler-1.3.72.zip
unzip kotlin-*.zip
cp kotlinc/bin/* /usr/bin/
cp kotlinc/lib/* /usr/lib/
rm -rf kotlin-*.zip kotlinc

cd /tmp
wget -nv https://swift.org/builds/swift-5.2.4-release/ubuntu2004/swift-5.2.4-RELEASE/swift-5.2.4-RELEASE-ubuntu20.04.tar.gz
mkdir /opt/swift
tar -xf swift-*.tar.gz -C /opt/swift --strip-components=2
ln -s /opt/swift/bin/swiftc /usr/bin/swiftc
rm swift-*.tar.gz

if (( "$uid" != 0 )); then
    useradd --uid="$uid" --create-home --groups sudo docker
    passwd -d docker
else
    ln -s /root /home/docker
fi

touch /home/docker/.zshrc
chown docker:docker /home/docker/.zshrc

rm "$0"
