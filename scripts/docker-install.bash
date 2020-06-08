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
apt-get install -y apt-transport-https curl gnupg lsb-release wget
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
jq
lsof
make
man-db
nano
sudo
tmux
vim
wget

# BASIC
bwbasic

# Bash
bash

# BrainF
beef

# C/C++
clang

# C#
mono-mcs

# Clojure
clojure

# Crystal
crystal

# Dart
dart

# Elixir
elixir

# Elvish
elvish

# Emacs Lisp
emacs-nox

# Erlang
erlang

# F#
fsharp

# Fish
fish

# FORTRAN
flang-7

# Forth
gforth

# Go
golang

# Haskell
cabal-install
ghc

# Java
default-jdk

# Julia
julia

# Ksh
ksh

# LOLCODE
cmake

# Lua
lua5.3

# Nim
nim

# Node.js
nodejs
yarn

# Objective-C
gcc
gnustep-devel

# Perl
perl
perlconsole

# PHP
php

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

# Scala
scala

# Scheme
mit-scheme

# Sh
posh

# SQLite
sqlite

# Swift
libpython2.7

# Tcl
tcl

# Tcsh
tcsh

# Unlambda
unlambda

# Vimscript
vim

# Zsh
zsh

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

# CoffeeScript
npm install -g coffeescript

# TypeScript
npm install -g ts-node typescript

# ReasonML
npm install -g bs-platform

# Perl
cpan Devel::REPL

# Needed for project infrastructure
cd /tmp
wget -nv https://github.com/watchexec/watchexec/releases/download/1.13.1/watchexec-1.13.1-x86_64-unknown-linux-gnu.deb
dpkg -i watchexec-*.deb
rm watchexec-*.deb

# Kotlin
cd /tmp
wget -nv https://github.com/JetBrains/kotlin/releases/download/v1.3.72/kotlin-compiler-1.3.72.zip
unzip kotlin-*.zip
cp kotlinc/bin/* /usr/bin/
cp kotlinc/lib/* /usr/lib/
rm -rf kotlin-*.zip kotlinc

# PowerShell
cd /tmp
wget -nv https://github.com/PowerShell/PowerShell/releases/download/v7.0.1/powershell-7.0.1-linux-x64.tar.gz
mkdir /opt/powershell
tar -xf powershell-*.tar.gz -C /opt/powershell
ln -s /opt/powershell/pwsh /usr/bin/pwsh

# Swift
cd /tmp
wget -nv https://swift.org/builds/swift-5.2.4-release/ubuntu2004/swift-5.2.4-RELEASE/swift-5.2.4-RELEASE-ubuntu20.04.tar.gz
mkdir /opt/swift
tar -xf swift-*.tar.gz -C /opt/swift --strip-components=2
ln -s /opt/swift/bin/swiftc /usr/bin/swiftc
rm swift-*.tar.gz

# LOLCODE
cd /tmp
git clone https://github.com/justinmeza/lci.git
pushd lci >/dev/null
python3 install.py --prefix=/usr
popd >/dev/null
rm -rf lci

# BrainF
tee /usr/bin/brainf-repl >/dev/null <<"EOF"
#!/usr/bin/env python3
import argparse
import readline
import subprocess
import tempfile

parser = argparse.ArgumentParser()
parser.add_argument("file", nargs="?")
args = parser.parse_args()

if args.file:
    subprocess.run(["beef", args.file])
while True:
    try:
        code = input("bf> ")
    except KeyboardInterrupt:
        print("^C")
        continue
    except EOFError:
        print("^D")
        break
    if not code:
        continue
    with tempfile.NamedTemporaryFile(mode="w") as f:
        f.write(code)
        f.flush()
        subprocess.run(["beef", f.name])
EOF
chmod +x /usr/bin/brainf-repl

# Unlambda
tee /usr/bin/unlambda-repl >/dev/null <<"EOF"
#!/usr/bin/env python3
import argparse
import readline
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("file", nargs="?")
args = parser.parse_args()

if args.file:
    with open(args.file) as f:
        subprocess.run(["unlambda"], input=f.read(), encoding="utf-8")
while True:
    try:
        code = input("λ> ")
    except KeyboardInterrupt:
        print("^C")
        continue
    except EOFError:
        print("^D")
        break
    if not code:
        continue
    subprocess.run(["unlambda"], input=code, encoding="utf-8")
EOF
chmod +x /usr/bin/unlambda-repl

if (( "$uid" != 0 )); then
    useradd --uid="$uid" --create-home --groups sudo docker
    passwd -d docker
else
    ln -s /root /home/docker
fi

touch /home/docker/.zshrc
chown docker:docker /home/docker/.zshrc

rm "$0"
