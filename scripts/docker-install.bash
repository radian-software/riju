#!/usr/bin/env bash

set -e
set -o pipefail

if (( $# != 1 )); then
    echo "usage: docker-install.bash UID" >&2
    exit 1
fi

uid="$1"

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

packages="

# Needed for project infrastructure
bash
git
make
nodejs
python3-pip
yarn

# Handy utilities
bsdmainutils
curl
emacs-nox
git
htop
jq
lsof
make
man-db
nano
sudo
tmux
vim
wget

# Ada
gnat

# Algol
algol68g

# ARM
gcc-arm-linux-gnueabihf
qemu-system-static

# ATS
ats2-lang

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

# Cmd
wine
wine32

# COBOL
gnucobol

# Common Lisp
rlwrap
sbcl

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

# Groovy
groovy

# Haskell
cabal-install
ghc

# INTERCAL
intercal

# Java
default-jdk

# Julia
julia

# Kalyn
haskell-stack

# Ksh
ksh

# LOLCODE
cmake

# Lua
lua5.3

# MIPS
gcc-mips64-linux-gnuabi64
qemu-system-static

# MUMPS
fis-gtm

# Nim
nim

# Node.js
nodejs
yarn

# Objective-C
gcc
gnustep-devel

# Octave
octave

# Pascal
fpc

# Perl
perl
perlconsole

# PHP
php

# Prolog
swi-prolog

# Python
python3
python3-pip
python3-venv

# R
r-base

# Racket
racket

# RISC-V
gcc-riscv64-linux-gnu
qemu-system-static

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

# Smalltalk
gnu-smalltalk

# SNOBOL
m4

# SQLite
sqlite

# Standard ML
rlwrap
smlnj

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

# Visual Basic
mono-vbnc

# Wolfram Language
python3.7

# x86
clang

# Zsh
zsh

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

npm config set unsafe-perm true

# Befunge
npm install -g befunge93 prompt-sync

# ClojureScript
npm install -g lumo-cljs

# CoffeeScript
npm install -g coffeescript

# Elm
npm install -g run-elm

# Perl
cpan Devel::REPL

# ReasonML
npm install -g bs-platform

# Shakespeare
pip3 install shakespearelang

# TypeScript
npm install -g ts-node typescript

# Whitespace
pip3 install whitespace

# Wolfram Language
python3.7 -m pip install install mathics

# Needed for project infrastructure
cd /tmp
wget -nv https://github.com/watchexec/watchexec/releases/download/1.13.1/watchexec-1.13.1-x86_64-unknown-linux-gnu.deb
dpkg -i watchexec-*.deb
rm watchexec-*.deb

# D
cd /tmp
wget -nv http://downloads.dlang.org/releases/2.x/2.092.0/dmd_2.092.0-0_amd64.deb

# Elm
cd /tmp
wget -nv https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
gunzip binary-for-linux-64-bit.gz
chmod +x binary-for-linux-64-bit
mv binary-for-linux-64-bit /usr/bin/elm
rm binary-for-linux-64-bit.gz

# Ink
cd /tmp
wget -nv https://github.com/thesephist/ink/releases/download/v0.1.7/ink-linux
wget -nv https://github.com/thesephist/ink/releases/download/v0.1.7/std.ink
wget -nv https://github.com/thesephist/ink/releases/download/v0.1.7/str.ink
chmod +x ink-linux
mv ink-linux /usr/bin/ink
mkdir /opt/ink
mv std.ink str.ink /opt/ink/

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

# SNOBOL
wget -nv ftp://ftp.snobol4.org/snobol/snobol4-2.0.tar.gz
tar -xf snobol4-*.tar.gz
rm snobol4-*.tar.gz
pushd snobol4-*
make || true
mv snobol4 /usr/bin/snobol4
popd
rm -rf snobol4-*

# Swift
cd /tmp
wget -nv https://swift.org/builds/swift-5.2.4-release/ubuntu2004/swift-5.2.4-RELEASE/swift-5.2.4-RELEASE-ubuntu20.04.tar.gz
mkdir /opt/swift
tar -xf swift-*.tar.gz -C /opt/swift --strip-components=2
ln -s /opt/swift/bin/swiftc /usr/bin/swiftc
rm swift-*.tar.gz

# Kalyn
cd /tmp
git clone https://github.com/raxod502/kalyn
pushd kalyn >/dev/null
stack build kalyn
mv "$(stack exec which kalyn)" /usr/bin/kalyn
mkdir /opt/kalyn
cp -R src-kalyn/Stdlib src-kalyn/Stdlib.kalyn /opt/kalyn/
popd
rm -rf kalyn

# LOLCODE
cd /tmp
git clone https://github.com/justinmeza/lci.git
pushd lci >/dev/null
python3 install.py --prefix=/usr
popd >/dev/null
rm -rf lci

# Malbolge
cd /tmp
git clone https://github.com/bipinu/malbolge.git
clang malbolge/malbolge.c -o /usr/bin/malbolge
rm -rf malbolge

# ActionScript
tee /usr/bin/amxmlc >/dev/null <<"EOF"
#!/bin/sh
exec /opt/actionscript/bin/amxmlc "$@"
EOF
chmod +x /usr/bin/amxmlc

# Befunge
tee /usr/bin/befunge-repl >/dev/null <<"EOF"
#!/usr/bin/env -S NODE_PATH=/usr/lib/node_modules node
const fs = require("fs");

const Befunge = require("befunge93");
const prompt = require("prompt-sync")();

const befunge = new Befunge();
befunge.onInput = prompt;
befunge.onOutput = (output) => {
  if (typeof output === "string") {
    process.stdout.write(output);
  } else {
    process.stdout.write(output + " ");
  }
};

const args = process.argv.slice(2);
if (args.length !== 1) {
  console.error("usage: befunge-repl FILE");
  process.exit(1);
}

befunge.run(fs.readFileSync(args[0], { encoding: "utf-8" })).catch((err) => {
  console.error(err);
  process.exit(1);
});
EOF
chmod +x /usr/bin/befunge-repl

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

# Elm
mkdir /opt/elm
tee /opt/elm/elm.json >/dev/null <<"EOF"
{
    "type": "application",
    "source-directories": [
        "."
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0"
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.2"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
EOF

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
