#!/usr/bin/env bash

set -e
set -o pipefail
set -x

packages="

# Scala
scala

# Scheme
mit-scheme

# Sed
sed

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

# TeX
liblua5.3-dev
luarocks
texlive-binaries

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

# YAML
jq

# Zot
qt5-qmake
qtscript5-dev

# Zsh
zsh

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

rm "$0"
