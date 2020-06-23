#!/usr/bin/env bash

set -e
set -o pipefail
set -x

packages="

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

# TeX
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

# Zsh
zsh

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

rm "$0"
