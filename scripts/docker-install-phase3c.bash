#!/usr/bin/env bash

set -e
set -o pipefail
set -x

packages="

# Markdown
pandoc
prettier

# MIPS
gcc-mips64-linux-gnuabi64
qemu-user-static

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

# Ocaml
ocaml

# Octave
octave

# Org
pandoc

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

# reStructuredText
pandoc

# RISC-V
gcc-riscv64-linux-gnu
qemu-user-static

# Ruby
ruby
ruby-dev

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

rm "$0"
