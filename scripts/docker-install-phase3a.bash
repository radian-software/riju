#!/usr/bin/env bash

set -e
set -o pipefail
set -x

packages="

# Ada
gnat

# Algol
algol68g

# APL
libtinfo5

# ARM
gcc-arm-linux-gnueabihf
qemu-user-static

# AsciiDoc
asciidoc

# ATS
ats2-lang

# Awk
mawk

# BASIC
bwbasic

# Bash
bash

# BrainF
beef

# C/C++
clang
clangd

# C#
mono-mcs

# Ceylon
openjdk-8-jdk-headless

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

# Dhall
dhall

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

rm "$0"
