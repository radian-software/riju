#!/usr/bin/env bash

set -e
set -o pipefail

packages="

# Ada
gnat

# Algol
algol68g

# ARM
gcc-arm-linux-gnueabihf
qemu-user-static

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

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

rm "$0"
