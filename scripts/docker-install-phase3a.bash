#!/usr/bin/env bash

set -e
set -o pipefail
set -x

export DEBIAN_FRONTEND=noninteractive
apt-get update

ceylon="$(grep-aptavail -F Package ceylon -s Package -n | sort -rV | head -n1)"

packages="

# A+
aplus-fsf
aplus-fsf-doc
rlwrap

# Ada
gnat

# Afnix
afnix
afnix-doc

# Algol
algol68g

# APL
libtinfo5

# ARM
gcc-arm-linux-gnueabihf
qemu-user-static

# AsciiDoc
asciidoc

# AspectC++
aspectc++

# AspectJ
aspectj

# Asymptote
asymptote

# ATS
ats2-lang

# Awk
mawk

# BASIC
bwbasic

# Bash
bash

# Battlestar
golang
yasm

# bc
bc

# Beanshell
bsh

# BrainF
beef

# C/C++
clang
clang-format
clangd

# C#
clang-format
mono-mcs

# Ceylon
${ceylon}
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

# Dylan
libunwind-dev

"

apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

rm "$0"
