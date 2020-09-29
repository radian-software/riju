#!/usr/bin/env bash

set -e
set -o pipefail
set -x

export DEBIAN_FRONTEND=noninteractive
apt-get update

lua_ver="$(grep-aptavail -XF Provides lua -s Version -n | sort -Vr | head -n1)"
lua_name="$(grep-aptavail -XF Provides lua -a -XF Version "${lua_ver}" -s Package -n | head -n1)"

packages="

# eC
ecere-dev

# Elixir
elixir

# Elvish
elvish

# Emacs Lisp
emacs-nox

# Erlang
erlang
libodbc1  # workaround bug in APT
rebar

# F#
fsharp

# Fish
fish

# FORTRAN
flang

# Forth
gforth

# Go
golang

# Groovy
groovy

# Hack
hhvm

# Haskell
cabal-install
ghc

# Haxe
haxe

# INTERCAL
intercal

# Java
clang-format
default-jdk

# Julia
julia

# Ksh
ksh

# LLVM
llvm

# LOLCODE
cmake

# Lua
${lua_name}

"

apt-get install -y $(sed 's/#.*//' <<< "$packages")
rm -rf /var/lib/apt/lists/*

rm "$0"
