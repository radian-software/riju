#!/usr/bin/env bash

set -e
set -o pipefail
set -x

packages="

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

# Ksh
ksh

# LOLCODE
cmake

# Lua
lua5.3

"

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

rm "$0"
