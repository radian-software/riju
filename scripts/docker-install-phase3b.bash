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

# Flex
flex
flex-doc

# FORTRAN
flang

# Forth
gforth

# Gambas
gambas3-script

# GAP
gap

# GDB
gdb

# GEL
genius

# Go
golang

# Groovy
groovy

# Gnuplot
gnuplot

# Hack
hhvm

# Haskell
cabal-install
ghc

# Haxe
haxe

# Icon
icont

# Idris
chezscheme
gcc

# INTERCAL
intercal

# Jasmin
jasmin-sable

# Java
clang-format
default-jdk

# jq
jq

# Julia
julia

# Ksh
ksh

# Limbo
gcc
libc6-dev-i386
libx11-dev:i386
libxext-dev:i386

# Lisaac
lisaac

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
