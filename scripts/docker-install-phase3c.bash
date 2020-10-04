#!/usr/bin/env bash

set -e
set -o pipefail
set -x

export DEBIAN_FRONTEND=noninteractive
apt-get update

pike_name="$(grep-aptavail -eF Package "^pike[0-9.]+$" -s Package -n | sort -Vr | head -n1)"

packages="

# m4
m4

# MariaDB
libtinfo5

# MiniZinc
minizinc

# MIPS
gcc-mips64-linux-gnuabi64
qemu-user-static

# MongoDB
mongodb

# MUMPS
fis-gtm

# MySQL
mysql-server

# Neko
neko

# Neo4j
neo4j

# Nickle
nickle

# Nim
nim

# Node.js
nodejs
yarn

# Oberon
clang
make

# Objective-C
gcc
gnustep-devel

# OCaml
ocaml
opam

# Octave
octave

# Ook
autoconf

# PARI/GP
pari-gp

# Parser3
parser3-cgi

# Pascal
fpc

# Perl
perl
perlconsole

# PHP
php

# Pike
${pike_name}
${pike_name}-doc

# PostgreSQL
postgresql
postgresql-client

# PostScript
ghostscript
rlwrap

# Prolog
swi-prolog

# Promela
spin

# PureScript
libtinfo5

# Python
python3
python3-pip
python3-venv

# R
r-base

# Racket
racket

# Rapira
clang

# Ratfor
flang
ratfor

# rc
rc

# Redis
redis

# REXX
regina-rexx

# RISC-V
gcc-riscv64-linux-gnu
qemu-user-static

# Ruby
ruby
ruby-dev

"

apt-get install -y $(grep -v "^#" <<< "$packages")
rm -rf /var/lib/apt/lists/*

rm /etc/mysql/mysql.conf.d/mysqld.cnf

rm "$0"
