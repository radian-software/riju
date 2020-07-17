#!/usr/bin/env bash

set -e
set -o pipefail
set -x
pushd /tmp >/dev/null

# Beatnik
git clone https://github.com/catseye/Beatnik.git
sed -i 's#env python#env python2#' Beatnik/script/beatnik.py
mv Beatnik/script/beatnik.py /usr/bin/beatnik
rm -rf Beatnik

# Erlang
git clone https://github.com/erlang-ls/erlang_ls.git
pushd erlang_ls >/dev/null
make
mv _build/default/bin/erlang_ls /usr/bin/erlang_ls
popd >/dev/null
rm -rf erlang_ls

# Kalyn
git clone https://github.com/raxod502/kalyn.git
pushd kalyn >/dev/null
stack build kalyn
mv "$(stack exec which kalyn)" /usr/bin/kalyn
mkdir /opt/kalyn
cp -R src-kalyn/Stdlib src-kalyn/Stdlib.kalyn /opt/kalyn/
popd >/dev/null
rm -rf kalyn

# LOLCODE
git clone https://github.com/justinmeza/lci.git
pushd lci >/dev/null
python3 install.py --prefix=/usr
popd >/dev/null
rm -rf lci

# Malbolge
git clone https://github.com/bipinu/malbolge.git
clang malbolge/malbolge.c -o /usr/bin/malbolge
rm -rf malbolge

# SNOBOL
wget -nv ftp://ftp.snobol4.org/snobol/old/snobol4-2.1.4.tar.gz
tar -xf snobol4-*.tar.gz
rm snobol4-*.tar.gz
pushd snobol4-* >/dev/null
make || true
mv snobol4 /usr/bin/snobol4
popd >/dev/null
rm -rf snobol4-*

popd >/dev/null
rm "$0"
