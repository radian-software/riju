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

# Binary Lambda Calculus
wget -nv https://www.ioccc.org/2012/tromp/tromp.c
clang tromp.c -Wno-everything -DInt=long -DX=8 -DA=500000 -o /usr/bin/tromp
rm tromp.c

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

# Thue
wget -nv https://catseye.tc/distfiles/thue-1.5-2015.0827.zip
unzip thue-*.zip
rm thue-*.zip
pushd thue-* >/dev/null
./build.sh
mv bin/thue /usr/bin/thue
popd >/dev/null
rm -rf thue-*

popd >/dev/null
rm "$0"
