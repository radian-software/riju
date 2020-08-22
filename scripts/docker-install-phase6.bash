#!/usr/bin/env bash

set -e
set -o pipefail
set -x
pushd /tmp >/dev/null

# Beatnik
git clone https://github.com/catseye/Beatnik.git
sed -i 's#env python#env python2#' Beatnik/script/beatnik.py
mv Beatnik/script/beatnik.py /usr/local/bin/beatnik
rm -rf Beatnik

# Binary Lambda Calculus
wget -nv https://www.ioccc.org/2012/tromp/tromp.c
clang tromp.c -Wno-everything -DInt=long -DX=8 -DA=500000 -o /usr/local/bin/tromp
rm tromp.c

# Erlang
git clone https://github.com/erlang-ls/erlang_ls.git
pushd erlang_ls >/dev/null
make
mv _build/default/bin/erlang_ls /usr/local/bin/erlang_ls
popd >/dev/null
rm -rf erlang_ls

# Groovy
git clone https://github.com/prominic/groovy-language-server.git
pushd groovy-language-server >/dev/null
./gradlew build
mkdir /opt/groovy
mv build/libs/groovy-language-server-all.jar /opt/groovy/language-server.jar
popd >/dev/null
rm -rf groovy-language-server

# Hexagony
git clone https://github.com/m-ender/hexagony.git /opt/hexagony

# Kalyn
git clone https://github.com/raxod502/kalyn.git
pushd kalyn >/dev/null
stack build kalyn
mv "$(stack exec which kalyn)" /usr/local/bin/kalyn
mkdir /opt/kalyn
mv src-kalyn/Stdlib src-kalyn/Stdlib.kalyn /opt/kalyn/
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
clang malbolge/malbolge.c -o /usr/local/bin/malbolge
rm -rf malbolge

# Rapira
git clone https://github.com/freeduke33/rerap2.git
pushd rerap2 >/dev/null
make
mv rapira /usr/local/bin/rapira
popd >/dev/null
rm -rf rerap2

# Qalb
git clone https://github.com/nasser/---.git qalb
pushd qalb >/dev/null
mkdir -p /opt/qalb
mv public/qlb/*.js /opt/qalb/
popd >/dev/null
rm -rf qalb

# Snobol
file="$(curl -sSL ftp://ftp.snobol4.org/snobol/ | grep -Eo 'snobol4-.*\.tar\.gz' | sort -rV | head -n1)"
wget -nv "ftp://ftp.snobol4.org/snobol/${file}"
tar -xf snobol4-*.tar.gz
rm snobol4-*.tar.gz
pushd snobol4-* >/dev/null
make || true
mv snobol4 /usr/local/bin/snobol4
popd >/dev/null
rm -rf snobol4-*

# Thue
wget -nv "$(curl -sSL https://catseye.tc/distribution/Thue_distribution | grep -Eo 'https://catseye.tc/distfiles/thue-[^"]+\.zip' | head -n1)"
unzip thue-*.zip
rm thue-*.zip
pushd thue-* >/dev/null
./build.sh
mv bin/thue /usr/local/bin/thue
popd >/dev/null
rm -rf thue-*

# Zot
git clone https://github.com/manyoso/zot.git
pushd zot >/dev/null
./build.sh
mv build/bin/zot /usr/local/bin/zot
popd >/dev/null
rm -rf zot

popd >/dev/null
rm "$0"
