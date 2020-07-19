#!/usr/bin/env bash

set -e
set -o pipefail
set -x
pushd /tmp >/dev/null

# Needed for project infrastructure
wget -nv https://github.com/watchexec/watchexec/releases/download/1.13.1/watchexec-1.13.1-x86_64-unknown-linux-gnu.deb
dpkg -i watchexec-*.deb
rm watchexec-*.deb

git clone https://github.com/circulosmeos/gdown.pl.git
mv gdown.pl/gdown.pl /usr/bin/gdown
rm -rf gdown.pl

# Shared
wget -nv https://github.com/jgm/pandoc/releases/download/2.10/pandoc-2.10-linux-amd64.tar.gz
tar -xf pandoc-*-linux-amd64.tar.gz -C /usr --strip-components=1
rm pandoc-*-linux-amd64.tar.gz

# ><>
wget -nv https://gist.githubusercontent.com/anonymous/6392418/raw/3b16018cb47f2f9ad1fa085c155cc5c0dc448b2d/fish.py -O /usr/bin/esofish
sed -i 's:^#!.*:#!/usr/bin/env python3:' /usr/bin/esofish
chmod +x /usr/bin/esofish

# Ada
wget -nv https://dl.bintray.com/reznikmm/ada-language-server/linux-latest.tar.gz
tar -xf linux-latest.tar.gz
mv linux/ada_language_server /usr/bin/ada_language_server
mv linux/*.so* /usr/lib/x86_64-linux-gnu/
rm -rf linux linux-latest.tar.gz

# APL
wget -nv ftp://ftp.gnu.org/gnu/apl/apl_1.8-1_amd64.deb
dpkg -i apl_*_amd64.deb
rm apl_*_amd64.deb

# Ceylon
wget -nv https://ceylon-lang.org/download/dist/1_3_3_deb -O ceylon.deb
dpkg -i ceylon.deb
rm ceylon.deb

# Clojure
wget -nv https://github.com/snoe/clojure-lsp/releases/download/release-20200629T153107/clojure-lsp
chmod +x clojure-lsp
mv clojure-lsp /usr/bin/clojure-lsp

# D
wget -nv http://downloads.dlang.org/releases/2.x/2.092.0/dmd_2.092.0-0_amd64.deb
dpkg -i dmd_*.deb
rm dmd_*.deb

# Dhall
wget -nv https://github.com/dhall-lang/dhall-haskell/releases/download/1.33.1/dhall-json-1.7.0-x86_64-linux.tar.bz2
mkdir dhall-json
tar -xf dhall-json-*-x86_64-linux.tar.bz2 -C dhall-json
mv dhall-json/bin/dhall-to-json dhall-json/bin/json-to-dhall /usr/bin/
rm -rf dhall-json dhall-json-*-x86_64-linux.tar.bz2

# Elixir
wget -nv https://github.com/elixir-lsp/elixir-ls/releases/download/v0.5.0/elixir-ls.zip
unzip -d /opt/elixir-ls elixir-ls.zip
ln -s /opt/elixir-ls/language_server.sh /usr/bin/elixir-ls
rm elixir-ls.zip

# Elm
wget -nv https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
gunzip binary-for-linux-64-bit.gz
chmod +x binary-for-linux-64-bit
mv binary-for-linux-64-bit /usr/bin/elm

# Emojicode
wget -nv https://github.com/emojicode/emojicode/releases/download/v1.0-beta.2/Emojicode-1.0-beta.2-Linux-x86_64.tar.gz
tar -xf Emojicode-*-Linux-x86_64.tar.gz
pushd Emojicode-*-Linux-x86_64 >/dev/null
mv emojicodec /usr/local/bin/
mkdir -p /usr/local/include/emojicode
mv include/* /usr/local/include/emojicode/
mkdir -p /usr/local/EmojicodePackages
mv packages/* /usr/local/EmojicodePackages/
popd >/dev/null
rm -rf Emojicode-*-Linux-x86_64 Emojicode-*-Linux-x86_64.tar.gz

# Entropy
wget -nv http://danieltemkin.com/Content/Entropy/Entropy.zip
unzip -d /opt/entropy Entropy.zip
rm Entropy.zip

# Erlang
wget -nv https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
mv rebar3 /usr/bin/rebar3

# Euphoria
wget -nv https://sourceforge.net/projects/rapideuphoria/files/Euphoria/4.0.5/euphoria_4.0.5_amd64.deb/download -O euphoria.deb
dpkg -i euphoria.deb
rm euphoria.deb

# Factor
wget -nv https://downloads.factorcode.org/releases/0.98/factor-linux-x86-64-0.98.tar.gz
tar -xf factor-linux-x86-64-*.tar.gz
mv -T factor /opt/factor
ln -s /opt/factor/factor /usr/bin/factor-lang
rm factor-linux-x86-64-*.tar.gz

# Go
export GO111MODULE=on
export GOPATH="$PWD/go"
go get golang.org/x/tools/gopls@latest
mv go/bin/gopls /usr/bin/gopls
rm -rf go

# GolfScript
wget -nv http://www.golfscript.com/golfscript/golfscript.rb -O /usr/bin/golfscript
chmod +x /usr/bin/golfscript

# Haskell
wget -nv https://get.haskellstack.org/stable/linux-x86_64-static.tar.gz
tar -xf linux-x86_64-static.tar.gz
mv stack-*-linux-x86_64-static/stack /usr/bin/stack
rm -rf stack-*-linux-x86_64-static linux-x86_64-static.tar.gz

mkdir -p /opt/haskell
gdown "https://drive.google.com/uc?export=download&id=1GPoR_ja4ns16KCamRgwB-JVag4HK0igz" /usr/bin/hie
gdown "https://drive.google.com/uc?export=download&id=1qSxj8JjAeetAmNjUGayX0RBARgr5R4Ij" /opt/haskell/hoogle.hoo
chmod +x /usr/bin/hie

# HCL/TOML/YAML
wget -nv https://github.com/sclevine/yj/releases/download/v4.0.0/yj-linux
chmod +x yj-linux
mv yj-linux /usr/bin/yj

# Ink
wget -nv https://github.com/thesephist/ink/releases/download/v0.1.7/ink-linux
wget -nv https://github.com/thesephist/ink/releases/download/v0.1.7/std.ink
wget -nv https://github.com/thesephist/ink/releases/download/v0.1.7/str.ink
chmod +x ink-linux
mv ink-linux /usr/bin/ink
mkdir /opt/ink
mv std.ink str.ink /opt/ink/

# Ioke
wget -nv https://ioke.org/dist/ioke-P-ikj-0.4.0.tar.gz
tar -xf ioke-P-ikj-*.tar.gz -C /opt
rm ioke-P-ikj-*.tar.gz
ln -s /opt/ioke/bin/ioke /usr/bin/ioke

# Kitten
wget -nv "https://drive.google.com/uc?export=download&id=11u0G2I8i0u4ez27zvEjAT6E9xF4RwuFZ" -O /usr/local/bin/kitten
wget -nv "https://drive.google.com/uc?export=download&id=1h-U1iURWax8h18kTD1AyGS21UblEIT9K" -O /usr/local/bin/common.ktn
chmod +x /usr/local/bin/kitten

# Kotlin
wget -nv https://github.com/JetBrains/kotlin/releases/download/v1.3.72/kotlin-compiler-1.3.72.zip
unzip kotlin-*.zip
cp kotlinc/bin/* /usr/bin/
cp kotlinc/lib/* /usr/lib/
rm -rf kotlin-*.zip kotlinc

# Lua
wget -nv https://github.com/EmmyLua/EmmyLua-LanguageServer/releases/download/0.3.6/EmmyLua-LS-all.jar
mv EmmyLua-LS-all.jar /usr/lib/EmmyLua-LS.jar

# MariaDB
wget -nv "https://downloads.mariadb.org/f/mariadb-10.5.4/bintar-linux-x86_64/mariadb-10.5.4-linux-x86_64.tar.gz/from/http%3A//mirror.vpsfree.cz/mariadb/?serve" -O mariadb.tar.gz
tar -xf mariadb.tar.gz
mkdir /opt/mariadb
mv mariadb-*-linux-x86_64/* /opt/mariadb/
chmod a=rx,u=rwx /opt/mariadb/lib/plugin/auth_pam_tool_dir
chmod a=rx,u=rwxs /opt/mariadb/lib/plugin/auth_pam_tool_dir/auth_pam_tool

# Omgrofl
mkdir /opt/omgrofl
wget -nv https://github.com/OlegSmelov/omgrofl-interpreter/releases/download/v0.1/Omgrofl.jar -O /opt/omgrofl/Omgrofl.jar

# PowerShell
wget -nv https://github.com/PowerShell/PowerShell/releases/download/v7.0.1/powershell-7.0.1-linux-x64.tar.gz
mkdir /opt/powershell
tar -xf powershell-*.tar.gz -C /opt/powershell
ln -s /opt/powershell/pwsh /usr/bin/pwsh
rm powershell-*.tar.gz

wget -nv https://github.com/PowerShell/PowerShellEditorServices/releases/download/v2.2.0/PowerShellEditorServices.zip
unzip PowerShellEditorServices.zip
mv PowerShellEditorServices /opt/powershell-editor-services
rm PowerShellEditorServices.zip

# Python
xml="$(curl -sSL "https://pvsc.blob.core.windows.net/python-language-server-stable?restype=container&comp=list&prefix=Python-Language-Server-linux-x64")"
nupkg="$(echo "$xml" | grep -Eo 'https://[^<]+\.nupkg' | tail -n1)"
wget -nv "${nupkg}"
unzip -d /opt/mspyls Python-Language-Server-linux-x64.*.nupkg
chmod +x /opt/mspyls/Microsoft.Python.LanguageServer
ln -s /opt/mspyls/Microsoft.Python.LanguageServer /usr/bin/Microsoft.Python.LanguageServer
rm Python-Language-Server-linux-x64.*.nupkg

# Scala
wget -nv https://git.io/coursier-cli
chmod +x coursier-cli
mv coursier-cli /usr/bin/coursier
coursier bootstrap --java-opt -Xss4m --java-opt -Xms100m --java-opt -Dmetals.client=emacs org.scalameta:metals_2.12:0.9.1 -r bintray:scalacenter/releases -r sonatype:snapshots -o /usr/bin/metals
metals -version </dev/null
mkdir /opt/coursier
mv "$HOME/.cache/coursier" /opt/coursier/cache

# SETL
wget -nv https://setl.org/setl/bin/Linux-x86-64bit/setlbin.tgz
tar -xf setlbin.tgz -C /usr/local/bin

# Snobol
gdown "https://drive.google.com/file/d/1ygQkpgfirpq4b7s4YNsSijkK8IQgSLWk/view?usp=sharing" /usr/bin/snobol4
chmod +x /usr/bin/snobol4

# Swift
gdown "https://drive.google.com/uc?export=download&id=1eE1-VuZz0gv-fITaGVT_r1UunCLjS-JT" swift.tar.gz
mkdir /opt/swift
tar -xf swift.tar.gz -C /opt/swift --strip-components=2
ln -s /opt/swift/bin/swiftc /usr/bin/swiftc
ln -s /opt/swift/bin/sourcekit-lsp /usr/bin/sourcekit-lsp
rm swift.tar.gz

popd >/dev/null
rm "$0"
