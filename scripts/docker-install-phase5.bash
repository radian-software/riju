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

# Ada
wget -nv https://dl.bintray.com/reznikmm/ada-language-server/linux-latest.tar.gz
tar -xf linux-latest.tar.gz
mv linux/ada_language_server /usr/bin/ada_language_server
mv linux/*.so* /usr/lib/x86_64-linux-gnu/
rm -rf linux linux-latest.tar.gz

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

# Entropy
wget -nv http://danieltemkin.com/Content/Entropy/Entropy.zip
unzip -d /opt/entropy Entropy.zip
rm Entropy.zip

# Erlang
wget -nv https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
mv rebar3 /usr/bin/rebar3

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

# Kotlin
wget -nv https://github.com/JetBrains/kotlin/releases/download/v1.3.72/kotlin-compiler-1.3.72.zip
unzip kotlin-*.zip
cp kotlinc/bin/* /usr/bin/
cp kotlinc/lib/* /usr/lib/
rm -rf kotlin-*.zip kotlinc

# Lua
wget -nv https://github.com/EmmyLua/EmmyLua-LanguageServer/releases/download/0.3.6/EmmyLua-LS-all.jar
mv EmmyLua-LS-all.jar /usr/lib/EmmyLua-LS.jar

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
