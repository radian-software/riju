#!/usr/bin/env bash

set -e
set -o pipefail
set -x
pushd /tmp >/dev/null

latest_release() {
    curl -sSL "https://api.github.com/repos/$1/releases/latest" | jq -r .tag_name
}

# Needed for project infrastructure
ver="$(latest_release watchexec/watchexec)"
wget -nv "https://github.com/watchexec/watchexec/releases/download/${ver}/watchexec-${ver}-x86_64-unknown-linux-gnu.deb"
dpkg -i watchexec-*.deb
rm watchexec-*.deb

git clone https://github.com/circulosmeos/gdown.pl.git
mv gdown.pl/gdown.pl /usr/local/bin/gdown
rm -rf gdown.pl

# Shared
ver="$(latest_release jgm/pandoc)"
wget -nv "https://github.com/jgm/pandoc/releases/download/${ver}/pandoc-${ver}-linux-amd64.tar.gz"
tar -xf pandoc-*-linux-amd64.tar.gz -C /usr --strip-components=1
rm pandoc-*-linux-amd64.tar.gz

# ><>
wget -nv https://gist.githubusercontent.com/anonymous/6392418/raw/fish.py -O /usr/local/bin/esofish
sed -i 's:^#!.*:#!/usr/bin/env python3:' /usr/local/bin/esofish
chmod +x /usr/local/bin/esofish

# Ada
wget -nv https://dl.bintray.com/reznikmm/ada-language-server/linux-latest.tar.gz
tar -xf linux-latest.tar.gz
mv linux/ada_language_server /usr/local/bin/ada_language_server
mv linux/*.so* /usr/lib/x86_64-linux-gnu/
rm -rf linux linux-latest.tar.gz

# APL
file="$(curl -sS ftp://ftp.gnu.org/gnu/apl/ | grep -Eo 'apl_[-0-9.]+_amd64.deb$' | sort -rV | head -n1)"
wget -nv "ftp://ftp.gnu.org/gnu/apl/${file}"
dpkg -i apl_*_amd64.deb
rm apl_*_amd64.deb

# Clojure
ver="$(latest_release snoe/clojure-lsp)"
wget -nv "https://github.com/snoe/clojure-lsp/releases/download/${ver}/clojure-lsp"
chmod +x clojure-lsp
mv clojure-lsp /usr/local/bin/clojure-lsp

# D
wget -nv "$(curl -sSL https://dlang.org/download.html | grep -Eo '"http://[^"]+amd64.deb"' | tr -d '"')"
dpkg -i dmd_*.deb
rm dmd_*.deb

# Dhall
ver="$(latest_release dhall-lang/dhall-haskell)"
file="$(curl -sSL "https://api.github.com/repos/dhall-lang/dhall-haskell/releases/tags/${ver}" | jq -r '.assets | map(select(.name | (contains("dhall-json") and contains("x86_64-linux.tar.bz2")))) | .[0].name')"
wget -nv "https://github.com/dhall-lang/dhall-haskell/releases/download/${ver}/${file}"
mkdir dhall-json
tar -xf dhall-json-*-x86_64-linux.tar.bz2 -C dhall-json
mv dhall-json/bin/dhall-to-json dhall-json/bin/json-to-dhall /usr/local/bin/
rm -rf dhall-json dhall-json-*-x86_64-linux.tar.bz2

# Elixir
ver="$(latest_release elixir-lsp/elixir-ls)"
wget -nv "https://github.com/elixir-lsp/elixir-ls/releases/download/${ver}/elixir-ls.zip"
unzip -d /opt/elixir-ls elixir-ls.zip
ln -s /opt/elixir-ls/language_server.sh /usr/local/bin/elixir-ls
rm elixir-ls.zip

# Elm
ver="$(latest_release elm/compiler)"
wget -nv "https://github.com/elm/compiler/releases/download/${ver}/binary-for-linux-64-bit.gz"
gunzip binary-for-linux-64-bit.gz
chmod +x binary-for-linux-64-bit
mv binary-for-linux-64-bit /usr/local/bin/elm

# Emojicode
ver="$(latest_release emojicode/emojicode)"
wget -nv "https://github.com/emojicode/emojicode/releases/download/${ver}/Emojicode-$(sed 's/^v//' <<< "$ver")-Linux-x86_64.tar.gz"
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
mv rebar3 /usr/local/bin/rebar3

# Euphoria
wget -nv http://www.rapideuphoria.com/31/euphor31.tar
mkdir /opt/euphoria
tar -xf euphor*.tar -C /opt/euphoria --strip-components=1
ln -s /opt/euphoria/bin/exu /usr/bin/
rm euphor*.tar

# Factor
ver="$(curl -sSL https://factorcode.org/ | grep -Eo 'release\?os=linux[^>]+>[^<]+' | sed -E 's/[^>]+>//' | head -n1)"
wget -nv "https://downloads.factorcode.org/releases/${ver}/factor-linux-x86-64-${ver}.tar.gz"
tar -xf factor-linux-x86-64-*.tar.gz
mv -T factor /opt/factor
ln -s /opt/factor/factor /usr/local/bin/factor-lang
rm factor-linux-x86-64-*.tar.gz

# Go
export GO111MODULE=on
export GOPATH="$PWD/go"
go get golang.org/x/tools/gopls@latest
mv go/bin/gopls /usr/local/bin/gopls
rm -rf go

# GolfScript
wget -nv http://www.golfscript.com/golfscript/golfscript.rb -O /usr/local/bin/golfscript
chmod +x /usr/local/bin/golfscript

# Haskell
curl -sSL https://get.haskellstack.org/ | sh

wget "https://drive.google.com/uc?export=download&id=1MpozlNLmWeUaQuT-5t6gyE3Yv56gUbea" -O /usr/local/bin/brittany
chmod +x /usr/local/bin/brittany

mkdir -p /opt/haskell
gdown "https://drive.google.com/uc?export=download&id=1GPoR_ja4ns16KCamRgwB-JVag4HK0igz" /usr/local/bin/hie
gdown "https://drive.google.com/uc?export=download&id=1qSxj8JjAeetAmNjUGayX0RBARgr5R4Ij" /opt/haskell/hoogle.hoo
chmod +x /usr/local/bin/hie

# HCL/TOML/YAML
ver="$(latest_release sclevine/yj)"
wget -nv "https://github.com/sclevine/yj/releases/download/${ver}/yj-linux"
chmod +x yj-linux
mv yj-linux /usr/local/bin/yj

# Ink
ver="$(latest_release thesephist/ink)"
wget -nv "https://github.com/thesephist/ink/releases/download/${ver}/ink-linux"
wget -nv "https://github.com/thesephist/ink/releases/download/${ver}/std.ink"
wget -nv "https://github.com/thesephist/ink/releases/download/${ver}/str.ink"
chmod +x ink-linux
mv ink-linux /usr/local/bin/ink
mkdir /opt/ink
mv std.ink str.ink /opt/ink/

# Ioke
wget -nv https://ioke.org/dist/ioke-ikj-latest.tar.gz
tar -xf ioke-ikj-*.tar.gz -C /opt
rm ioke-ikj-*.tar.gz
ln -s /opt/ioke/bin/ioke /usr/local/bin/ioke

# Kitten
wget -nv "https://drive.google.com/uc?export=download&id=11u0G2I8i0u4ez27zvEjAT6E9xF4RwuFZ" -O /usr/local/bin/kitten
wget -nv "https://drive.google.com/uc?export=download&id=1h-U1iURWax8h18kTD1AyGS21UblEIT9K" -O /usr/local/bin/common.ktn
chmod +x /usr/local/bin/kitten

# Kotlin
ver="$(latest_release JetBrains/kotlin)"
wget -nv "https://github.com/JetBrains/kotlin/releases/download/${ver}/kotlin-compiler-$(sed 's/^v//' <<< "$ver").zip"
unzip kotlin-*.zip
cp kotlinc/bin/* /usr/local/bin/
cp kotlinc/lib/* /usr/local/lib/
rm -rf kotlin-*.zip kotlinc

# Lua
ver="$(latest_release EmmyLua/EmmyLua-LanguageServer)"
wget -nv "https://github.com/EmmyLua/EmmyLua-LanguageServer/releases/download/${ver}/EmmyLua-LS-all.jar"
mv EmmyLua-LS-all.jar /usr/lib/EmmyLua-LS.jar

# MariaDB
ver="$(curl -sSL https://downloads.mariadb.org/ | grep 'href="/mariadb/[0-9]' | grep -Eo '[0-9][^/]+' | sort -rV | head -n1)"
wget -nv "https://downloads.mariadb.org/f/mariadb-${ver}/bintar-linux-x86_64/mariadb-${ver}-linux-x86_64.tar.gz/from/http%3A//mirror.dal.ca/mariadb/?serve" -O mariadb.tar.gz
tar -xf mariadb.tar.gz
mkdir /opt/mariadb
mv mariadb-*-linux-x86_64/* /opt/mariadb/
chmod a=rx,u=rwx /opt/mariadb/lib/plugin/auth_pam_tool_dir
chmod a=rx,u=rwxs /opt/mariadb/lib/plugin/auth_pam_tool_dir/auth_pam_tool

# Omgrofl
ver="$(latest_release OlegSmelov/omgrofl-interpreter)"
mkdir /opt/omgrofl
wget -nv "https://github.com/OlegSmelov/omgrofl-interpreter/releases/download/${ver}/Omgrofl.jar" -O /opt/omgrofl/Omgrofl.jar

# PowerShell
ver="$(latest_release PowerShell/PowerShell)"
wget -nv "https://github.com/PowerShell/PowerShell/releases/download/${ver}/powershell-$(sed 's/^v//' <<< "$ver")-linux-x64.tar.gz"
mkdir /opt/powershell
tar -xf powershell-*.tar.gz -C /opt/powershell
ln -s /opt/powershell/pwsh /usr/local/bin/pwsh
rm powershell-*.tar.gz

ver="$(latest_release PowerShell/PowerShellEditorServices)"
wget -nv "https://github.com/PowerShell/PowerShellEditorServices/releases/download/${ver}/PowerShellEditorServices.zip"
unzip PowerShellEditorServices.zip
mv PowerShellEditorServices /opt/powershell-editor-services
rm PowerShellEditorServices.zip

# Python
xml="$(curl -sSL "https://pvsc.blob.core.windows.net/python-language-server-stable?restype=container&comp=list&prefix=Python-Language-Server-linux-x64")"
nupkg="$(echo "$xml" | grep -Eo 'https://[^<]+\.nupkg' | tail -n1)"
wget -nv "${nupkg}"
unzip -d /opt/mspyls Python-Language-Server-linux-x64.*.nupkg
chmod +x /opt/mspyls/Microsoft.Python.LanguageServer
ln -s /opt/mspyls/Microsoft.Python.LanguageServer /usr/local/bin/Microsoft.Python.LanguageServer
rm Python-Language-Server-linux-x64.*.nupkg

# ReasonML
ver="$(latest_release jaredly/reason-language-server)"
wget -nv "https://github.com/jaredly/reason-language-server/releases/download/${ver}/rls-linux.zip"
unzip rls-linux.zip
mv rls-linux/reason-language-server /usr/local/bin/
rm rls-linux.zip

# Rust
export CARGO_HOME=/opt/rust
export RUSTUP_HOME=/opt/rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path

tee /opt/rust/wrapper >/dev/null <<"EOF"
#!/usr/bin/env bash
RUSTUP_HOME=/opt/rust exec /opt/rust/bin/${0##*/} "$@"
EOF
chmod +x /opt/rust/wrapper
for file in /opt/rust/bin/*; do
    ln -s /opt/rust/wrapper "/usr/local/bin/${file##*/}"
done

# Scala
file="$(curl -sSL https://scalameta.org/metals/docs/editors/emacs.html | grep -Eo 'org.scalameta[^ ]+')"
wget -nv https://git.io/coursier-cli
chmod +x coursier-cli
mv coursier-cli /usr/local/bin/coursier
coursier bootstrap --java-opt -Xss4m --java-opt -Xms100m --java-opt -Dmetals.client=emacs "${file}" -r bintray:scalacenter/releases -r sonatype:snapshots -o /usr/local/bin/metals
metals -version </dev/null
mkdir /opt/coursier
mv "$HOME/.cache/coursier" /opt/coursier/cache

# SETL
wget -nv https://setl.org/setl/bin/Linux-x86-64bit/setlbin.tgz
tar -xf setlbin.tgz -C /usr/local/bin

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

# Swift
gdown "https://drive.google.com/uc?export=download&id=1eE1-VuZz0gv-fITaGVT_r1UunCLjS-JT" swift.tar.gz
mkdir /opt/swift
tar -xf swift.tar.gz -C /opt/swift --strip-components=2
ln -s /opt/swift/bin/swiftc /usr/local/bin/swiftc
ln -s /opt/swift/bin/sourcekit-lsp /usr/local/bin/sourcekit-lsp
rm swift.tar.gz

popd >/dev/null
rm "$0"
