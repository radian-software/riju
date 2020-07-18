#!/usr/bin/env bash

set -e
set -o pipefail
set -x

# Package manager - Julia
mkdir /opt/julia
export JULIA_DEPOT_PATH=/opt/julia

# Package manager - Node.js
npm config set unsafe-perm true
PERL_MM_USE_DEFAULT=1 cpan App::cpanminus
rm -rf /tmp/cpan_install_*.txt

# Package manager - Rust
export CARGO_HOME=/opt/rust
export RUSTUP_HOME=/opt/rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path

tee /opt/rust/wrapper >/dev/null <<"EOF"
#!/usr/bin/env bash
RUSTUP_HOME=/opt/rust exec /opt/rust/bin/${0##*/} "$@"
EOF
chmod +x /opt/rust/wrapper
for file in /opt/rust/bin/*; do
    ln -s /opt/rust/wrapper "/usr/bin/${file##*/}"
done

# Shared
npm install -g prettier

# Bash
npm install -g bash-language-server

# Befunge
npm install -g befunge93 prompt-sync

# Chef
cpanm -n Acme::Chef

# ClojureScript
npm install -g lumo-cljs

# CoffeeScript
npm install -g coffeescript

# Dogescript
npm install -g dogescript

# Elm
npm install -g @kachkaev/run-elm
npm install -g @elm-tooling/elm-language-server

# FORTRAN
pip3 install fortran-language-server

# Julia
julia -e 'using Pkg; Pkg.add("LanguageServer")'

# Less
npm install -g less

# LiveScript
npm install -g livescript

# Perl
cpanm -n Devel::REPL

# PHP
npm install -g intelephense

# Pug
npm install -g pug-cli

# PureScript
npm install -g purescript spago

# ReasonML
npm install -g bs-platform

# Ruby
gem install solargraph

# Rust
rustup component add rls rust-analysis rust-src

# Sass/SCSS
npm install -g sass

# Shakespeare
pip3 install shakespearelang

# TeX
luarocks install digestif

# TypeScript
npm install -g ts-node typescript

# Vim
npm install -g vim-language-server

# Whitespace
pip3 install whitespace

# Wolfram Language
python3.7 -m pip install mathics

rm -rf /root/.cache /root/.config /root/.cpan /root/.cpanm /root/.gem /root/.npm /root/.npmrc
rm -f /tmp/core-js-banners

rm "$0"
