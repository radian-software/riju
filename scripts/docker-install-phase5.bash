#!/usr/bin/env bash

set -e
set -o pipefail
set -x

# Package manager - Julia
mkdir /opt/julia
export JULIA_DEPOT_PATH=/opt/julia

# Package manager - OCaml
export OPAMROOT=/opt/opam
export OPAMROOTISOK=1
opam init -n --disable-sandboxing

# Package manager - Node.js
npm config set unsafe-perm true
PERL_MM_USE_DEFAULT=1 cpan App::cpanminus
rm -rf /tmp/cpan_install_*.txt

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

# D
dub fetch --version='~master' dfmt
dub run dfmt -- --version
mv "$HOME/.dub/packages/dfmt-master/dfmt/bin/dfmt" /usr/local/bin/

# Dogescript
npm install -g dogescript

# Elm
npm install -g @kachkaev/run-elm
npm install -g @elm-tooling/elm-language-server

# FORTRAN
pip3 install fortran-language-server

# Hy
pip3 install hy

# Julia
julia -e 'using Pkg; Pkg.add("LanguageServer")'

# Less
npm install -g less

# LiveScript
npm install -g livescript

# OCaml
opam install -y ocamlformat
opam pin add -y ocaml-lsp-server https://github.com/ocaml/ocaml-lsp.git
ln -s /opt/opam/default/bin/ocamlformat /usr/local/bin/ocamlformat
ln -s /opt/opam/default/bin/ocamllsp /usr/local/bin/ocamllsp

# Perl
cpanm -n Devel::REPL
cpanm -n Perl::Tidy

# PHP
npm install -g intelephense

# Pikachu
pip3 install pikalang

# Pug
npm install -g pug-cli

# PureScript
npm install -g purescript spago

# Python
pip3 install black

# ReasonML
npm install -g bs-platform

# Ruby
gem install rufo
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

rm -rf /root/.cache /root/.config /root/.cpan /root/.cpanm /root/.dub /root/.gem /root/.npm /root/.npmrc
rm -f /tmp/core-js-banners

rm "$0"
