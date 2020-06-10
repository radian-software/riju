#!/usr/bin/env bash

set -e
set -o pipefail

npm config set unsafe-perm true
PERL_MM_USE_DEFAULT=1 cpan App::cpanminus

# Befunge
npm install -g befunge93 prompt-sync

# ClojureScript
npm install -g lumo-cljs

# CoffeeScript
npm install -g coffeescript

# Elm
npm install -g @kachkaev/run-elm

# Perl
cpanm -n Devel::REPL

# ReasonML
npm install -g bs-platform

# Shakespeare
pip3 install shakespearelang

# TypeScript
npm install -g ts-node typescript

# Whitespace
pip3 install whitespace

# Wolfram Language
python3.7 -m pip install mathics

rm "$0"
