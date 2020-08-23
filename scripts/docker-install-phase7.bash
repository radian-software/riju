#!/usr/bin/env bash

set -e
set -o pipefail
set -x
pushd /tmp >/dev/null
useradd -m -N -l -r -p '!' build

# Cmd
sudo -u build wine cmd < /dev/null
mkdir -p /opt/cmd/home-template
mv /home/build/.wine /opt/cmd/home-template/
chmod -R a=u,go-w /opt/cmd/home-template

# Elm
mkdir -p /opt/elm
mkdir elm-project
pushd elm-project >/dev/null
(yes || true) | elm init
cat elm.json | jq '."source-directories" = ["."]' > /opt/elm/elm.json
popd >/dev/null
rm -rf elm-project

# PureScript
mkdir project-template
pushd project-template >/dev/null
spago init -C
rm -rf .gitignore test
sed -i 's#, "test/\*\*/\*\.purs"##' spago.dhall
cat <<"EOF" > src/Main.spago
import Prelude

import Effect (Effect)

main :: Effect Unit
main = pure unit
EOF
spago build
spago repl < /dev/null
rm -rf src
popd >/dev/null
mkdir /opt/purescript
mv project-template /opt/purescript/

# ReasonML
mkdir -p /opt/reasonml/project-template
pushd /opt/reasonml/project-template >/dev/null
bsb -init .
cat bsconfig.json | jq '.name = "riju-project"' | sponge bsconfig.json
yarn install
popd >/dev/null

# Befunge
tee /usr/local/bin/befunge-repl >/dev/null <<"EOF"
#!/usr/bin/env -S NODE_PATH=/usr/lib/node_modules node
const fs = require("fs");

const Befunge = require("befunge93");
const prompt = require("prompt-sync")();

const befunge = new Befunge();
befunge.onInput = prompt;
befunge.onOutput = (output) => {
  if (typeof output === "string") {
    process.stdout.write(output);
  } else {
    process.stdout.write(output + " ");
  }
};

const args = process.argv.slice(2);
if (args.length !== 1) {
  console.error("usage: befunge-repl FILE");
  process.exit(1);
}

befunge.run(fs.readFileSync(args[0], { encoding: "utf-8" })).catch((err) => {
  console.error(err);
  process.exit(1);
});
EOF
chmod +x /usr/local/bin/befunge-repl

# Binary Lambda Calculus
tee /usr/local/bin/binary-to-text >/dev/null <<"EOF"
#!/usr/bin/env python3

import re
import sys

text = re.sub(r"[^01]", "", sys.stdin.read())
out = []

for m in re.finditer(r"([01]{8})", text):
    out += chr(int(m.group(0), 2))

print("".join(out), end="")
EOF
chmod +x /usr/local/bin/binary-to-text

# BrainF
tee /usr/local/bin/brainf-repl >/dev/null <<"EOF"
#!/usr/bin/env python3
import argparse
import readline
import subprocess
import tempfile

parser = argparse.ArgumentParser()
parser.add_argument("file", nargs="?")
args = parser.parse_args()

if args.file:
    subprocess.run(["beef", args.file])
while True:
    try:
        code = input("bf> ")
    except KeyboardInterrupt:
        print("^C")
        continue
    except EOFError:
        print("^D")
        break
    if not code:
        continue
    with tempfile.NamedTemporaryFile(mode="w") as f:
        f.write(code)
        f.flush()
        subprocess.run(["beef", f.name])
EOF
chmod +x /usr/local/bin/brainf-repl

# Cat
tee /opt/cat/repl.js >/dev/null <<"EOF"
const fs = require("fs");
const repl = require("repl");

const args = process.argv.slice(2);
if (args.length > 1) {
  console.error("usage: repl.js [FILE]");
  process.exit(1);
}

const program = args.length === 1 ? fs.readFileSync(args[0], "utf-8") : null;

const cat = require("cat");
const ce = new cat.CatLanguage.CatEvaluator();

if (program !== null) {
  ce.eval(program);
}

repl.start({prompt: "cat> ", eval: (cmd, context, filename, callback) => callback(null, ce.eval(cmd))});
EOF

# Haskell
mkdir -p /opt/haskell
tee /opt/haskell/hie.yaml >/dev/null <<"EOF"
cradle:
  direct:
    arguments: []
EOF

# Qalb
mkdir -p /opt/qalb
tee /opt/qalb/repl.js >/dev/null <<"EOF"
const fs = require("fs");
const repl = require("repl");

const args = process.argv.slice(2);
if (args.length > 1) {
  console.error("usage: repl.js [FILE]");
  process.exit(1);
}

const program = args.length === 1 ? fs.readFileSync(args[0], "utf-8") : null;

eval(fs.readFileSync("/opt/qalb/qlb.js", "utf-8"));
eval(fs.readFileSync("/opt/qalb/parser.js", "utf-8"));
eval(fs.readFileSync("/opt/qalb/primitives.js", "utf-8"));

Qlb.init({console});

if (program !== null) {
  Qlb.execute(program);
}

repl.start({prompt: "قلب> ", eval: (cmd, context, filename, callback) => callback(null, Qlb.execute(cmd))});
EOF

# Unlambda
tee /usr/local/bin/unlambda-repl >/dev/null <<"EOF"
#!/usr/bin/env python3
import argparse
import readline
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("file", nargs="?")
args = parser.parse_args()

if args.file:
    with open(args.file) as f:
        subprocess.run(["unlambda"], input=f.read(), encoding="utf-8")
while True:
    try:
        code = input("λ> ")
    except KeyboardInterrupt:
        print("^C")
        continue
    except EOFError:
        print("^D")
        break
    if not code:
        continue
    subprocess.run(["unlambda"], input=code, encoding="utf-8")
EOF
chmod +x /usr/local/bin/unlambda-repl

userdel -r build
popd >/dev/null
rm "$0"
