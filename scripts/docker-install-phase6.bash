#!/usr/bin/env bash

set -e
set -o pipefail
set -x
pushd /tmp >/dev/null

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

# Befunge
tee /usr/bin/befunge-repl >/dev/null <<"EOF"
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
chmod +x /usr/bin/befunge-repl

# Binary Lambda Calculus
tee /usr/bin/binary-to-text >/dev/null <<"EOF"
#!/usr/bin/env python3

import re
import sys

text = re.sub(r"[^01]", "", sys.stdin.read())
out = []

for m in re.finditer(r"([01]{8})", text):
    out += chr(int(m.group(0), 2))

print("".join(out), end="")
EOF
chmod +x /usr/bin/binary-to-text

# BrainF
tee /usr/bin/brainf-repl >/dev/null <<"EOF"
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
chmod +x /usr/bin/brainf-repl

# Elm
mkdir /opt/elm
tee /opt/elm/elm.json >/dev/null <<"EOF"
{
    "type": "application",
    "source-directories": [
        "."
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "elm/browser": "1.0.2",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0"
        },
        "indirect": {
            "elm/json": "1.1.3",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.2"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
EOF

# Haskell
mkdir -p /opt/haskell
tee /opt/haskell/hie.yaml >/dev/null <<"EOF"
cradle:
  direct:
    arguments: []
EOF

# Unlambda
tee /usr/bin/unlambda-repl >/dev/null <<"EOF"
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
chmod +x /usr/bin/unlambda-repl

popd >/dev/null
rm "$0"
