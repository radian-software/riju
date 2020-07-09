#!/usr/bin/env bash

set -e
set -o pipefail
set -x

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

rm "$0"
