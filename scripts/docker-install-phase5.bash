#!/usr/bin/env bash

set -e
set -o pipefail

# Needed for project infrastructure
cd /tmp
wget -nv https://github.com/watchexec/watchexec/releases/download/1.13.1/watchexec-1.13.1-x86_64-unknown-linux-gnu.deb
dpkg -i watchexec-*.deb
rm watchexec-*.deb

cd /tmp
git clone https://github.com/circulosmeos/gdown.pl.git
mv gdown.pl/gdown.pl /usr/bin/gdown

# D
cd /tmp
wget -nv http://downloads.dlang.org/releases/2.x/2.092.0/dmd_2.092.0-0_amd64.deb
dpkg -i dmd_*.deb
rm dmd_*.deb

# Elm
cd /tmp
wget -nv https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz
gunzip binary-for-linux-64-bit.gz
chmod +x binary-for-linux-64-bit
mv binary-for-linux-64-bit /usr/bin/elm

# Ink
cd /tmp
wget -nv https://github.com/thesephist/ink/releases/download/v0.1.7/ink-linux
wget -nv https://github.com/thesephist/ink/releases/download/v0.1.7/std.ink
wget -nv https://github.com/thesephist/ink/releases/download/v0.1.7/str.ink
chmod +x ink-linux
mv ink-linux /usr/bin/ink
mkdir /opt/ink
mv std.ink str.ink /opt/ink/

# Kotlin
cd /tmp
wget -nv https://github.com/JetBrains/kotlin/releases/download/v1.3.72/kotlin-compiler-1.3.72.zip
unzip kotlin-*.zip
cp kotlinc/bin/* /usr/bin/
cp kotlinc/lib/* /usr/lib/
rm -rf kotlin-*.zip kotlinc

# PowerShell
cd /tmp
wget -nv https://github.com/PowerShell/PowerShell/releases/download/v7.0.1/powershell-7.0.1-linux-x64.tar.gz
mkdir /opt/powershell
tar -xf powershell-*.tar.gz -C /opt/powershell
ln -s /opt/powershell/pwsh /usr/bin/pwsh
rm powershell-*.tar.gz

# SNOBOL
wget -nv ftp://ftp.snobol4.org/snobol/snobol4-2.0.tar.gz
tar -xf snobol4-*.tar.gz
rm snobol4-*.tar.gz
pushd snobol4-* >/dev/null
make || true
mv snobol4 /usr/bin/snobol4
popd >/dev/null
rm -rf snobol4-*

# Swift
cd /tmp
gdown "https://drive.google.com/uc?export=download&id=1eE1-VuZz0gv-fITaGVT_r1UunCLjS-JT" swift.tar.gz
mkdir /opt/swift
tar -xf swift.tar.gz -C /opt/swift --strip-components=2
ln -s /opt/swift/bin/swiftc /usr/bin/swiftc
rm swift.tar.gz

# Kalyn
cd /tmp
git clone https://github.com/raxod502/kalyn
pushd kalyn >/dev/null
stack build kalyn
mv "$(stack exec which kalyn)" /usr/bin/kalyn
mkdir /opt/kalyn
cp -R src-kalyn/Stdlib src-kalyn/Stdlib.kalyn /opt/kalyn/
popd >/dev/null
rm -rf kalyn

# LOLCODE
cd /tmp
git clone https://github.com/justinmeza/lci.git
pushd lci >/dev/null
python3 install.py --prefix=/usr
popd >/dev/null
rm -rf lci

# Malbolge
cd /tmp
git clone https://github.com/bipinu/malbolge.git
clang malbolge/malbolge.c -o /usr/bin/malbolge
rm -rf malbolge

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
