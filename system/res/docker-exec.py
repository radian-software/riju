#!/usr/bin/env python3

import argparse
import signal
import subprocess
import sys
import uuid

class Parser(argparse.ArgumentParser):
    def format_help(self):
        return """
Usage:  docker-exec.bash [OPTIONS] CONTAINER COMMAND [ARG...]

Run a command in a running container

Options:
  -i, --interactive   Keep STDIN open even if not attached
  -t, --tty           Allocate a pseudo-TTY
  -u, --user string   Username or UID (format: <name|uid>:[<group|gid>])
"""

parser = Parser()
parser.add_argument("-i", "--interactive", action="store_true")
parser.add_argument("-t", "--tty", action="store_true")
parser.add_argument("-u", "--user", type=str)
parser.add_argument("container", type=str)
parser.add_argument("arg", type=str, nargs="*")

args = parser.parse_args()

pidfiles = "/var/run/riju/pidfiles"
pidfile = pidfiles + "/" + str(uuid.uuid4()).replace("-", "")
print(pidfile)

# We have to use 'kill -9' here, otherwise runuser intercepts the
# signal and takes its sweet time cleaning up.
def cleanup(*ignored_args):
    subprocess.run([
        "docker",
        "exec",
        args.container,
        "bash",
        "-c",
        f"""
set -euo pipefail
if [[ -f '{pidfile}' ]]; then
    kill -9 -$(< '{pidfile}')
    rm -f '{pidfile}'
fi
        """
    ])

signal.signal(signal.SIGINT, cleanup)
signal.signal(signal.SIGTERM, cleanup)

exec_args = []

if args.interactive:
    exec_args.append("-i")
if args.tty:
    exec_args.append("-t")

runuser_args = []

if args.user:
    runuser_args = ["runuser", "-u", args.user, "--"]

subprocess.run([
    "docker",
    "exec",
    *exec_args,
    args.container,
    "bash",
    "-c",
    f"""
set -euo pipefail
umask 077
mkdir -p '{pidfiles}'
echo "$$" > '{pidfile}'
exec "$@"
    """,
    "--",
    *runuser_args,
    *args.arg,
])
