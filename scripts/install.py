#!/usr/bin/env python3

import argparse
import errno
import os
import re
import signal
import subprocess
import sys
import tempfile
import time

result = subprocess.run(["pgrep", "-x", "riju-install"], stdout=subprocess.PIPE)
assert result.returncode in {0, 1}
for pid in result.stdout.decode().splitlines():
    print(f"Found existing process {pid}, trying to kill ...", file=sys.stderr)
    pid = int(pid)
    os.kill(pid, signal.SIGTERM)
    while True:
        time.sleep(0.01)
        try:
            os.kill(pid, 0)
        except OSError as e:
            if e.errno == errno.ESRCH:
                break

with tempfile.TemporaryDirectory() as tmpdir:
    os.chdir(tmpdir)
    subprocess.run(
        [
            "git",
            "clone",
            "https://github.com/raxod502/riju.git",
            "--single-branch",
            "--depth=1",
            "--no-tags",
        ],
        check=True,
    )
    os.chdir("riju")
    subprocess.run(["make", "image-prod"], check=True)
    subprocess.run(["scripts/install-scripts.bash"], check=True)
    subprocess.run(["docker", "system", "prune", "-f"], check=True)
    subprocess.run(["systemctl", "restart", "riju"], check=True)

print("==> Successfully deployed Riju! <==", file=sys.stderr)
