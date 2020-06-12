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

subprocess.run(["make", "image-prod"], check=True)
subprocess.run(["docker", "system", "prune", "-f"], check=True)
existing_containers = subprocess.run(
    ["docker", "ps", "-q"], check=True, stdout=subprocess.PIPE
).output.splitlines()
subprocess.run(["scripts/install-scripts.bash"], check=True)
if existing_containers:
    subprocess.run(["docker", "kill", *existing_containers], check=True)
subprocess.run(["systemctl", "restart", "riju"], check=True)

print("==> Successfully deployed Riju! <==", file=sys.stderr)
