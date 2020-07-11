#!/usr/bin/env bash

set -e
set -o pipefail

mkdir -p /tmp/riju
if [[ -x system/out/riju-system-privileged ]]; then
    system/out/riju-system-privileged teardown "*" "*" || true
fi
chmod a=x,u=rwx /tmp/riju
