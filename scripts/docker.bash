#!/usr/bin/env bash

set -e
set -o pipefail

if [[ "$OSTYPE" != darwin* ]] && [[ "$EUID" != 0 ]]; then
    exec sudo -E docker "$@"
else
    exec docker "$@"
fi
