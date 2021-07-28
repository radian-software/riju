#!/usr/bin/env bash

set -euo pipefail

if [[ -z "${NOHUP:-}" ]]; then
    NOHUP=1 nohup "$0" "$@" &
fi

while true; do
    sleep 60
    # https://unix.stackexchange.com/a/92579
    if ! sudo netstat -tnpa | grep 'ESTABLISHED.*sshd'; then
        sudo shutdown -h now
    fi
done
