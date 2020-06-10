#!/usr/bin/env bash

set -e
set -o pipefail

uid="$1"

if [[ -n "$uid" ]] && (( "$uid" != 0 )); then
    useradd --uid="$uid" --create-home --groups sudo docker
    passwd -d docker
else
    useradd --create-home --groups sudo docker
    passwd -d docker
fi

touch /home/docker/.zshrc
chown docker:docker /home/docker/.zshrc

rm "$0"
