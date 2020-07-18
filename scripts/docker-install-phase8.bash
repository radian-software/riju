#!/usr/bin/env bash

set -e
set -o pipefail
set -x

uid="$1"

rm -rf /tmp/hsperfdata_root

if [[ -n "$uid" ]] && (( "$uid" != 0 )); then
    useradd --uid="$uid" --password "!" --create-home --groups sudo docker
else
    useradd --password "!" --create-home --groups sudo docker
fi

tee /etc/sudoers.d/90-passwordless >/dev/null <<"EOF"
%sudo ALL=(ALL:ALL) NOPASSWD: ALL
EOF

touch /home/docker/.zshrc
chown docker:docker /home/docker/.zshrc

rm "$0"
