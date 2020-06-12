#!/usr/bin/env bash

set -e
set -o pipefail

tmpdir="$(mktemp -d)"
keyfile="${tmpdir}/id"

if [[ -n "$DEPLOY_KEY" ]]; then
    printf '%s\n' "$DEPLOY_KEY" | base64 -d > "$keyfile"
elif [[ -f "$HOME/.ssh/id_rsa_riju_deploy" ]]; then
    cp "$HOME/.ssh/id_rsa_riju_deploy" "$keyfile"
else
    echo 'deploy.bash: you must set $DEPLOY_KEY' >&2
    exit 1
fi

chmod go-rw "$keyfile"
ssh -o IdentitiesOnly=yes \
    -o StrictHostKeyChecking=no \
    -o UserKnownHostsFile=/dev/null \
    -o LogLevel=QUIET \
    -i "${keyfile}" deploy@138.68.247.206
