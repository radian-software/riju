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

ssh -o IdentitiesOnly=yes -o StrictHostKeyChecking=no \
    -i "${keyfile}" deploy@209.141.40.107 /usr/bin/riju-install
