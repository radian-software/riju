#!/usr/bin/env bash

set -euo pipefail

if [[ -z "${DEPLOY_SSH_PRIVATE_KEY:-}" ]]; then
    : ${DEPLOY_SSH_PUBLIC_KEY_FILE}
    DEPLOY_SSH_PRIVATE_KEY="$(base64 < "${DEPLOY_SSH_PUBLIC_KEY_FILE%.pub}")"
fi

: ${DOMAIN}

if (( $# != 1 )); then
    echo "usage: deploy.bash IMAGE" >&2
    exit 1
fi

image="$1"

if [[ -z "${DEPLOY_SSH_PRIVATE_KEY:-}" ]]; then
    DEPLOY_SSH_PRIVATE_KEY="$(base64 < "${DEPLOY_SSH_PUBLIC_KEY_FILE%.pub}")"
fi

: ${DOCKER_REPO}
: ${DOMAIN}

tmpdir="$(mktemp -d)"

function cleanup {
    rm -rf "${tmpdir}"
}

trap cleanup EXIT

base64 -d <<< "${DEPLOY_SSH_PRIVATE_KEY}" > "${tmpdir}/id"
chmod go-rwx "${tmpdir}/id"

ssh -o IdentitiesOnly=yes \
    -o StrictHostKeyChecking=no \
    -o UserKnownHostsFile=/dev/null \
    -i "${tmpdir}/id" "deploy@${DOMAIN}"
