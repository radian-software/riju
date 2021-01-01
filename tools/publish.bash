#!/usr/bin/env bash

set -euo pipefail

: ${DOCKER_REPO}
: ${DOMAIN}
: ${S3_BUCKET}

if [[ -z "${DEPLOY_SSH_PRIVATE_KEY:-}" ]]; then
    DEPLOY_SSH_PRIVATE_KEY="$(base64 < "${DEPLOY_SSH_PUBLIC_KEY_FILE%.pub}")"
fi

make pull-base scripts

node tools/plan-publish.js --publish

sha="$(git describe --match=always-omit-tag --always --abbrev=40 --dirty)"

image="${DOCKER_REPO}:app-${sha}"

docker tag "${DOCKER_REPO}:app" "${image}"
docker push "${image}"

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
    -i "${tmpdir}/id" "deploy@${DOMAIN}" "${image}"
