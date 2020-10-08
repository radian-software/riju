#!/usr/bin/env bash

set -euxo pipefail

if [[ -z "${DOCKER_REPO}" ]]; then
    echo "environment variable not set: DOCKER_REPO" >&2
    exit 1
fi

if [[ -z "${DOMAIN}" ]]; then
    echo "environment variable not set: DOMAIN" >&2
    exit 1
fi

if [[ -z "${DEPLOY_SSH_PRIVATE_KEY}" ]]; then
    if [[ -f "$HOME/.ssh/id_rsa_riju_deploy" ]]; then
        DEPLOY_SSH_PRIVATE_KEY="$(< "$HOME/.ssh/id_rsa_riju_deploy")"
    else
        echo "environment variable not set: DEPLOY_SSH_PRIVATE_KEY"
    fi
else
    DEPLOY_SSH_PRIVATE_KEY="$(printf '%s\n' "${DEPLOY_SSH_PRIVATE_KEY}" | base64 -d)"
fi

tag="$(date +%s%3N)-$(git branch --show-current)-$(git rev-parse @)"

if [[ -n "$(git status --porcelain)" ]]; then
    tag="${tag}-dirty"
fi

scripts/docker.bash tag riju:prod "${DOCKER_REPO}:${tag}"
scripts/docker.bash push "${DOCKER_REPO}:${tag}"

tmpdir="$(mktemp -d)"

function cleanup {
    rm -rf "${tmpdir}"
}

trap cleanup EXIT

printf '%s' "${DEPLOY_SSH_PRIVATE_KEY}" > "${tmpdir}/id"

chmod go-rw "${tmpdir}/id"
ssh -o IdentitiesOnly=yes \
    -o StrictHostKeyChecking=no \
    -o UserKnownHostsFile=/dev/null \
    -o LogLevel=QUIET \
    -i "${tmpdir}/id" "deploy@${DOMAIN}" \
    deploy "${tag}"
