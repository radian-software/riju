#!/usr/bin/env bash

set -euo pipefail

: ${AWS_ACCESS_KEY_ID}
: ${AWS_SECRET_ACCESS_KEY}
: ${DEPLOY_SSH_PRIVATE_KEY}
: ${DOCKER_PASSWORD}
: ${DOCKER_REPO}
: ${DOCKER_USERNAME}
: ${DOMAIN}
: ${S3_BUCKET}

make image shell I=admin CMD="tools/ci-run.bash"
