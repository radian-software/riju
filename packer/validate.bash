#!/usr/bin/env bash

set -euo pipefail

: ${DOCKER_REPO}
: ${ADMIN_PASSWORD}
: ${ADMIN_SSH_PUBLIC_KEY_FILE}
: ${DEPLOY_SSH_PUBLIC_KEY_FILE}
